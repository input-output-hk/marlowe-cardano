{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /withdrawals REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Withdrawals
  where

import Cardano.Api (AsType(..), deserialiseFromTextEnvelope, getTxBody, getTxId)
import qualified Cardano.Api.SerialiseTextEnvelope as Cardano
import Control.Monad (unless)
import Data.Aeson (Value(..))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page(..), WithdrawalFilter(..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadWithdrawal, loadWithdrawals, submitWithdrawal, withdraw)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError
  (ApiError(ApiError), badRequest', notFound', rangeNotSatisfiable', throwDTOError)
import qualified Language.Marlowe.Runtime.Web.Server.REST.ApiError as ApiError
import qualified Language.Marlowe.Runtime.Web.Server.REST.Transactions as Transactions
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(..), TempTxStatus(..), Withdrawn(..))
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorField(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

type WithdrawalHeaders = [WithdrawalHeader]
type PolicyIds = [PolicyId]
type Addresses = CommaList Address
type TxOutRefs = CommaList TxOutRef

compile $ SelectorSpec "withdrawals"
  [ "get" ≔ FieldSpec "get"
      [ ["start", "from"] ≔ ''TxId
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , ["role", "currencies"] ≔ ''PolicyIds
      , ["withdrawal", "headers"] ≔ ''WithdrawalHeaders
      ]
  , "post" ≔ FieldSpec "post"
      [ ["new", "withdrawal"] ≔ ''PostWithdrawalsRequest
      , ["change", "address"] ≔ ''Address
      , "addresses" ≔ ''Addresses
      , "collateral" ≔ ''TxOutRefs
      , ["post", "error"] ≔ ''String
      , ["post", "response"] ≔ ''WithdrawTxBody
      ]
  , ["get", "one"] ≔ FieldSpec ["get", "one"]
      [ ["get", "id"] ≔ ''TxId
      , ["get", "result"] ≔ ''Withdrawal
      ]
  , "put" ≔ FieldSpec "put"
      [ ["put", "id"] ≔ ''TxId
      , "body" ≔ ''Cardano.TextEnvelope
      , "error" ≔ ''String
      ]
  , "transactions" ≔ Inject ''Transactions.TransactionsSelector
  ]

server
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> ServerT WithdrawalsAPI (AppM r)
server eb = get eb
       :<|> post eb
       :<|> withdrawalServer eb

post
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r PostWithdrawalsResponse
post eb req@PostWithdrawalsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = withEvent eb Post \ev -> do
  addField ev $ NewWithdrawal req
  addField ev $ ChangeAddress changeAddressDTO
  traverse_ (addField ev . Addresses) mAddresses
  traverse_ (addField ev . Collateral) mCollateralUtxos
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  role' <- fromDTOThrow (badRequest' "Invalid role") role
  contractId' <- fromDTOThrow (badRequest' "Invalid contract ID") contractId
  withdraw MarloweV1 WalletAddresses{..} contractId' role' >>= \case
    Left err -> do
      addField ev $ PostError $ show err
      throwDTOError err
    Right txBody -> do
      let (withdrawalId', txBody') = toDTO (fromCardanoTxId $ getTxId txBody, txBody)
      let body = WithdrawTxBody withdrawalId' txBody'
      addField ev $ PostResponse body
      pure $ IncludeLink (Proxy @"withdrawal") body

get
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> [PolicyId]
  -> Maybe (Ranges '["withdrawalId"] GetWithdrawalsResponse)
  -> AppM r (PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse)
get eb roleCurrencies ranges = withEvent eb Get \ev -> do
  let
    range :: Range "withdrawalId" TxId
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @WithdrawalHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  addField ev $ RoleCurrencies roleCurrencies
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  roleCurrencies' <- traverse (\role -> maybe (throwError $ badRequest' $ "Invalid role value " <> show role) pure $ fromDTO role) roleCurrencies
  let wFilter = WithdrawalFilter $ Set.fromList roleCurrencies'
  loadWithdrawals wFilter range' >>= \case
    Nothing -> throwError $ rangeNotSatisfiable' "Initial withdrawal ID not found"
    Just Page{..} -> do
      let headers' = toWithdrawalHeader <$> toDTO items
      addField ev $ WithdrawalHeaders headers'
      let response = IncludeLink (Proxy @"withdrawal") <$> headers'
      addHeader totalCount . fmap ListObject <$> returnRange range response

toWithdrawalHeader :: Withdrawal -> WithdrawalHeader
toWithdrawalHeader Withdrawal{..} = WithdrawalHeader{..}

withdrawalServer
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> TxId
  -> ServerT WithdrawalAPI (AppM r)
withdrawalServer eb withdrawalId =
  getOne eb withdrawalId :<|> put eb withdrawalId

getOne
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> TxId
  -> AppM r Withdrawal
getOne eb withdrawalId = withEvent eb GetOne \ev -> do
  addField ev $ GetId withdrawalId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") withdrawalId
  loadWithdrawal contractId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just result -> do
      let withdrawal = either toDTO toDTO result
      addField ev $ GetResult withdrawal
      pure withdrawal

put
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> TxId
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId body = withEvent eb Put \ev -> do
  addField ev $ PutId contractId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  loadWithdrawal contractId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just (Left (TempTx _ Unsigned (Withdrawn txBody))) -> do
      textEnvelope <- fromDTOThrow (badRequest' "Invalid body value") body
      addField ev $ Body textEnvelope
      tx <- either (const $ throwError $ badRequest' "Invalid body text envelope content") pure $ deserialiseFromTextEnvelope (AsTx AsBabbage) textEnvelope
      unless (getTxBody tx == txBody) $ throwError (badRequest' "Provided transaction body differs from the original one")
      submitWithdrawal contractId' (setAncestor $ reference ev) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    Just _  -> throwError $
      ApiError.toServerError $
       ApiError "Withdrawal already submitted" "WithdrawalAlreadySubmitted" Null 409
