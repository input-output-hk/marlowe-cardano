{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /withdrawals REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Withdrawals
  where

import Cardano.Api (BabbageEra, TxBody, getTxBody, getTxId, makeSignedTransaction)
import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWitness
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..))
import Data.Foldable (for_, traverse_)
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
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(..), TempTxStatus(..), TxClientSelector, Withdrawn(..))
import Language.Marlowe.Runtime.Web.Server.Util
import Observe.Event.Backend (Event)
import Observe.Event.DSL (FieldSpec(..), SelectorField(..), SelectorSpec(..))
import Observe.Event.Explicit
  ( EventBackend
  , addField
  , hoistEventBackend
  , injectSelector
  , narrowEventBackend
  , reference
  , setAncestorEventBackend
  , withEvent
  )
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
      , ["post", "response", "txBody"] ≔ [t|WithdrawTxEnvelope CardanoTxBody|]
      , ["post", "response", "tx"] ≔ [t|WithdrawTxEnvelope CardanoTx|]
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
  , [ "run", "tx" ] ≔ Inject ''TxClientSelector
  ]

server
  :: EventBackend IO r WithdrawalsSelector
  -> ServerT WithdrawalsAPI (AppM r)
server eb = get eb
       :<|> (postCreateTxBodyResponse eb :<|> postCreateTxResponse eb)
       :<|> withdrawalServer eb

postCreateTxBody
  :: Event (AppM r) r' PostField
  -> PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r (TxBody BabbageEra)
postCreateTxBody ev req@PostWithdrawalsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
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
    Right txBody -> pure txBody

postCreateTxBodyResponse
  :: EventBackend IO r WithdrawalsSelector
  -> PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r (PostWithdrawalsResponse CardanoTxBody)
postCreateTxBodyResponse eb req changeAddressDTO mAddresses mCollateralUtxos = withEvent (hoistEventBackend liftIO eb) Post \ev -> do
  txBody <- postCreateTxBody ev req changeAddressDTO mAddresses mCollateralUtxos
  let (withdrawalId, txBody') = toDTO (fromCardanoTxId $ getTxId txBody, txBody)
  let body = WithdrawTxEnvelope withdrawalId txBody'
  addField ev $ PostResponseTxBody body
  pure $ IncludeLink (Proxy @"withdrawal") body

postCreateTxResponse
  :: EventBackend IO r WithdrawalsSelector
  -> PostWithdrawalsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r (PostWithdrawalsResponse CardanoTx)
postCreateTxResponse eb req changeAddressDTO mAddresses mCollateralUtxos = withEvent (hoistEventBackend liftIO eb) Post \ev -> do
  txBody <- postCreateTxBody ev req changeAddressDTO mAddresses mCollateralUtxos
  let tx = makeSignedTransaction [] txBody
  let (withdrawalId, tx') = toDTO (fromCardanoTxId $ getTxId txBody, tx)
  let body = WithdrawTxEnvelope withdrawalId tx'
  addField ev $ PostResponseTx body
  pure $ IncludeLink (Proxy @"withdrawal") body


get
  :: EventBackend IO r WithdrawalsSelector
  -> [PolicyId]
  -> Maybe (Ranges '["withdrawalId"] GetWithdrawalsResponse)
  -> AppM r (PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse)
get eb roleCurrencies ranges = withEvent (hoistEventBackend liftIO eb) Get \ev -> do
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
  :: EventBackend IO r WithdrawalsSelector
  -> TxId
  -> ServerT WithdrawalAPI (AppM r)
withdrawalServer eb withdrawalId =
  getOne eb withdrawalId :<|> put eb withdrawalId

getOne
  :: EventBackend IO r WithdrawalsSelector
  -> TxId
  -> AppM r Withdrawal
getOne eb withdrawalId = withEvent (hoistEventBackend liftIO eb) GetOne \ev -> do
  addField ev $ GetId withdrawalId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") withdrawalId
  loadWithdrawal contractId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just result -> do
      let withdrawal = either toDTO toDTO result
      addField ev $ GetResult withdrawal
      pure withdrawal

put
  :: EventBackend IO r WithdrawalsSelector
  -> TxId
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId body = withEvent (hoistEventBackend liftIO eb) Put \ev -> do
  addField ev $ PutId contractId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  loadWithdrawal contractId' >>= \case
    Nothing -> throwError $ notFound' "Withdrawal not found"
    Just (Left (TempTx _ Unsigned (Withdrawn txBody))) -> do
      (req :: Maybe (Either (Cardano.Tx BabbageEra) (ShelleyTxWitness BabbageEra))) <- case teType body of
        "Tx BabbageEra" -> pure $ Left <$> fromDTO body
        "ShelleyTxWitness BabbageEra" -> pure $ Right <$> fromDTO body
        _ -> throwError $ badRequest' "Unknown envelope type - allowed types are: \"Tx BabbageEra\", \"ShelleyTxWitness BabbageEra\""

      for_ (fromDTO body :: Maybe Cardano.TextEnvelope) \te ->
        addField ev $ Body te

      tx <- case req of
        Nothing -> throwError $ badRequest' "Invalid text envelope cbor value"
        Just (Left tx) -> do
          unless (getTxBody tx == txBody) $ throwError (badRequest' "Provided transaction body differs from the original one")
          pure tx
        Just (Right (ShelleyTxWitness (TxWitness wtKeys _ _ _ _))) -> do
          case makeSignedTxWithWitnessKeys txBody wtKeys of
            Just tx -> pure tx
            Nothing -> throwError $ badRequest' "Invalid witness keys"
      submitWithdrawal contractId' (narrowEventBackend (injectSelector RunTx) $ setAncestorEventBackend (reference ev) eb) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    Just _  -> throwError $
      ApiError.toServerError $
       ApiError "Withdrawal already submitted" "WithdrawalAlreadySubmitted" Null 409
