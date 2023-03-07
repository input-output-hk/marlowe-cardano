{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Transactions
  where

import Cardano.Api (AsType(..), deserialiseFromTextEnvelope, getTxBody, getTxId)
import qualified Cardano.Api.SerialiseTextEnvelope as Cardano
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(Null))
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page(..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api (InputsApplied(..), WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad
  (AppM, applyInputs, loadTransaction, loadTransactions, submitTransaction)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError
  (ApiError(ApiError), badRequest', notFound', rangeNotSatisfiable', throwDTOError)
import qualified Language.Marlowe.Runtime.Web.Server.REST.ApiError as ApiError
import Language.Marlowe.Runtime.Web.Server.SyncClient (LoadTxError(..))
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(TempTx), TempTxStatus(..), TxClientSelector)
import Observe.Event.DSL (FieldSpec(..), SelectorField(Inject), SelectorSpec(..))
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

type TxHeaders = [TxHeader]
type Addresses = CommaList Address
type TxOutRefs = CommaList TxOutRef

compile $ SelectorSpec "transactions"
  [ "get" ≔ FieldSpec "get"
      [ ["get", "contract", "id"] ≔ ''TxOutRef
      , ["start", "from"] ≔ ''TxId
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , ["tx", "headers"] ≔ ''TxHeaders
      ]
  , "post" ≔ FieldSpec "post"
      [ ["new", "contract"] ≔ ''PostTransactionsRequest
      , ["change", "address"] ≔ ''Address
      , "addresses" ≔ ''Addresses
      , "collateral" ≔ ''TxOutRefs
      , ["post", "error"] ≔ ''String
      , ["post", "response"] ≔ ''ApplyInputsTxBody
      ]
  , ["get", "one"] ≔ FieldSpec ["get", "one"]
      [ ["get", "one", "contract", "id"] ≔ ''TxOutRef
      , ["get", "tx", "id"] ≔ ''TxId
      , ["get", "result"] ≔ ''Tx
      ]
  , "put" ≔ FieldSpec "put"
      [ ["put", "contract", "id"] ≔ ''TxOutRef
      , ["put", "tx", "id"] ≔ ''TxId
      , "body" ≔ ''Cardano.TextEnvelope
      , "error" ≔ ''String
      ]
  , ["run", "tx" ] ≔ Inject ''TxClientSelector
  ]

server
  :: EventBackend IO r TransactionsSelector
  -> TxOutRef
  -> ServerT TransactionsAPI (AppM r)
server eb contractId = get eb contractId
                  :<|> post eb contractId
                  :<|> transactionServer eb contractId

get
  :: EventBackend IO r TransactionsSelector
  -> TxOutRef
  -> Maybe (Ranges '["transactionId"] GetTransactionsResponse)
  -> AppM r (PaginatedResponse '["transactionId"] GetTransactionsResponse)
get eb contractId ranges = withEvent (hoistEventBackend liftIO eb) Get \ev -> do
  let
    range :: Range "transactionId" TxId
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @TxHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ GetContractId contractId
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  loadTransactions contractId' range' >>= \case
    Left ContractNotFound -> throwError $ notFound' "Contract not found"
    Left TxNotFound -> throwError $ rangeNotSatisfiable' "Transcations not found"
    Right Page{..} -> do
      let headers' = toDTO items
      addField ev $ TxHeaders headers'
      addHeader totalCount . fmap ListObject <$> returnRange range (IncludeLink (Proxy @"transaction") <$> headers')

post
  :: EventBackend IO r TransactionsSelector
  -> TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r PostTransactionsResponse
post eb contractId req@PostTransactionsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = withEvent (hoistEventBackend liftIO eb) Post \ev -> do
  addField ev $ NewContract req
  addField ev $ ChangeAddress changeAddressDTO
  traverse_ (addField ev . Addresses) mAddresses
  traverse_ (addField ev . Collateral) mCollateralUtxos
  SomeMarloweVersion v@MarloweV1  <- fromDTOThrow (badRequest' "Invalid Marlowe version") version
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address") changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  metadata' <- fromDTOThrow (badRequest' "Invalid metadata value") metadata
  applyInputs v WalletAddresses{..} contractId' metadata' invalidBefore invalidHereafter inputs >>= \case
    Left err -> do
      addField ev $ PostError $ show err
      throwDTOError err
    Right InputsApplied{txBody} -> do
      let txBody' = toDTO txBody
      let txId = toDTO $ fromCardanoTxId $ getTxId txBody
      let body = ApplyInputsTxBody contractId txId txBody'
      addField ev $ PostResponse body
      pure $ IncludeLink (Proxy @"transaction") body

transactionServer
  :: EventBackend IO r TransactionsSelector
  -> TxOutRef
  -> TxId
  -> ServerT TransactionAPI (AppM r)
transactionServer eb contractId txId = getOne eb contractId txId
                                  :<|> put eb contractId txId

getOne
  :: EventBackend IO r TransactionsSelector
  -> TxOutRef
  -> TxId
  -> AppM r GetTransactionResponse
getOne eb contractId txId = withEvent (hoistEventBackend liftIO eb) GetOne \ev -> do
  addField ev $ GetOneContractId contractId
  addField ev $ GetTxId txId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- fromDTOThrow (badRequest' "Invalid transaction id value") txId
  loadTransaction contractId' txId' >>= \case
    Nothing -> throwError $ notFound' "Transaction not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      addField ev $ GetResult contractState
      pure
        $ IncludeLink (Proxy @"previous")
        $ IncludeLink (Proxy @"next") contractState

put
  :: EventBackend IO r TransactionsSelector
  -> TxOutRef
  -> TxId
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId txId body = withEvent (hoistEventBackend liftIO eb) Put \ev -> do
  addField ev $ PutContractId contractId
  addField ev $ PutTxId txId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  txId' <- fromDTOThrow (badRequest' "Invalid transaction id value") txId
  loadTransaction contractId' txId' >>= \case
    Nothing -> throwError $ notFound' "Transaction not found"
    Just (Left (TempTx _ Unsigned Tx.InputsApplied{txBody})) -> do
      textEnvelope <- fromDTOThrow (badRequest' "Invalid body value") body
      addField ev $ Body textEnvelope
      tx <- either (const $ throwError $ badRequest' "Invalid body text envelope content") pure $ deserialiseFromTextEnvelope (AsTx AsBabbage) textEnvelope
      unless (getTxBody tx == txBody) $ throwError (badRequest' "Provided transaction body differs from the original one")
      submitTransaction contractId' txId' (narrowEventBackend (injectSelector RunTx) $ setAncestorEventBackend (reference ev) eb) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    Just _  -> throwError $
      ApiError.toServerError $
      ApiError "Transaction already submitted" "ContractAlreadySubmitted" Null 409
