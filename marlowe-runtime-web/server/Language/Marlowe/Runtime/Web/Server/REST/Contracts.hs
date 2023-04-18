{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts
  where

import Cardano.Api (BabbageEra, TxBody, getTxBody, makeSignedTransaction)
import qualified Cardano.Api as Cardano
import Cardano.Ledger.Alonzo.TxWitness (TxWitness(TxWitness))
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(Null))
import Data.Foldable (for_, traverse_)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Language.Marlowe.Protocol.Query.Types (ContractFilter(..), Page(..))
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace(..))
import Language.Marlowe.Runtime.Core.Api
  (ContractId, MarloweMetadataTag(..), MarloweTransactionMetadata(..), MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated(..), WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad
  (AppM, createContract, loadContract, loadContractHeaders, submitContract)
import Language.Marlowe.Runtime.Web.Server.REST.ApiError
  (ApiError(ApiError), badRequest', notFound', rangeNotSatisfiable', throwDTOError)
import qualified Language.Marlowe.Runtime.Web.Server.REST.ApiError as ApiError
import qualified Language.Marlowe.Runtime.Web.Server.REST.Transactions as Transactions
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(TempTx), TempTxStatus(Unsigned), TxClientSelector)
import Language.Marlowe.Runtime.Web.Server.Util (makeSignedTxWithWitnessKeys)
import Observe.Event.Backend (narrowEventBackend)
import Observe.Event.DSL (FieldSpec(..), SelectorField(Inject), SelectorSpec(..))
import Observe.Event.Explicit
  (Event, EventBackend, addField, hoistEventBackend, injectSelector, reference, setAncestorEventBackend, withEvent)
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

type ContractHeaders = [ContractHeader]
type Addresses = CommaList Address
type TxOutRefs = CommaList TxOutRef

compile $ SelectorSpec "contracts"
  [ "get" ≔ FieldSpec "get"
      [ ["start", "from"] ≔ ''TxOutRef
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , ["contract", "headers"] ≔ ''ContractHeaders
      ]
  , "post" ≔ FieldSpec "post"
      [ ["new", "contract"] ≔ ''PostContractsRequest
      , ["change", "address"] ≔ ''Address
      , "addresses" ≔ ''Addresses
      , "collateral" ≔ ''TxOutRefs
      , ["post", "error"] ≔ ''String
      , ["post", "response", "txBody"] ≔ [t|CreateTxEnvelope CardanoTxBody|]
      , ["post", "response", "tx"] ≔ [t|CreateTxEnvelope CardanoTx|]
      ]
  , ["get", "one"] ≔ FieldSpec ["get", "one"]
      [ ["get", "id"] ≔ ''TxOutRef
      , ["get", "result"] ≔ ''ContractState
      ]
  , "put" ≔ FieldSpec "put"
      [ ["put", "id"] ≔ ''TxOutRef
      , "body" ≔ ''Cardano.TextEnvelope
      , "error" ≔ ''String
      ]
  , "transactions" ≔ Inject ''Transactions.TransactionsSelector
  , [ "run", "tx" ] ≔ Inject ''TxClientSelector
  ]

server
  :: EventBackend IO r ContractsSelector
  -> ServerT ContractsAPI (AppM r)
server eb = get eb
       :<|> (postCreateTxBodyResponse eb :<|> postCreateTxResponse eb)
       :<|> contractServer eb

postCreateTxBody
  :: Event (AppM r) r1 PostField
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r (ContractId, TxBody BabbageEra)
postCreateTxBody ev req@PostContractsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = do
  addField ev $ NewContract req
  addField ev $ ChangeAddress changeAddressDTO
  traverse_ (addField ev . Addresses) mAddresses
  traverse_ (addField ev . Collateral) mCollateralUtxos
  SomeMarloweVersion v@MarloweV1  <- fromDTOThrow (badRequest' "Unsupported Marlowe version") version
  changeAddress <- fromDTOThrow (badRequest' "Invalid change address value") changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  roles' <- fromDTOThrow (badRequest' "Invalid roles value") roles
  transactionMetadata <- fromDTOThrow (badRequest' "Invalid metadata value") metadata
  marloweMetadata <- fromDTOThrow
    (badRequest' "Invalid tags value")
    if Map.null tags then Nothing else Just (tags, Nothing)
  createContract Nothing v WalletAddresses{..} roles' MarloweTransactionMetadata{..} (Lovelace minUTxODeposit) contract >>= \case
    Left err -> do
      addField ev $ PostError $ show err
      throwDTOError err
    Right ContractCreated{contractId, txBody} -> do
      pure (contractId, txBody)

postCreateTxBodyResponse
  :: EventBackend IO r ContractsSelector
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r (PostContractsResponse CardanoTxBody)
postCreateTxBodyResponse eb req changeAddressDTO mAddresses mCollateralUtxos = withEvent (hoistEventBackend liftIO eb) Post \ev -> do
  res <- postCreateTxBody ev req changeAddressDTO mAddresses mCollateralUtxos
  let (contractId', txBody') = toDTO res
  let body = CreateTxEnvelope contractId' txBody'
  addField ev $ PostResponseTxBody body
  pure $ IncludeLink (Proxy @"contract") body

postCreateTxResponse
  :: EventBackend IO r ContractsSelector
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r (PostContractsResponse CardanoTx)
postCreateTxResponse eb req changeAddressDTO mAddresses mCollateralUtxos = withEvent (hoistEventBackend liftIO eb) Post \ev -> do
  (contractId, txBody) <- postCreateTxBody ev req changeAddressDTO mAddresses mCollateralUtxos
  let tx = makeSignedTransaction [] txBody
  let (contractId', tx') = toDTO (contractId, tx)
  let body = CreateTxEnvelope contractId' tx'
  addField ev $ PostResponseTx body
  pure $ IncludeLink (Proxy @"contract") body

get
  :: EventBackend IO r ContractsSelector
  -> [PolicyId]
  -> [Text]
  -> Maybe (Ranges '["contractId"] GetContractsResponse)
  -> AppM r (PaginatedResponse '["contractId"] GetContractsResponse)
get eb roleCurrencies' tags' ranges = withEvent (hoistEventBackend liftIO eb) Get \ev -> do
  let
    range :: Range "contractId" TxOutRef
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  range' <- maybe (throwError $ rangeNotSatisfiable' "Invalid range value") pure $ fromPaginationRange range
  roleCurrencies <- Set.fromList <$> fromDTOThrow (badRequest' "Invalid role currency") roleCurrencies'
  let tags = Set.fromList $ MarloweMetadataTag <$> tags'
  loadContractHeaders ContractFilter{..} range' >>= \case
    Nothing -> throwError $ rangeNotSatisfiable' "Initial contract ID not found"
    Just Page{..} -> do
      let headers' = toDTO items
      addField ev $ ContractHeaders headers'
      let response = IncludeLink (Proxy @"transactions") . IncludeLink (Proxy @"contract") <$> headers'
      addHeader totalCount . fmap ListObject <$> returnRange range response

toContractHeader :: ContractState -> ContractHeader
toContractHeader ContractState{..} = ContractHeader{..}

contractServer
  :: EventBackend IO r ContractsSelector
  -> TxOutRef
  -> ServerT ContractAPI (AppM r)
contractServer eb contractId = getOne eb contractId
                          :<|> put eb contractId
                          :<|> Transactions.server (narrowEventBackend (injectSelector Transactions) eb) contractId

getOne
  :: EventBackend IO r ContractsSelector
  -> TxOutRef
  -> AppM r GetContractResponse
getOne eb contractId = withEvent (hoistEventBackend liftIO eb) GetOne \ev -> do
  addField ev $ GetId contractId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId
  loadContract contractId' >>= \case
    Nothing -> throwError $ notFound' "Contract not found"
    Just result -> do
      let contractState = either toDTO toDTO result
      addField ev $ GetResult contractState
      pure $ IncludeLink (Proxy @"transactions") contractState

put
  :: EventBackend IO r ContractsSelector
  -> TxOutRef
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId body = withEvent (hoistEventBackend liftIO eb) Put \ev -> do
  addField ev $ PutId contractId
  contractId' <- fromDTOThrow (badRequest' "Invalid contract id value") contractId

  loadContract contractId' >>= \case
    Nothing -> throwError $ notFound' "Contract not found"
    Just (Left (TempTx _ Unsigned Tx.ContractCreated{txBody})) -> do
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
        -- It seems that wallets provide nearly empty `TxWitness` back. Here is a quoat from `CIP-30` docs:
        -- > Only the portion of the witness set that were signed as a result of this call are returned to
        -- > encourage dApps to verify the contents returned by this endpoint while building the final transaction.
        Just (Right (ShelleyTxWitness (TxWitness wtKeys _ _ _ _))) -> do
          case makeSignedTxWithWitnessKeys txBody wtKeys of
            Just tx -> pure tx
            Nothing -> throwError $ badRequest' "Invalid witness keys"
      submitContract contractId' (narrowEventBackend (injectSelector RunTx) $ setAncestorEventBackend (reference ev) eb) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError $ ApiError.toServerError $ ApiError (show err) "SubmissionError" Null 403
    Just _  -> throwError $
      ApiError.toServerError $
      ApiError "Contract already submitted" "ContractAlreadySubmitted" Null 409
