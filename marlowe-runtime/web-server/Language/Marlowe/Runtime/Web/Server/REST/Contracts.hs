{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts
  where

import Cardano.Api (AsType(..), deserialiseFromTextEnvelope, getTxBody)
import qualified Cardano.Api.SerialiseTextEnvelope as Cardano
import Control.Monad (unless)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (Page(..))
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated(..), WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad
  (AppM, createContract, loadContract, loadContractHeaders, submitContract)
import Language.Marlowe.Runtime.Web.Server.REST.Error (badRequest, throwDTOError)
import qualified Language.Marlowe.Runtime.Web.Server.REST.Transactions as Transactions
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(TempTx), TempTxStatus(Unsigned))
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.Backend (narrowEventBackend)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorField(Inject), SelectorSpec(..))
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
      , ["post", "response"] ≔ ''CreateTxBody
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
  ]

server
  :: EventBackend (AppM r) r ContractsSelector
  -> ServerT ContractsAPI (AppM r)
server eb = get eb
       :<|> post eb
       :<|> contractServer eb

post
  :: EventBackend (AppM r) r ContractsSelector
  -> PostContractsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r PostContractsResponse
post eb req@PostContractsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = withEvent eb Post \ev -> do
  addField ev $ NewContract req
  addField ev $ ChangeAddress changeAddressDTO
  traverse_ (addField ev . Addresses) mAddresses
  traverse_ (addField ev . Collateral) mCollateralUtxos
  SomeMarloweVersion v@MarloweV1  <- fromDTOThrow (badRequest "Unsupported Marlowe version") version
  changeAddress <- fromDTOThrow (badRequest "change address format") changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow (badRequest "addresses header value") (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow (badRequest "collateral header UTxO value") (maybe [] unCommaList mCollateralUtxos)
  roles' <- fromDTOThrow (badRequest "roles value") roles
  metadata' <- fromDTOThrow (badRequest "metadata value") metadata
  createContract Nothing v WalletAddresses{..} roles' metadata' (Lovelace minUTxODeposit) contract >>= \case
    Left err -> do
      addField ev $ PostError $ show err
      throwDTOError err
    Right ContractCreated{contractId, txBody} -> do
      let (contractId', txBody') = toDTO (contractId, txBody)
      let body = CreateTxBody contractId' txBody'
      addField ev $ PostResponse body
      pure $ IncludeLink (Proxy @"contract") body

get
  :: EventBackend (AppM r) r ContractsSelector
  -> Maybe (Ranges '["contractId"] GetContractsResponse)
  -> AppM r (PaginatedResponse '["contractId"] GetContractsResponse)
get eb ranges = withEvent eb Get \ev -> do
  let
    range :: Range "contractId" TxOutRef
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  range' <- maybe (throwError err416) pure $ fromPaginationRange range
  loadContractHeaders range' >>= \case
    Nothing -> throwError err416 { errBody = "Initial contract ID not found" }
    Just Page{..} -> do
      let headers' = toDTO items
      addField ev $ ContractHeaders headers'
      let response = IncludeLink (Proxy @"transactions") . IncludeLink (Proxy @"contract") <$> headers'
      addHeader totalCount . fmap ListObject <$> returnRange range response

toContractHeader :: ContractState -> ContractHeader
toContractHeader ContractState{..} = ContractHeader{..}

contractServer
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> ServerT ContractAPI (AppM r)
contractServer eb contractId = getOne eb contractId
                          :<|> put eb contractId
                          :<|> Transactions.server (narrowEventBackend Transactions eb) contractId

getOne
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> AppM r GetContractResponse
getOne eb contractId = withEvent eb GetOne \ev -> do
  addField ev $ GetId contractId
  contractId' <- fromDTOThrow err400 contractId
  loadContract contractId' >>= \case
    Nothing -> throwError err404
    Just result -> do
      let contractState = either toDTO toDTO result
      addField ev $ GetResult contractState
      pure $ IncludeLink (Proxy @"transactions") contractState

put
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId body = withEvent eb Put \ev -> do
  addField ev $ PutId contractId
  contractId' <- fromDTOThrow err400 contractId
  loadContract contractId' >>= \case
    Nothing -> throwError err404
    Just (Left (TempTx _ Unsigned Tx.ContractCreated{txBody})) -> do
      textEnvelope <- fromDTOThrow err400 body
      addField ev $ Body textEnvelope
      tx <- either (const $ throwError err400) pure $ deserialiseFromTextEnvelope (AsTx AsBabbage) textEnvelope
      unless (getTxBody tx == txBody) $ throwError err400
      submitContract contractId' (setAncestor $ reference ev) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError err403
    Just _  -> throwError err409
