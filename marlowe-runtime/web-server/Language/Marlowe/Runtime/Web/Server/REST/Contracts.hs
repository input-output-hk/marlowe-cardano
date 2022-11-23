{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts
  where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), CreateBuildupError(..), CreateError(..), LoadMarloweContextError(..), WalletAddresses(..))
import Language.Marlowe.Runtime.Transaction.Constraints (ConstraintError(..))
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, createContract, loadContract, loadContractHeaders)
import qualified Language.Marlowe.Runtime.Web.Server.REST.Transactions as Transactions
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(Created))
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
  SomeMarloweVersion v@MarloweV1  <- fromDTOThrow err400 version
  changeAddress <- fromDTOThrow err400 changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow err400 (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow err400 (maybe [] unCommaList mCollateralUtxos)
  roles' <- fromDTOThrow err400 roles
  metadata' <- fromDTOThrow err400 metadata
  createContract Nothing v WalletAddresses{..} roles' metadata' (Lovelace minUTxODeposit) contract >>= \case
    Left err -> do
      addField ev $ PostError $ show err
      case err of
        CreateConstraintError (MintingUtxoNotFound _) -> throwError err500
        CreateConstraintError (RoleTokenNotFound _) -> throwError err403
        CreateConstraintError ToCardanoError -> throwError err500
        CreateConstraintError MissingMarloweInput -> throwError err500
        CreateConstraintError (PayoutInputNotFound _) -> throwError err500
        CreateConstraintError (CalculateMinUtxoFailed _) -> throwError err500
        CreateConstraintError (CoinSelectionFailed _) -> throwError err400
        CreateConstraintError (BalancingError _) -> throwError err500
        CreateLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> throwError err404
        CreateLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> throwError err400
        CreateLoadMarloweContextFailed LoadMarloweContextToCardanoError -> throwError err500
        CreateLoadMarloweContextFailed (MarloweScriptNotPublished _) -> throwError err500
        CreateLoadMarloweContextFailed (PayoutScriptNotPublished _) -> throwError err500
        CreateLoadMarloweContextFailed (InvalidScriptAddress _) -> throwError err500
        CreateLoadMarloweContextFailed (UnknownMarloweScript _) -> throwError err500
        CreateBuildupFailed MintingUtxoSelectionFailed -> throwError err400
        CreateBuildupFailed (AddressDecodingFailed _) -> throwError err500
        CreateBuildupFailed (MintingScriptDecodingFailed _) -> throwError err500
        CreateToCardanoError -> throwError err400
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
  startFrom <- fromDTOThrow err416 rangeValue
  loadContractHeaders startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Nothing -> throwError err416
    Just headers -> do
      let headers' = either (toContractHeader . tempTxToDTO) toDTO <$> headers
      addField ev $ ContractHeaders headers'
      let response = IncludeLink (Proxy @"contract") <$> headers'
      addHeader (length headers) <$> returnRange range response

tempTxToDTO :: TempTx ContractCreated -> ContractState
tempTxToDTO (Created a) = toDTO a

toContractHeader :: ContractState -> ContractHeader
toContractHeader ContractState{..} = ContractHeader{..}

contractServer
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> ServerT ContractAPI (AppM r)
contractServer eb contractId = getOne eb contractId
                          :<|> Transactions.server (narrowEventBackend Transactions eb) contractId

getOne
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> AppM r GetContractResponse
getOne eb contractId = withEvent eb GetOne \ev -> do
  addField ev $ GetId contractId
  contractId' <- fromDTOThrow err400 contractId
  loadContract (setAncestor $ reference ev) contractId' >>= \case
    Nothing -> throwError err404
    Just result -> do
      let contractState = either tempTxToDTO toDTO result
      addField ev $ GetResult contractState
      pure case result of
        Left _ -> OmitLink contractState
        Right _ -> IncludeLink (Proxy @"transactions") contractState
