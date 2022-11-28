{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Transactions
  where

import Cardano.Api (getTxId)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsConstraintsBuildupError(..)
  , ApplyInputsError(..)
  , InputsApplied(..)
  , LoadMarloweContextError(..)
  , WalletAddresses(..)
  )
import Language.Marlowe.Runtime.Transaction.Constraints (ConstraintError(..))
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.HistoryClient (LoadContractHeadersError(..))
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, applyInputs, loadTransactions)
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
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
  ]

server
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> ServerT TransactionsAPI (AppM r)
server eb contractId = get eb contractId :<|> post eb contractId

get
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> Maybe (Ranges '["transactionId"] TxHeader)
  -> AppM r (PaginatedResponse '["transactionId"] TxHeader)
get eb contractId ranges = withEvent eb Get \ev -> do
  let
    range :: Range "transactionId" TxId
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @TxHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ GetContractId contractId
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  contractId' <- fromDTOThrow err400 contractId
  startFrom <- fromDTOThrow err416 rangeValue
  let mods = setAncestor $ reference ev
  loadTransactions mods contractId' startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Left ContractNotFound -> throwError err404
    Left InitialTransactionNotFound -> throwError err416
    Right headers -> do
      let headers' = either id id <$> toDTO headers
      addField ev $ TxHeaders headers'
      addHeader (length headers) . fmap ListObject <$> returnRange range headers'

post
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> PostTransactionsRequest
  -> Address
  -> Maybe (CommaList Address)
  -> Maybe (CommaList TxOutRef)
  -> AppM r PostTransactionsResponse
post eb contractId req@PostTransactionsRequest{..} changeAddressDTO mAddresses mCollateralUtxos = withEvent eb Post \ev -> do
  addField ev $ NewContract req
  addField ev $ ChangeAddress changeAddressDTO
  traverse_ (addField ev . Addresses) mAddresses
  traverse_ (addField ev . Collateral) mCollateralUtxos
  SomeMarloweVersion v@MarloweV1  <- fromDTOThrow err400 version
  changeAddress <- fromDTOThrow err400 changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow err400 (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow err400 (maybe [] unCommaList mCollateralUtxos)
  contractId' <- fromDTOThrow err400 contractId
  applyInputs v WalletAddresses{..} contractId' invalidBefore invalidHereafter inputs >>= \case
    Left err -> do
      addField ev $ PostError $ show err
      case err of
        ApplyInputsConstraintError (MintingUtxoNotFound _) -> throwError err500
        ApplyInputsConstraintError (RoleTokenNotFound _) -> throwError err403
        ApplyInputsConstraintError ToCardanoError -> throwError err500
        ApplyInputsConstraintError MissingMarloweInput -> throwError err500
        ApplyInputsConstraintError (PayoutInputNotFound _) -> throwError err500
        ApplyInputsConstraintError (CalculateMinUtxoFailed _) -> throwError err500
        ApplyInputsConstraintError (CoinSelectionFailed _) -> throwError err400
        ApplyInputsConstraintError (BalancingError _) -> throwError err500
        ScriptOutputNotFound -> throwError err400
        ApplyInputsLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> throwError err404
        ApplyInputsLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> throwError err400
        ApplyInputsLoadMarloweContextFailed LoadMarloweContextToCardanoError -> throwError err500
        ApplyInputsLoadMarloweContextFailed (MarloweScriptNotPublished _) -> throwError err500
        ApplyInputsLoadMarloweContextFailed (PayoutScriptNotPublished _) -> throwError err500
        ApplyInputsLoadMarloweContextFailed (InvalidScriptAddress _) -> throwError err500
        ApplyInputsLoadMarloweContextFailed (UnknownMarloweScript _) -> throwError err500
        ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed _) -> throwError err400
        ApplyInputsConstraintsBuildupFailed UnableToDetermineTransactionTimeout -> throwError err400
        SlotConversionFailed _ -> throwError err400
    Right InputsApplied{txBody} -> do
      let txBody' = toDTO txBody
      let txId = toDTO $ fromCardanoTxId $ getTxId txBody
      let body = ApplyInputsTxBody contractId txId txBody'
      addField ev $ PostResponse body
      pure body
