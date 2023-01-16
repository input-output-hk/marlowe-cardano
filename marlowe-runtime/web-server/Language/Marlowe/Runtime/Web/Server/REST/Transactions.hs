{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Transactions
  where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , HasTextEnvelope
  , HasTypeProxy(proxyToAsType)
  , ScriptValidity(ScriptInvalid, ScriptValid)
  , SerialiseAsCBOR(serialiseToCBOR)
  , TxScriptValidity(TxScriptValidity, TxScriptValidityNone)
  , deserialiseFromTextEnvelope
  , getTxId
  , makeSignedTransaction
  )
import qualified Cardano.Api.SerialiseTextEnvelope as Cardano
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsConstraintsBuildupError(..)
  , ApplyInputsError(..)
  , ConstraintError(..)
  , InputsApplied(..)
  , LoadMarloweContextError(..)
  , WalletAddresses(..)
  )
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.HistoryClient (LoadTxError(..))
import Language.Marlowe.Runtime.Web.Server.Monad
  (AppM, applyInputs, loadTransaction, loadTransactions, submitTransaction)
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(TempTx), TempTxStatus(..))
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

-- import Cardano.Ledger.Alonzo.TxWitness (TxWitness(..))
import qualified Cardano.Binary as CBOR
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
-- import qualified Cardano.Api.SerialiseTextEnvelope as Cardano
-- import Control.Monad.Except (MonadError)
-- import qualified Data.ByteString.Lazy as BL
-- import Data.Foldable (traverse_)
-- import Data.Maybe (fromMaybe)
-- import qualified Data.Set as Set
-- import Data.Text.Lazy (pack)
-- import Data.Text.Lazy.Encoding (encodeUtf8)
-- import Language.Marlowe.Runtime.ChainSync.Api (Lovelace(..))
-- import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
-- import Language.Marlowe.Runtime.Transaction.Api
--   ( ConstraintError(..)
--   , ContractCreated(..)
--   , CreateBuildupError(..)
--   , CreateError(..)
--   , LoadMarloweContextError(..)
--   , WalletAddresses(..)
--   )
-- import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
-- import Language.Marlowe.Runtime.Web hiding (Unsigned)
-- import Language.Marlowe.Runtime.Web.Server.DTO
-- import Language.Marlowe.Runtime.Web.Server.Monad
--   (AppM, createContract, loadContract, loadContractHeaders, submitContract)
-- import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(TempTx), TempTxStatus(Unsigned))
-- import Observe.Event (EventBackend, addField, reference, withEvent)
-- import Observe.Event.Backend (narrowEventBackend)
-- import Observe.Event.BackendModification (setAncestor)
-- import Observe.Event.DSL (FieldSpec(..), SelectorField(Inject), SelectorSpec(..))
-- import Observe.Event.Render.JSON.DSL.Compile (compile)
-- import Observe.Event.Syntax ((≔))
-- import Servant
-- import Servant.Pagination
-- import qualified Cardano.Api as C
import Cardano.Api.Shelley (ShelleyLedgerEra, TxBody(ShelleyTxBody))
import qualified Cardano.Api.Shelley as Shelley
import qualified Cardano.Ledger.Alonzo.Tx as Allonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Data.ByteString.Lazy as LBS
--
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo


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
  ]

server
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> ServerT TransactionsAPI (AppM r)
server eb contractId = get eb contractId
                  :<|> post eb contractId
                  :<|> transactionServer eb contractId

get
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> Maybe (Ranges '["transactionId"] GetTransactionsResponse)
  -> AppM r (PaginatedResponse '["transactionId"] GetTransactionsResponse)
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
    Left TxNotFound -> throwError err416
    Right headers -> do
      let headers' = either toTxHeader id <$> toDTO headers
      addField ev $ TxHeaders headers'
      addHeader (length headers) . fmap ListObject <$> returnRange range (IncludeLink (Proxy @"transaction") <$> headers')

toTxHeader :: Tx -> TxHeader
toTxHeader Tx{..} = TxHeader
  { contractId
  , transactionId
  , metadata
  , status
  , block
  , utxo = outputUtxo
  }

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
  metadata' <- fromDTOThrow err400 metadata
  applyInputs v WalletAddresses{..} contractId' metadata' invalidBefore invalidHereafter inputs >>= \case
    Left err -> do
      let
        err400' = err400 { errBody=TLE.encodeUtf8 . TL.pack $ show err}
        err403' = err403 { errBody=TLE.encodeUtf8 . TL.pack $ show err}
        err500' = err500 { errBody=TLE.encodeUtf8 . TL.pack $ show err}
      addField ev $ PostError $ show err
      case err of
        ApplyInputsConstraintError (MintingUtxoNotFound _) -> throwError err500'
        ApplyInputsConstraintError (RoleTokenNotFound _) -> throwError err403'
        ApplyInputsConstraintError ToCardanoError -> throwError err500'
        ApplyInputsConstraintError MissingMarloweInput -> throwError err500'
        ApplyInputsConstraintError (PayoutInputNotFound _) -> throwError err500'
        ApplyInputsConstraintError (CalculateMinUtxoFailed _) -> throwError err500'
        ApplyInputsConstraintError (CoinSelectionFailed _) -> throwError err400'
        ApplyInputsConstraintError (BalancingError _) -> throwError err500'
        ScriptOutputNotFound -> throwError err400'
        ApplyInputsLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> throwError err404
        ApplyInputsLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> throwError err400'
        ApplyInputsLoadMarloweContextFailed (HandshakeFailed _) -> throwError err500'
        ApplyInputsLoadMarloweContextFailed LoadMarloweContextToCardanoError -> throwError err500'
        ApplyInputsLoadMarloweContextFailed (MarloweScriptNotPublished _) -> throwError err500'
        ApplyInputsLoadMarloweContextFailed (PayoutScriptNotPublished _) -> throwError err500'
        ApplyInputsLoadMarloweContextFailed (ExtractCreationError _) -> throwError err500'
        ApplyInputsLoadMarloweContextFailed (ExtractMarloweTransactionError _) -> throwError err500'
        ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed _) -> throwError err400'
        ApplyInputsConstraintsBuildupFailed UnableToDetermineTransactionTimeout -> throwError err400'
        SlotConversionFailed _ -> throwError err400'
        TipAtGenesis -> throwError err500'
        ValidityLowerBoundTooHigh _ _ -> throwError err400'
    Right InputsApplied{txBody} -> do
      let txBody' = toDTO txBody
      let txId = toDTO $ fromCardanoTxId $ getTxId txBody
      let body = ApplyInputsTxBody contractId txId txBody'
      addField ev $ PostResponse body
      pure $ IncludeLink (Proxy @"transaction") body

transactionServer
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> TxId
  -> ServerT TransactionAPI (AppM r)
transactionServer eb contractId txId = getOne eb contractId txId
                                  :<|> put eb contractId txId

getOne
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> TxId
  -> AppM r GetTransactionResponse
getOne eb contractId txId = withEvent eb GetOne \ev -> do
  addField ev $ GetOneContractId contractId
  addField ev $ GetTxId txId
  contractId' <- fromDTOThrow err400 contractId
  txId' <- fromDTOThrow err400 txId
  loadTransaction (setAncestor $ reference ev) contractId' txId' >>= \case
    Nothing -> throwError err404
    Just result -> do
      let contractState = either toDTO toDTO result
      addField ev $ GetResult contractState
      pure
        $ IncludeLink (Proxy @"previous")
        $ IncludeLink (Proxy @"next") contractState


newtype WitnessesBabbage = WitnessesBabbage (Alonzo.TxWitness (ShelleyLedgerEra BabbageEra))

instance HasTypeProxy WitnessesBabbage where
    data AsType _ = AsWitnessesBabbage
    proxyToAsType _ = AsWitnessesBabbage

instance SerialiseAsCBOR WitnessesBabbage where
  serialiseToCBOR (WitnessesBabbage wit) = CBOR.serialize' wit

  deserialiseFromCBOR _ bs = do
    w <- CBOR.decodeAnnotator "witnessSet" CBOR.fromCBOR (LBS.fromStrict bs)
    pure $ WitnessesBabbage w


instance HasTextEnvelope WitnessesBabbage where
  textEnvelopeType _ = "WitnessesBabbage"


put
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> TxId
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId txId body = withEvent eb Put \ev -> do
  addField ev $ PutContractId contractId
  addField ev $ PutTxId txId
  contractId' <- fromDTOThrow err400 contractId
  txId' <- fromDTOThrow err400 txId
  loadTransaction (setAncestor $ reference ev) contractId' txId' >>= \case
    Nothing -> throwError err404
    Just (Left (TempTx _ Unsigned Tx.InputsApplied{txBody})) -> do
      textEnvelope <- fromDTOThrow err400 body
      addField ev $ Body textEnvelope
      -- tx <- either (const $ throwError err400) pure $ deserialiseFromTextEnvelope (AsTx AsBabbage) textEnvelope
      -- unless (getTxBody tx == txBody) $ throwError err400



      WitnessesBabbage wt <- either (const $ throwError $ err400 { errBody = "Witness decoding failed" }) pure $ deserialiseFromTextEnvelope AsWitnessesBabbage textEnvelope

      let
        txScriptValidityToScriptValidity :: TxScriptValidity era -> ScriptValidity
        txScriptValidityToScriptValidity TxScriptValidityNone = ScriptValid
        txScriptValidityToScriptValidity (TxScriptValidity _ scriptValidity) = scriptValidity

        scriptValidityToIsValid :: ScriptValidity -> Alonzo.IsValid
        scriptValidityToIsValid ScriptInvalid = Alonzo.IsValid False
        scriptValidityToIsValid ScriptValid = Alonzo.IsValid True

        txScriptValidityToIsValid :: TxScriptValidity era -> Alonzo.IsValid
        txScriptValidityToIsValid = scriptValidityToIsValid . txScriptValidityToScriptValidity

        -- tx = makeSignedTransaction txBody []
        -- renderDoc = renderStrict . PP.layoutPretty PP.defaultLayoutOptions
        -- wtRepr = renderDoc $ ppTxWitness wt
        -- case  of
        --   ShelleyTx _ (Alonzo.ValidatedTx _ wt' _ _)  -> addField ev $ WitnessesBackend $ renderDoc (ppTxWitness wt')

        tx = case (txBody, wt, makeSignedTransaction [] txBody) of
          (ShelleyTxBody era txBody' _ _ txmetadata scriptValidity, Alonzo.TxWitness wtKeys _ _ _ _, Shelley.ShelleyTx _ (Alonzo.ValidatedTx _ bkTxWitness _ _)) -> do
            let
              Alonzo.TxWitness _ bkBoot bkScripts bkDats bkRdmrs = bkTxWitness
              wt' =
                Alonzo.TxWitness
                  wtKeys
                  bkBoot
                  bkScripts
                  bkDats
                  bkRdmrs

            Shelley.ShelleyTx era $ Allonzo.ValidatedTx
              txBody'
              wt'
              (txScriptValidityToIsValid scriptValidity)
              (maybeToStrictMaybe txmetadata)

      -- addField ev $ WitnessesWallet wtRepr


      submitTransaction contractId' txId' (setAncestor $ reference ev) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError err403
    Just _  -> throwError err409


