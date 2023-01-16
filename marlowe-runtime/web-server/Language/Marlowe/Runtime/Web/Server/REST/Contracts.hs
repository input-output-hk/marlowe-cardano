{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines a server for the /contracts REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts
  where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , HasTypeProxy(proxyToAsType)
  , ScriptValidity(ScriptInvalid, ScriptValid)
  , SerialiseAsCBOR(serialiseToCBOR)
  , TxScriptValidity(TxScriptValidity, TxScriptValidityNone)
  , deserialiseFromTextEnvelope
  , makeSignedTransaction
  )
import qualified Cardano.Api as C
import qualified Cardano.Api.SerialiseTextEnvelope as Cardano
import Cardano.Api.Shelley (ShelleyLedgerEra, Tx(ShelleyTx), TxBody(ShelleyTxBody))
import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.Tx as Allonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import Cardano.Ledger.Alonzo.TxWitness (TxWitness(..))
import Cardano.Ledger.BaseTypes (maybeToStrictMaybe)
import Cardano.Ledger.Pretty.Alonzo (ppTxWitness)
import Control.Monad.Except (MonadError)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace(..))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api
  ( ConstraintError(..)
  , ContractCreated(..)
  , CreateBuildupError(..)
  , CreateError(..)
  , LoadMarloweContextError(..)
  , WalletAddresses(..)
  )
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad
  (AppM, createContract, loadContract, loadContractHeaders, submitContract)
import qualified Language.Marlowe.Runtime.Web.Server.REST.Transactions as Transactions
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(TempTx), TempTxStatus(Unsigned))
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.Backend (narrowEventBackend)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorField(Inject), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import qualified Prettyprinter as PP
import Prettyprinter.Render.Text as PP
import Servant
import Servant.Pagination

import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo

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
      , "witnessesBackend" ≔ ''Text
      , "witnessesWallet" ≔ ''Text
      -- , "txWallet" ≔ ''Text
      -- , "txBackend" ≔ ''Text
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

fromDTOThrow' :: (MonadError ServerError m, FromDTO a) => BL.ByteString -> ServerError -> DTO a -> m a
fromDTOThrow' reason err = fromDTOThrow (err { errBody = reason })

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
  SomeMarloweVersion v@MarloweV1  <- fromDTOThrow' "Version decoding failed" err400 version
  changeAddress <- fromDTOThrow' "Change address decoding failed" err400 changeAddressDTO
  extraAddresses <- Set.fromList <$> fromDTOThrow' "Address decoding failed" err400 (maybe [] unCommaList mAddresses)
  collateralUtxos <- Set.fromList <$> fromDTOThrow' "Collateral UTxO decoding failed" err400 (maybe [] unCommaList mCollateralUtxos)
  roles' <- fromDTOThrow' "Roles decoding failed" err400 roles
  metadata' <- fromDTOThrow' "Metadata decoding failed" err400 metadata
  createContract Nothing v WalletAddresses{..} roles' metadata' (Lovelace minUTxODeposit) contract >>= \case
    Left err -> do
      let
        err400' = err400 { errBody=encodeUtf8 . pack $ show err}
        err404' = err404 { errBody=encodeUtf8 . pack $ show err}
        err403' = err403 { errBody=encodeUtf8 . pack $ show err}
        err500' = err500 { errBody=encodeUtf8 . pack $ show err}
      addField ev $ PostError $ show err
      case err of
        CreateConstraintError (MintingUtxoNotFound _) -> throwError err500'
        CreateConstraintError (RoleTokenNotFound _) -> throwError err403'
        CreateConstraintError ToCardanoError -> throwError err500'
        CreateConstraintError MissingMarloweInput -> throwError err500'
        CreateConstraintError (PayoutInputNotFound _) -> throwError err500'
        CreateConstraintError (CalculateMinUtxoFailed _) -> throwError err500'
        CreateConstraintError (CoinSelectionFailed _) -> throwError err400'
        CreateConstraintError (BalancingError _) -> throwError err500'
        CreateLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> throwError err404'
        CreateLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> throwError err400'
        CreateLoadMarloweContextFailed (HandshakeFailed _) -> throwError err500'
        CreateLoadMarloweContextFailed LoadMarloweContextToCardanoError -> throwError err500'
        CreateLoadMarloweContextFailed (MarloweScriptNotPublished _) -> throwError err500'
        CreateLoadMarloweContextFailed (PayoutScriptNotPublished _) -> throwError err500'
        CreateLoadMarloweContextFailed (ExtractCreationError _) -> throwError err500'
        CreateLoadMarloweContextFailed (ExtractMarloweTransactionError _) -> throwError err500'
        CreateBuildupFailed MintingUtxoSelectionFailed -> throwError err400'
        CreateBuildupFailed (AddressDecodingFailed _) -> throwError err500'
        CreateBuildupFailed (MintingScriptDecodingFailed _) -> throwError err500'
        CreateToCardanoError -> throwError err400'
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
      let headers' = either toContractHeader id <$> toDTO headers
      addField ev $ ContractHeaders headers'
      let response = IncludeLink (Proxy @"contract") <$> headers'
      addHeader (length headers) . fmap ListObject <$> returnRange range response

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
  loadContract (setAncestor $ reference ev) contractId' >>= \case
    Nothing -> throwError err404
    Just result -> do
      let contractState = either toDTO toDTO result
      addField ev $ GetResult contractState
      pure $ IncludeLink (Proxy @"transactions") contractState


newtype WitnessesBabbage = WitnessesBabbage (TxWitness (ShelleyLedgerEra BabbageEra))

instance HasTypeProxy WitnessesBabbage where
    data AsType _ = AsWitnessesBabbage
    proxyToAsType _ = AsWitnessesBabbage

instance SerialiseAsCBOR WitnessesBabbage where
  serialiseToCBOR (WitnessesBabbage wit) = CBOR.serialize' wit

  deserialiseFromCBOR _ bs = do
    w <- CBOR.decodeAnnotator "witnessSet" CBOR.fromCBOR (LBS.fromStrict bs)
    pure $ WitnessesBabbage w


instance C.HasTextEnvelope WitnessesBabbage where
  textEnvelopeType _ = "WitnessesBabbage"

put
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> TextEnvelope
  -> AppM r NoContent
put eb contractId body = withEvent eb Put \ev -> do
  addField ev $ PutId contractId
  contractId' <- fromDTOThrow err400 contractId
  loadContract (setAncestor $ reference ev) contractId' >>= \case
    Nothing -> throwError err404
    Just (Left (TempTx _ Unsigned Tx.ContractCreated{txBody})) -> do
      textEnvelope <- fromDTOThrow err400 body
      addField ev $ Body textEnvelope

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
        renderDoc = renderStrict . PP.layoutPretty PP.defaultLayoutOptions
        wtRepr = renderDoc $ ppTxWitness wt
        -- case  of
        --   ShelleyTx _ (Alonzo.ValidatedTx _ wt' _ _)  -> addField ev $ WitnessesBackend $ renderDoc (ppTxWitness wt')

        tx = case (txBody, wt, makeSignedTransaction [] txBody) of
          (ShelleyTxBody era txBody' _ _ txmetadata scriptValidity, TxWitness wtKeys _ _ _ _, ShelleyTx _ (Alonzo.ValidatedTx _ bkTxWitness _ _)) -> do
            let
              Alonzo.TxWitness _ bkBoot bkScripts bkDats bkRdmrs = bkTxWitness
              wt' =
                Alonzo.TxWitness
                  wtKeys
                  bkBoot
                  bkScripts
                  bkDats
                  bkRdmrs

            ShelleyTx era $ Allonzo.ValidatedTx
              txBody'
              wt'
              (txScriptValidityToIsValid scriptValidity)
              (maybeToStrictMaybe txmetadata)

      addField ev $ WitnessesWallet wtRepr


      submitContract contractId' (setAncestor $ reference ev) tx >>= \case
        Nothing -> pure NoContent
        Just err -> do
          addField ev $ Error $ show err
          throwError err403
    Just _  -> throwError err409




--       ShelleyTx era $
--         Alonzo.ValidatedTx
--           txbody'
--           (Alonzo.TxWitness
--             (Set.fromList [ w | ShelleyKeyWitness _ w <- witnesses ])
--             (Set.fromList [ w | ShelleyBootstrapWitness _ w <- witnesses ])
--             (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
--                           | sw <- txscripts ])
--             datums
--             redeemers)
--           (txScriptValidityToIsValid scriptValidity)
--           (maybeToStrictMaybe txmetadata)
--       where
--         (datums, redeemers) =
--           case txscriptdata of
--             TxBodyScriptData _ ds rs -> (ds, rs)
--             TxBodyNoScriptData       -> (mempty, Alonzo.Redeemers mempty)



-- makeSignedTransaction :: forall era.
--      [KeyWitness era]
--   -> TxBody era
--   -> Tx era
-- makeSignedTransaction witnesses (ByronTxBody txbody) =
--     ByronTx
--   . Byron.annotateTxAux
--   $ Byron.mkTxAux
--       (unAnnotated txbody)
--       (Vector.fromList [ w | ByronKeyWitness w <- witnesses ])
--
-- makeSignedTransaction witnesses (ShelleyTxBody era txbody
--                                                txscripts
--                                                txscriptdata
--                                                txmetadata
--                                                scriptValidity
--                                                ) =
--     case era of
--       ShelleyBasedEraShelley -> makeShelleySignedTransaction txbody
--       ShelleyBasedEraAllegra -> makeShelleySignedTransaction txbody
--       ShelleyBasedEraMary    -> makeShelleySignedTransaction txbody
--       ShelleyBasedEraAlonzo  -> makeAlonzoSignedTransaction  txbody
--       ShelleyBasedEraBabbage -> makeAlonzoSignedTransaction  txbody
--   where
--     makeShelleySignedTransaction
--       :: forall ledgerera.
--          ShelleyLedgerEra era ~ ledgerera
--       => ToCBOR (Ledger.AuxiliaryData ledgerera)
--       => ToCBOR (Ledger.TxBody ledgerera)
--       => ToCBOR (Ledger.Script ledgerera)
--       => FromCBOR (CBOR.Annotator (Ledger.Script ledgerera))
--       => Ledger.Crypto ledgerera ~ StandardCrypto
--       => Ledger.Witnesses ledgerera ~ Shelley.WitnessSetHKD Identity ledgerera
--       => Ledger.Tx ledgerera ~ Shelley.Tx ledgerera
--       => ToCBOR (Ledger.Witnesses ledgerera)
--       => Shelley.UsesValue ledgerera
--       => Shelley.ValidateScript ledgerera
--       => Ledger.TxBody ledgerera
--       -> Tx era
--     makeShelleySignedTransaction txbody' =
--       ShelleyTx era $
--         Shelley.Tx
--           txbody'
--           (Shelley.WitnessSet
--             (Set.fromList [ w | ShelleyKeyWitness _ w <- witnesses ])
--             (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
--                           | sw <- txscripts ])
--             (Set.fromList [ w | ShelleyBootstrapWitness _ w <- witnesses ]))
--           (maybeToStrictMaybe txmetadata)
--
--     makeAlonzoSignedTransaction
--       :: forall ledgerera.
--          ShelleyLedgerEra era ~ ledgerera
--       => Ledger.Crypto ledgerera ~ StandardCrypto
--       => Ledger.Tx ledgerera ~ Alonzo.ValidatedTx ledgerera
--       => Ledger.Script ledgerera ~ Alonzo.Script ledgerera
--       => Shelley.UsesValue ledgerera
--       => Shelley.ValidateScript ledgerera
--       => Ledger.TxBody ledgerera
--       -> Tx era
--     makeAlonzoSignedTransaction txbody' =
--       ShelleyTx era $
--         Alonzo.ValidatedTx
--           txbody'
--           (Alonzo.TxWitness
--             (Set.fromList [ w | ShelleyKeyWitness _ w <- witnesses ])
--             (Set.fromList [ w | ShelleyBootstrapWitness _ w <- witnesses ])
--             (Map.fromList [ (Ledger.hashScript @ledgerera sw, sw)
--                           | sw <- txscripts ])
--             datums
--             redeemers)
--           (txScriptValidityToIsValid scriptValidity)
--           (maybeToStrictMaybe txmetadata)
--       where
--         (datums, redeemers) =
--           case txscriptdata of
--             TxBodyScriptData _ ds rs -> (ds, rs)
--             TxBodyNoScriptData       -> (mempty, Alonzo.Redeemers mempty)
