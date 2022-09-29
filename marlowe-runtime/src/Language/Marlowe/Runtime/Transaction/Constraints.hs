{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Constraints
  where

import qualified Cardano.Api as C
import Cardano.Api.Shelley (NetworkId)
import qualified Cardano.Api.Shelley as C
import Control.Monad (forM)
import Data.Binary (Binary)
import Data.Crosswalk (Crosswalk(sequenceL))
import Data.Function (on)
import Data.List (find, nub)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Cardano.Api
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.SystemStart as C
import qualified Plutus.V2.Ledger.Api as P
import Witherable (wither)

-- | Describes a set of Marlowe-specific conditions that a transaction must satisfy.
data TxConstraints v = TxConstraints
  { marloweInputConstraints :: MarloweInputConstraints v
  , payoutInputConstraints :: Set (Core.PayoutDatum v)
  , roleTokenConstraints :: RoleTokenConstraints
  , payToAddresses :: Map Chain.Address Chain.Assets
  , payToRoles :: Map (Core.PayoutDatum v) Chain.Assets
  , marloweOutputConstraints :: MarloweOutputConstraints v
  , signatureConstraints :: Set Chain.PaymentKeyHash
  , metadataConstraints :: Map Word64 Chain.Metadata
  }

deriving instance Show (TxConstraints 'V1)
deriving instance Eq (TxConstraints 'V1)

-- | Constraints related to role tokens.
data RoleTokenConstraints
  = RoleTokenConstraintsNone
  | MintRoleTokens Chain.TxOutRef (C.ScriptWitness C.WitCtxMint C.BabbageEra) (Map Chain.AssetId Chain.Address)
  | SpendRoleTokens (Set Chain.AssetId)
  deriving (Eq, Show)

instance Semigroup RoleTokenConstraints where
  a <> RoleTokenConstraintsNone = a
  MintRoleTokens _ _ a <> MintRoleTokens witness ref b = MintRoleTokens witness ref $ a <> b
  SpendRoleTokens a <> SpendRoleTokens b = SpendRoleTokens $ a <> b
  _ <> b = b

instance Monoid RoleTokenConstraints where
  mempty = RoleTokenConstraintsNone

-- | Require the transaction to mint 1 role token with the specified assetId and
-- send it to the given address. Additionally, require that the given UTXO is
-- consumed.
--
-- Requires that:
--   1. The transaction mints one token with the given assetId.
--   2. The transaction sends one token with the given assetId to the given address.
--   3. The output in rule 2 does not contain any other tokens aside from ADA.
--   4. The transaction consumes the given TxOutRef.
mustMintRoleToken
  :: Core.IsMarloweVersion v
  => Chain.TxOutRef
  -> C.ScriptWitness C.WitCtxMint C.BabbageEra
  -> Chain.AssetId
  -> Chain.Address
  -> TxConstraints v
mustMintRoleToken txOutRef witness assetId address =
  mempty { roleTokenConstraints = MintRoleTokens txOutRef witness $ Map.singleton assetId address }

-- | Require the transaction to spend a UTXO with 1 role token of the specified
-- assetID. It also needs to send an identical output (same assets) to the
-- address that held the spent UTXO.
--
-- Requires that:
--   1. The transaction consumes a UTXO that contains the necessary role
--      token.
--   2. The transaction produces an output that is identical to the output from
--      rule 1.
mustSpendRoleToken :: Core.IsMarloweVersion v => Chain.AssetId -> TxConstraints v
mustSpendRoleToken assetId = mempty { roleTokenConstraints = SpendRoleTokens $ Set.singleton assetId }

data MarloweOutputConstraints v
  = MarloweOutputConstraintsNone
  | MarloweOutput Chain.Assets (Core.Datum v)

deriving instance Show (MarloweOutputConstraints 'V1)
deriving instance Eq (MarloweOutputConstraints 'V1)

instance Semigroup (MarloweOutputConstraints v) where
  a <> MarloweOutputConstraintsNone = a
  _ <> b = b

instance Monoid (MarloweOutputConstraints v) where
  mempty = MarloweOutputConstraintsNone

-- | Require the transaction to send the specified assets to the address.
--
-- Requires that:
--   postulate:
--     total :: Address -> TxBody era -> TxOutValue era
--     subValue :: TxOutValue era -> TxOutValue era -> TxOutValue era
--   given:
--     constraints :: TxConstraints v
--     assets :: Assets
--     address :: Address
--   define:
--     payConstraint = mustPayToAddress assets address
--     Right txBody = solveConstraints $ constraints <> payConstraint
--     Right txBody' = solveConstraints constraints
--   1. fromCardano (total address txBody `subValue` total address txBody') == assets
mustPayToAddress :: Core.IsMarloweVersion v => Chain.Assets -> Chain.Address -> TxConstraints v
mustPayToAddress assets address = mempty { payToAddresses = Map.singleton address assets }

-- | Require the transaction to send an output to the marlowe script address
-- with the given assets and the given datum.
--
-- Requires that:
--   1. The transaction sends an output with the given assets and datum to a
--      script address.
--   2. The script address in rule 1 is in the script address set for the
--      corresponding Marlowe version.
mustSendMarloweOutput :: Core.IsMarloweVersion v => Chain.Assets -> Core.Datum v -> TxConstraints v
mustSendMarloweOutput assets datum =
  mempty { marloweOutputConstraints = MarloweOutput assets datum }

-- | Require the transaction to send an output to the payout script address
-- with the given assets and the given datum.
--
-- Requires that:
--   postulate:
--     total :: PayoutDatum v -> TxBody era -> TxOutValue era
--     subValue :: TxOutValue era -> TxOutValue era -> TxOutValue era
--   given:
--     constraints :: TxConstraints v
--     assets :: Assets
--     role :: PayoutDatum v
--   define:
--     payConstraint = mustPayToRole assets role
--     Right txBody = solveConstraints $ constraints <> payConstraint
--     Right txBody' = solveConstraints constraints
--   1. fromCardano (total role txBody `subValue` total role txBody') == assets
--   2. The transaction sends an output to a script address.
--   3. The datum of the output in rule 2 is equal to the role.
--   4. The transaction consumes an output from a marlowe script address.
--   5. The script address in rule 1 is the payout script address associated
--      with the marlowe script address in rule 2.
mustPayToRole :: Core.IsMarloweVersion v => Chain.Assets -> Core.PayoutDatum v -> TxConstraints v
mustPayToRole assets datum =
  mempty { payToRoles = Map.singleton datum assets }

-- | Get the total amount required to be paid to the given address.
getTotalForAddress :: Chain.Address -> TxConstraints v -> Chain.Assets
getTotalForAddress address = fromMaybe mempty . Map.lookup address . payToAddresses

-- | Get the total amount required to be paid to the given role.
getTotalForRole
  :: forall v
   . Core.IsMarloweVersion v
  => Core.PayoutDatum v
  -> TxConstraints v
  -> Chain.Assets
getTotalForRole role = case Core.marloweVersion @v of
  Core.MarloweV1 -> fromMaybe mempty . Map.lookup role . payToRoles

data MarloweInputConstraints v
  = MarloweInputConstraintsNone
  | MarloweInput P.POSIXTime P.POSIXTime (Core.Redeemer v)

deriving instance Show (MarloweInputConstraints 'V1)
deriving instance Eq (MarloweInputConstraints 'V1)

instance Semigroup (MarloweInputConstraints v) where
  a <> MarloweInputConstraintsNone = a
  _ <> b = b

instance Monoid (MarloweInputConstraints v) where
  mempty = MarloweInputConstraintsNone

-- | Require the transaction to consume the UTXO for the current contract from
-- the Marlowe script with the given validity interval and redeemer (input).
-- Used for apply-inputs.
--
-- Requires that:
--   1. The input at the Marlowe Script Address for the contract is consumed.
--   2. The input in rule 1 includes the given redeemer.
--   3. The validity range of the transaction matches the given min and max
--      validity bounds (converted to slots).
--   4. The input in rule 1 comes from one of the marlowe script addresses
--      associated with marlowe version v.
--   5. All other inputs do not come from a script address.
--   6. If 'computeTransaction' returns 'Close' for the previous datum and the redeemer,
--      there are no outputs to any Marlowe script address.
--   7. Otherwise, there is an output to the same address as the input with the
--      correct datum and assets.
mustConsumeMarloweOutput :: Core.IsMarloweVersion v => P.POSIXTime -> P.POSIXTime -> Core.Redeemer v -> TxConstraints v
mustConsumeMarloweOutput invalidBefore invalidHereafter inputs =
  mempty { marloweInputConstraints = MarloweInput invalidBefore invalidHereafter inputs }

-- | Require the transaction to consume any input from the payout script that
-- bear the given datum.
--
-- Requires that:
--   1. At least one UTXO is consumed that bears the correct payout datum.
--   2. All such inputs that satisfy rule 1 come from the same address.
--   3. The address from rule 2 exists in the set of payout validator
--      addresses associated with marlowe version v.
--   4. For all inputs i that do not satisfy rule 1, the address of i does
--      not exist in the set of payout script addresses associated with
--      Marlowe version v.
--   5. For all inputs i, the address of i does not exist in the set of
--      Marlowe script addresses associated with Marlowe version v.
mustConsumePayouts :: Core.IsMarloweVersion v => Core.PayoutDatum v -> TxConstraints v
mustConsumePayouts payoutDatum = mempty { payoutInputConstraints = Set.singleton payoutDatum }

-- | Require the transaction to hold a signature for the given payment key
-- hash.
--
-- Requires that:
--   1. If none of the inputs are from an address withe a matching payment key,
--      then the txExtraKeyWits of the body content contain the given hash.
--   2. If theExtraKeyWits of the body content does not contain the given hash,
--      then at least one input must be from an address with a matching payment
--      key.
requiresSignature :: Core.IsMarloweVersion v => Chain.PaymentKeyHash -> TxConstraints v
requiresSignature pkh = mempty { signatureConstraints = Set.singleton pkh }

-- | Require the transaction include the given metadata.
--
-- Requires that:
--   1. The given metadata is present in the given index in the transaction.
requiresMetadata :: Core.IsMarloweVersion v => Word64 -> Chain.Metadata -> TxConstraints v
requiresMetadata i value = mempty { metadataConstraints = Map.singleton i value }

instance Core.IsMarloweVersion v => Semigroup (TxConstraints v) where
  a <> b = case Core.marloweVersion @v of
    Core.MarloweV1 -> TxConstraints
      { marloweInputConstraints = on (<>) marloweInputConstraints a b
      , payoutInputConstraints = on Set.union payoutInputConstraints a b
      , roleTokenConstraints = on (<>) roleTokenConstraints a b
      , payToAddresses = on (Map.unionWith (<>)) payToAddresses a b
      , payToRoles = on (Map.unionWith (<>)) payToRoles a b
      , marloweOutputConstraints = on (<>) marloweOutputConstraints a b
      , signatureConstraints = on (<>) signatureConstraints a b
      , metadataConstraints = on (<>) metadataConstraints a b
      }

instance Core.IsMarloweVersion v => Monoid (TxConstraints v) where
  mempty = case Core.marloweVersion @v of
    Core.MarloweV1 -> TxConstraints
      { marloweInputConstraints = mempty
      , payoutInputConstraints = mempty
      , roleTokenConstraints = mempty
      , payToAddresses = mempty
      , payToRoles = mempty
      , marloweOutputConstraints = mempty
      , signatureConstraints = mempty
      , metadataConstraints = mempty
      }

-- | Errors that can occur when trying to solve the constraints.
data ConstraintError v
  = ConstraintError
  | MintingUtxoNotFound Chain.TxOutRef
  | RoleTokenNotFound Chain.AssetId
  | ToCardanoError
  | MissingMarloweInput
  | PayoutInputNotFound (Core.PayoutDatum v)
  deriving (Generic)

deriving instance Eq (ConstraintError 'V1)
deriving instance Show (ConstraintError 'V1)
deriving instance Binary (ConstraintError 'V1)

-- | Data from a wallet needed to solve the constraints.
data WalletContext = WalletContext
  { availableUtxos :: Map Chain.TxOutRef Chain.TransactionOutput
  -- ^ The UTXO set of the wallet that can be used for coin selection and
  -- satisfying constraints.
  , collateralUtxos :: Set Chain.TxOutRef
  -- ^ The subset of keys in 'availableUtxos' that may be used for collateral.
  , changeAddress :: Chain.Address
  -- ^ The change address of the wallet.
  }

-- | Data from Marlowe Scripts needed to solve the constraints.
data MarloweContext v = MarloweContext
  { scriptOutput :: Maybe (Core.TransactionScriptOutput v)
  -- ^ The UTXO at the script address, if any.
  , payoutOutputs :: Map Chain.TxOutRef (Core.Payout v)
  -- ^ The UTXOs at the payout address.
  , marloweAddress :: Chain.Address
  , payoutAddress :: Chain.Address
  , marloweScriptUTxO :: Chain.TxOutRef
  , payoutScriptUTxO :: Chain.TxOutRef
  , marloweScriptHash :: Chain.ScriptHash
  , payoutScriptHash :: Chain.ScriptHash
  }

type SolveConstraints
   = forall v
   . Core.MarloweVersion v
  -> MarloweContext v
  -> WalletContext
  -> TxConstraints v
  -> Either (ConstraintError v) (C.TxBody C.BabbageEra)

-- | Given a set of constraints and the context of a wallet, produces a
-- balanced, unsigned transaction that satisfies the constraints.
solveConstraints
  :: NetworkId
  -> C.SystemStart
  -> C.EraHistory C.CardanoMode
  -> C.ProtocolParameters
  -> Chain.SlotConfig
  -> SolveConstraints
solveConstraints _networkId _start _history protocol slotConfig version marloweCtx walletCtx constraints = do
  _txBodyContent <- solveInitialTxBodyContent protocol slotConfig version marloweCtx walletCtx constraints
  Left ConstraintError

solveInitialTxBodyContent
  :: forall v
   . C.ProtocolParameters
  -> Chain.SlotConfig
  -> Core.MarloweVersion v
  -> MarloweContext v
  -> WalletContext
  -> TxConstraints v
  -> Either (ConstraintError v) (C.TxBodyContent C.BuildTx C.BabbageEra)
solveInitialTxBodyContent protocol slotConfig marloweVersion MarloweContext{..} WalletContext{..} TxConstraints{..} = do
  txIns <- solveTxIns
  txInsReference <- solveTxInsReference
  txOuts <- solveTxOuts
  txValidityRange <- solveTxValidityRange
  txExtraKeyWits <- solveTxExtraKeyWits
  txMintValue <- solveTxMintValue
  pure C.TxBodyContent
    { txIns
    , txInsCollateral = C.TxInsCollateralNone
    , txInsReference
    , txOuts
    , txTotalCollateral = C.TxTotalCollateralNone
    , txReturnCollateral = C.TxReturnCollateralNone
    , txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra 0
    , txValidityRange
    , txMetadata
    , txAuxScripts = C.TxAuxScriptsNone
    , txExtraKeyWits
    , txProtocolParams = C.BuildTxWith $ Just protocol
    , txWithdrawals = C.TxWithdrawalsNone
    , txCertificates = C.TxCertificatesNone
    , txUpdateProposal = C.TxUpdateProposalNone
    , txMintValue
    , txScriptValidity = C.TxScriptValidityNone
    }
  where
    getWalletInputs :: Either (ConstraintError v) [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
    getWalletInputs = case roleTokenConstraints of
      RoleTokenConstraintsNone -> pure []
      MintRoleTokens txOutRef _ _ -> do
        txIn <- note ToCardanoError $ toCardanoTxIn txOutRef
        _ <- note (MintingUtxoNotFound txOutRef) $ Map.lookup txOutRef availableUtxos
        pure [(txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
      SpendRoleTokens roleTokens -> do
        let availTuples = Map.toList availableUtxos
        txIns <- nub <$> forM (Set.toList roleTokens) \token -> do
          -- Find an element from availTuples where 'token' is in the assets.
          let
            containsToken :: Chain.TransactionOutput -> Bool
            containsToken = Map.member token . Chain.unTokens . Chain.tokens . Chain.assets
          (txOutRef, _) <- note (RoleTokenNotFound token) $ find (containsToken . snd) availTuples
          note ToCardanoError $ toCardanoTxIn txOutRef
        pure $ (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) <$> txIns

    getMarloweInput :: Either (ConstraintError v) (Maybe (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra)))
    getMarloweInput = case marloweInputConstraints of
      MarloweInputConstraintsNone -> pure Nothing
      MarloweInput _ _ redeemer -> fmap Just $ do
        Core.TransactionScriptOutput{..} <- note MissingMarloweInput scriptOutput
        txIn <- note ToCardanoError $ toCardanoTxIn utxo
        plutusScriptOrRefInput <- note ToCardanoError $ C.PReferenceScript
          <$> toCardanoTxIn marloweScriptUTxO
          <*> (Just <$> toCardanoScriptHash marloweScriptHash)
        let
          scriptWitness = C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            plutusScriptOrRefInput
            (C.ScriptDatumForTxIn $ toCardanoScriptData $ Core.toChainDatum marloweVersion datum)
            (toCardanoScriptData $ Chain.unRedeemer $ Core.toChainRedeemer marloweVersion redeemer)
            (C.ExecutionUnits 0 0)
        pure (txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending scriptWitness)

    getPayoutInputs :: Either (ConstraintError v) [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
    getPayoutInputs = do
      let availableUtxoList = Map.toList payoutOutputs
      foldMap NE.toList <$> forM (Set.toList payoutInputConstraints) \payoutDatum ->
        note (PayoutInputNotFound payoutDatum) . toNonEmpty
          =<< wither (maybeGetPayoutInput payoutDatum) availableUtxoList

    -- Because Data.List.NonEmpty.fromList is a partial function(!)
    toNonEmpty [] = Nothing
    toNonEmpty (x: xs) = Just $ x NE.:| xs

    maybeGetPayoutInput
      :: Core.PayoutDatum v
      -> (Chain.TxOutRef, Core.Payout v)
      -> Either
          (ConstraintError v)
          (Maybe (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra)))
    maybeGetPayoutInput payoutDatum (txOutRef, Core.Payout{..})
      | case marloweVersion of
          Core.MarloweV1 -> datum == payoutDatum = do
        txIn <- note ToCardanoError $ toCardanoTxIn txOutRef
        plutusScriptOrRefInput <- note ToCardanoError $ C.PReferenceScript
          <$> toCardanoTxIn payoutScriptUTxO
          <*> (Just <$> toCardanoScriptHash payoutScriptHash)
        let
          scriptWitness = C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            plutusScriptOrRefInput
            (C.ScriptDatumForTxIn $ toCardanoScriptData $ Core.toChainPayoutDatum marloweVersion datum)
            (C.ScriptDataConstructor 1 [])
            (C.ExecutionUnits 0 0)
        pure $ Just (txIn, C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending scriptWitness)
      | otherwise = pure Nothing

    solveTxIns = do
      walletInputs <- getWalletInputs
      marloweInputs <- maybeToList <$> getMarloweInput
      payoutInputs <- getPayoutInputs
      pure $ walletInputs <> marloweInputs <> payoutInputs

    solveTxInsReference :: Either (ConstraintError v) (C.TxInsReference C.BuildTx C.BabbageEra)
    solveTxInsReference = maybe (pure C.TxInsReferenceNone) (fmap (C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra) . sequence)
      -- sequenceL is from the 'Crosswalk' type class. It behaves similarly to
      -- 'sequenceA' except that it uses 'Align' semantics instead of
      -- 'Applicative' semantics for merging results. Here is an example of the
      -- difference this makes in practice:
      --
      -- sequenceL :: (Crosswalk t  , Align f      ) => t (f a) -> f (t a)
      -- sequenceA :: (Traversable t, Applicative m) => t (f a) -> f (t a)
      --
      -- sequenceA [Nothing, Just 2, Just 1] = Nothing
      -- sequenceA [Nothing, Nothing, Nothing] = Nothing
      -- sequenceL [Nothing, Just 2, Just 1] = Just [2, 1]
      -- sequenceL [Nothing, Nothing, Nothing] = Nothing
     $ sequenceL [marloweTxInReference, payoutTxInReference]

    -- Only include the marlowe reference script if we are consuming a marlowe
    -- input.
    marloweTxInReference :: Maybe (Either (ConstraintError v) C.TxIn)
    marloweTxInReference = case marloweInputConstraints of
      MarloweInputConstraintsNone -> Nothing
      _ -> Just $ note ToCardanoError $ toCardanoTxIn marloweScriptUTxO

    -- Only include the payout reference script if we are consuming any payout
    -- inputs.
    payoutTxInReference :: Maybe (Either (ConstraintError v) C.TxIn)
    payoutTxInReference
      | Set.null payoutInputConstraints = Nothing
      | otherwise = Just $ note ToCardanoError $ toCardanoTxIn payoutScriptUTxO

    getMarloweOutput :: Maybe Chain.TransactionOutput
    getMarloweOutput = case marloweOutputConstraints of
      MarloweOutputConstraintsNone -> Nothing
      MarloweOutput assets datum -> Just
        $ Chain.TransactionOutput marloweAddress assets Nothing
        $ Just
        $ Core.toChainDatum marloweVersion datum

    getRoleTokenOutputs :: Either (ConstraintError v) [Chain.TransactionOutput]
    getRoleTokenOutputs = case roleTokenConstraints of
      RoleTokenConstraintsNone -> pure []
      MintRoleTokens _ _ distribution ->
        pure . fmap snd . Map.toList $ flip Map.mapWithKey distribution \assetId address ->
          Chain.TransactionOutput
            address
            (Chain.Assets 0 $ Chain.Tokens $ Map.singleton assetId 1)
            Nothing
            Nothing
      SpendRoleTokens roleTokens -> do
        let availTuples = Map.toList availableUtxos
        nub <$> forM (Set.toList roleTokens) \token -> do
          -- Find an element from availTuples where 'token' is in the assets.
          let
            containsToken :: Chain.TransactionOutput -> Bool
            containsToken = Map.member token . Chain.unTokens . Chain.tokens . Chain.assets
          note (RoleTokenNotFound token) $ snd <$> find (containsToken . snd) availTuples


    getPayoutOutputs :: [Chain.TransactionOutput]
    getPayoutOutputs = uncurry getPayoutOutput <$> Map.toList payToRoles

    getPayoutOutput :: Core.PayoutDatum v -> Chain.Assets -> Chain.TransactionOutput
    getPayoutOutput payoutDatum assets = Chain.TransactionOutput payoutAddress assets Nothing $ Just datum
      where
        datum = Core.toChainPayoutDatum marloweVersion payoutDatum

    getAddressOutputs :: [Chain.TransactionOutput]
    getAddressOutputs = uncurry getAddressOutput <$> Map.toList payToAddresses

    getAddressOutput :: Chain.Address -> Chain.Assets -> Chain.TransactionOutput
    getAddressOutput address assets = Chain.TransactionOutput address assets Nothing Nothing

    solveTxOuts = note ToCardanoError . traverse (toCardanoTxOut C.MultiAssetInBabbageEra) =<< do
      roleTokenOutputs <- getRoleTokenOutputs
      pure $ concat
        [ maybeToList getMarloweOutput
        , roleTokenOutputs
        , getPayoutOutputs
        , getAddressOutputs
        ]

    solveTxValidityRange = case marloweInputConstraints of
      MarloweInputConstraintsNone ->
        pure ( C.TxValidityNoLowerBound
             , C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
             )
      MarloweInput invalidBefore invalidHereafter _ -> do
        let utcTimeToSlotNo time = C.SlotNo $ floor $ diffUTCTime time (Chain.slotZeroTime slotConfig) / Chain.slotLength slotConfig
        let posixTimeToUTCTime (P.POSIXTime t) = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromInteger t / 1000
        let minSlotNo = utcTimeToSlotNo $ posixTimeToUTCTime invalidBefore
        let maxSlotNo = utcTimeToSlotNo $ posixTimeToUTCTime invalidHereafter
        pure ( C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra minSlotNo
             , C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra maxSlotNo
             )

    txMetadata :: C.TxMetadataInEra C.BabbageEra
    txMetadata
      | Map.null metadataConstraints = C.TxMetadataNone
      | otherwise = C.TxMetadataInEra C.TxMetadataInBabbageEra $ C.TxMetadata $ toCardanoMetadata <$> metadataConstraints

    solveTxExtraKeyWits :: Either (ConstraintError v) (C.TxExtraKeyWitnesses C.BabbageEra)
    solveTxExtraKeyWits
      | Set.null signatureConstraints = pure C.TxExtraKeyWitnessesNone
      | otherwise = note ToCardanoError $ C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra
          <$> traverse toCardanoPaymentKeyHash (Set.toList signatureConstraints)

    solveTxMintValue :: Either (ConstraintError v) (C.TxMintValue C.BuildTx C.BabbageEra)
    solveTxMintValue = case roleTokenConstraints of
      MintRoleTokens _ witness distribution -> do
        let assetIds = Map.keysSet distribution
        let tokens = Map.fromSet (const 1) assetIds
        policyIds <- note ToCardanoError $ Set.fromAscList
          <$> traverse (toCardanoPolicyId . Chain.policyId) (Set.toAscList assetIds)
        value <- note ToCardanoError $ tokensToCardanoValue $ Chain.Tokens tokens
        pure
          $ C.TxMintValue C.MultiAssetInBabbageEra value
          $ C.BuildTxWith
          $ Map.fromSet (const witness) policyIds
      _ -> pure C.TxMintNone

note :: a -> Maybe b -> Either a b
note e = maybe (Left e) Right
