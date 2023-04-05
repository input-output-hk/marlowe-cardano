{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Constraints
  ( ConstraintError(..)
  , MarloweContext(..)
  , MarloweInputConstraints(..)
  , MarloweOutputConstraints(..)
  , RoleTokenConstraints(..)
  , SolveConstraints
  , TxConstraints(..)
  , WalletContext(..)
  , adjustTxForMinUtxo
  , balanceTx
  , ensureMinUtxo
  , findMinUtxo
  , mustConsumeMarloweOutput
  , mustConsumePayouts
  , mustMintRoleToken
  , mustPayToAddress
  , mustPayToRole
  , mustSendMarloweOutput
  , mustSpendRoleToken
  , requiresMetadata
  , requiresSignature
  , selectCoins
  , solveConstraints
  , solveInitialTxBodyContent
  ) where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import Control.Applicative ((<|>))
import Control.Error (note)
import Control.Monad (forM, unless, when)
import Data.Aeson (ToJSON)
import Data.Crosswalk (Crosswalk(sequenceL))
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (delete, find, minimumBy, nub)
import qualified Data.List.NonEmpty as NE (NonEmpty(..), toList)
import Data.Map (Map)
import qualified Data.Map as Map (elems, fromSet, keysSet, mapWithKey, member, null, singleton, toList, unionWith)
import qualified Data.Map.Strict as SMap (fromList, toList)
import Data.Maybe (mapMaybe, maybeToList)
import Data.Monoid (First(..), getFirst)
import Data.Set (Set)
import qualified Data.Set as Set (fromAscList, member, null, singleton, toAscList, toList, union)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api
  ( fromCardanoAddressInEra
  , fromCardanoTxIn
  , toCardanoAddressInEra
  , toCardanoPaymentKeyHash
  , toCardanoPolicyId
  , toCardanoScriptData
  , toCardanoScriptHash
  , toCardanoTxIn
  , toCardanoTxOut
  , toCardanoTxOut'
  , toCardanoTxOutValue
  , tokensToCardanoValue
  )
import Language.Marlowe.Runtime.ChainSync.Api (lookupUTxO, toCardanoMetadata, toPlutusData, toUTxOTuple, toUTxOsList)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( MarloweTransactionMetadata(..)
  , MarloweVersionTag(..)
  , TransactionScriptOutput(utxo)
  , emptyMarloweTransactionMetadata
  , encodeMarloweTransactionMetadata
  )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.ScriptRegistry (ReferenceScriptUtxo(..))
import Language.Marlowe.Runtime.Transaction.Api (ConstraintError(..))
import qualified Language.Marlowe.Scripts as V1
import Ouroboros.Consensus.BlockchainTime (SystemStart)
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
  , metadataConstraints :: Core.MarloweTransactionMetadata
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
  MintRoleTokens _ _ a <> MintRoleTokens ref witness b = MintRoleTokens ref witness $ a <> b
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
mustPayToRole :: Core.IsMarloweVersion v => Chain.Assets -> Core.PayoutDatum v -> TxConstraints v
mustPayToRole assets datum =
  mempty { payToRoles = Map.singleton datum assets }

data MarloweInputConstraints v
  = MarloweInputConstraintsNone
  | MarloweInput C.SlotNo C.SlotNo (Core.Inputs v)

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
--      validity bounds.
--   4. All other inputs do not come from a script address.
mustConsumeMarloweOutput :: Core.IsMarloweVersion v => C.SlotNo -> C.SlotNo -> Core.Inputs v -> TxConstraints v
mustConsumeMarloweOutput invalidBefore invalidHereafter inputs =
  mempty { marloweInputConstraints = MarloweInput invalidBefore invalidHereafter inputs }

-- | Require the transaction to consume any input from the payout script that
-- bear the given datum.
--
-- Requires that:
--   1. At least one UTXO is consumed that bears the correct payout datum.
--   2. All such inputs that satisfy rule 1 come from the same address.
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
--   1. The given metadata map is present in the transaction.
requiresMetadata :: Core.IsMarloweVersion v => Core.MarloweTransactionMetadata -> TxConstraints v
requiresMetadata metadataConstraints = mempty { metadataConstraints }

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
      , metadataConstraints = MarloweTransactionMetadata
          { marloweMetadata = on (<|>) (marloweMetadata . metadataConstraints) a b
          , transactionMetadata = on (<>) (transactionMetadata . metadataConstraints) a b
          }
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
      , metadataConstraints = emptyMarloweTransactionMetadata
      }

-- | Data from a wallet needed to solve the constraints.
data WalletContext = WalletContext
  { availableUtxos :: Chain.UTxOs
  -- ^ The UTXO set of the wallet that can be used for coin selection and
  -- satisfying constraints.
  , collateralUtxos :: Set Chain.TxOutRef
  -- ^ The subset of keys in 'availableUtxos' that may be used for collateral.
  , changeAddress :: Chain.Address
  -- ^ The change address of the wallet.
  }
  deriving (Show, Generic, ToJSON)


-- | Data from Marlowe Scripts needed to solve the constraints.
data MarloweContext v = MarloweContext
  { scriptOutput :: Maybe (Core.TransactionScriptOutput v)
  -- ^ The UTXO at the script address, if any.
  , payoutOutputs :: Map Chain.TxOutRef (Core.Payout v)
  -- ^ The UTXOs at the payout address.
  , marloweAddress :: Chain.Address
  , payoutAddress :: Chain.Address
  , marloweScriptUTxO :: ReferenceScriptUtxo
  , payoutScriptUTxO :: ReferenceScriptUtxo
  , marloweScriptHash :: Chain.ScriptHash
  , payoutScriptHash :: Chain.ScriptHash
  } deriving Generic

deriving instance Show (MarloweContext 'V1)
deriving instance ToJSON (MarloweContext 'V1)

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
  :: SystemStart
  -> C.EraHistory C.CardanoMode
  -> C.ProtocolParameters
  -> SolveConstraints
solveConstraints start history protocol version marloweCtx walletCtx constraints =
  solveInitialTxBodyContent protocol version marloweCtx walletCtx constraints
    >>= adjustTxForMinUtxo protocol (marloweAddress marloweCtx)
    >>= selectCoins protocol version marloweCtx walletCtx
    >>= balanceTx C.BabbageEraInCardanoMode start history protocol version marloweCtx walletCtx

-- | 2022-08 This function was written to compensate for a bug in Cardano's
--   calculateMinimumUTxO. It's called by adjustOutputForMinUTxO below. We will
--   eventually be able to remove it.
ensureAtLeastHalfAnAda :: C.Value -> C.Value
ensureAtLeastHalfAnAda origValue =
  if origLovelace < minLovelace
    then origValue <> C.lovelaceToValue (minLovelace - origLovelace)
    else origValue
  where
    origLovelace = C.selectLovelace origValue
    minLovelace = C.Lovelace 500_000

-- | Compute the `minAda` and adjust the lovelace in a single output to conform
--   to the minimum ADA requirement.
adjustOutputForMinUtxo
  :: forall v
   .  C.ProtocolParameters
  -> C.TxOut C.CtxTx C.BabbageEra
  -> Either (ConstraintError v) (C.TxOut C.CtxTx C.BabbageEra)
adjustOutputForMinUtxo protocol (C.TxOut address txOrigValue datum script) = do
  let
    origValue = C.txOutValueToValue txOrigValue
    adjustedForCalculateMin = ensureAtLeastHalfAnAda origValue
    txOut' = C.TxOut address (C.TxOutValue C.MultiAssetInBabbageEra adjustedForCalculateMin) datum script
  minValue <- case C.calculateMinimumUTxO C.ShelleyBasedEraBabbage txOut' protocol of
    Right minValue' -> pure minValue'
    Left e -> Left (CalculateMinUtxoFailed $ show e)
  let
    minLovelace = C.selectLovelace minValue
    deficit = if minLovelace > C.selectLovelace origValue
      then minLovelace <> (negate $ C.selectLovelace origValue)
      else mempty
    newValue = origValue <> C.lovelaceToValue deficit
  pure $ C.TxOut address (C.TxOutValue C.MultiAssetInBabbageEra newValue) datum script

-- Adjusts all the TxOuts as necessary to comply with Minimum UTXO
-- requirements. Additionally, ensures that the Value of the marlowe output
-- does not change (fails with an error if it does).
adjustTxForMinUtxo
  :: forall v
   . C.ProtocolParameters
  -> Chain.Address
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> Either (ConstraintError v) (C.TxBodyContent C.BuildTx C.BabbageEra)
adjustTxForMinUtxo protocol marloweAddress txBodyContent = do
  let
    getMarloweOutputValue :: [C.TxOut C.CtxTx C.BabbageEra] -> Maybe (C.TxOutValue C.BabbageEra)
    getMarloweOutputValue = getFirst . mconcat . map (First
      . (\(C.TxOut addressInEra txOutValue _ _) ->
          if fromCardanoAddressInEra C.BabbageEra addressInEra == marloweAddress
            then Just txOutValue
            else Nothing))

    origTxOuts = C.txOuts txBodyContent
    origMarloweValue = getMarloweOutputValue origTxOuts

  adjustedTxOuts <- traverse (adjustOutputForMinUtxo protocol) origTxOuts

  if origMarloweValue == getMarloweOutputValue adjustedTxOuts
    then Right $ txBodyContent { C.txOuts = adjustedTxOuts }
    else Left $ CalculateMinUtxoFailed "Marlowe output value changed during output adjustment"

-- | Compute the maximum fee for any transaction.
maximumFee :: C.ProtocolParameters -> C.Lovelace
maximumFee C.ProtocolParameters{..} =
  let
    txFee :: C.Lovelace
    txFee = fromIntegral $ protocolParamTxFeeFixed + protocolParamTxFeePerByte * protocolParamMaxTxSize
    executionFee :: Rational
    executionFee =
      case (protocolParamPrices, protocolParamMaxTxExUnits) of
        (Just C.ExecutionUnitPrices{..}, Just C.ExecutionUnits{..})
          -> priceExecutionSteps  * fromIntegral executionSteps + priceExecutionMemory * fromIntegral executionMemory
        _ -> 0
  in
    txFee + round executionFee

-- | Calculate the minimum UTxO requirement for a value.
--   We must pack the address, datum and value into a dummy TxOut along with
--   the "none" reference script in order to use it with C.calculateMinimumUTxO
--   in a subsequent call
findMinUtxo
  :: forall v
   . C.ProtocolParameters
  -> (Chain.Address, Maybe Chain.Datum, C.Value)
  -> Either (ConstraintError v) C.Value
findMinUtxo protocol (chAddress, mbDatum, origValue) =
  do
    let
      atLeastHalfAnAda :: C.Value
      -- FIXME Found what looks like the same value being added and then subtracted from this computation
      -- atLeastHalfAnAda = origValue <> C.lovelaceToValue (maximum [500_000, C.selectLovelace origValue] - C.selectLovelace origValue)
      atLeastHalfAnAda = C.lovelaceToValue (maximum [500_000, C.selectLovelace origValue])
      revisedValue = origValue <> C.negateValue (C.lovelaceToValue $ C.selectLovelace origValue) <> atLeastHalfAnAda
      datum = maybe C.TxOutDatumNone
        (C.TxOutDatumInTx C.ScriptDataInBabbageEra . C.fromPlutusData . toPlutusData)
        mbDatum

    dummyTxOut <- makeTxOut chAddress datum revisedValue C.ReferenceScriptNone
    case C.calculateMinimumUTxO C.ShelleyBasedEraBabbage dummyTxOut protocol of
       Right minValue -> pure minValue
       Left e -> Left . CoinSelectionFailed $ show e

-- | Ensure that the minimum UTxO requirement is satisfied for outputs.
ensureMinUtxo
  :: forall v
   . C.ProtocolParameters
  -> (Chain.Address, C.Value)
  -> Either (ConstraintError v) (Chain.Address, C.Value)
ensureMinUtxo protocol (chAddress, origValue) =
  case findMinUtxo protocol (chAddress, Nothing, origValue) of
    Right minValue -> pure (chAddress, origValue <> (C.lovelaceToValue $ maximum [C.selectLovelace minValue, C.selectLovelace origValue] - C.selectLovelace origValue))
    Left e -> Left e

-- | Compute transaction output for building a transaction.
makeTxOut
  :: Chain.Address
  -> C.TxOutDatum C.CtxTx C.BabbageEra
  -> C.Value
  -> C.ReferenceScript C.BabbageEra
  -> Either (ConstraintError v) (C.TxOut C.CtxTx C.BabbageEra)
makeTxOut address datum value referenceScript = do
  cardanoAddress <- note
    (CalculateMinUtxoFailed $ "Unable to convert address: " <> show address)
    $ C.anyAddressInShelleyBasedEra <$> Chain.toCardanoAddressAny address
  Right $ C.TxOut
    cardanoAddress
    (C.TxOutValue C.MultiAssetInBabbageEra value)
    datum
    referenceScript

-- Test whether a value only contains lovelace.
onlyLovelace :: C.Value -> Bool
onlyLovelace value = C.lovelaceToValue (C.selectLovelace value) == value

-- Selects enough additional inputs to cover the excess balance of the
-- transaction (total outputs - total inputs).
selectCoins
  :: forall v
  .  C.ProtocolParameters
  -> Core.MarloweVersion v
  -> MarloweContext v
  -> WalletContext
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> Either (ConstraintError v) (C.TxBodyContent C.BuildTx C.BabbageEra)
selectCoins protocol marloweVersion marloweCtx walletCtx@WalletContext{..} txBodyContent = do
  let
    -- Extract the value of a UTxO
    txOutToValue :: C.TxOut C.CtxTx C.BabbageEra -> C.Value
    txOutToValue (C.TxOut _ value _ _) = C.txOutValueToValue value

    -- All utxos that are spendable from either the Marlowe context or wallet context
    -- False means not including the reference utxo
    utxos :: [(C.TxIn, C.TxOut C.CtxTx C.BabbageEra)]
    utxos = allUtxos marloweVersion marloweCtx walletCtx False

    -- Compute the value of all available UTxOs
    universe :: C.Value
    universe = foldMap (txOutToValue . snd) utxos

    -- FIXME: Use protocolParamCollateralPercent from ProtocolParameters instead of this hard-coded '2'. We will automatically pick up future chain values this way.
    fee :: C.Value
    fee = C.lovelaceToValue $ 2 * maximumFee protocol

  collateral <-
    let
      candidateCollateral =
        if Set.null collateralUtxos
          then utxos  -- Use any UTxO if the wallet did not constrain collateral.
          else -- The filter below is safe because, by the definition of `WalletContext`,
               -- the `collateralUtxos` are an improper subset of `availableUtxos`. Also
               -- note that the definition of `WalletContext` does not *require* that
               -- every UTxO specified as collateral be used: it just states that those
               -- UTxOs are *available* for use as collateral.
               filter (flip Set.member collateralUtxos . fromCardanoTxIn . fst) utxos
      isPlutusScriptWitness C.PlutusScriptWitness{} = True
      isPlutusScriptWitness _ = False
      hasPlutusScriptWitness :: (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra)) -> Bool
      hasPlutusScriptWitness (_, C.BuildTxWith (C.ScriptWitness _ witness)) = isPlutusScriptWitness witness
      hasPlutusScriptWitness _ = False
      hasPlutusMinting = case C.txMintValue txBodyContent of
        C.TxMintNone -> False
        C.TxMintValue _ _ (C.BuildTxWith policies) -> any isPlutusScriptWitness$ Map.elems policies
    in
      -- TODO: Support Babbage-style collateral, where multiple UTxOs are used and change is made.
      if hasPlutusMinting || any hasPlutusScriptWitness (C.txIns txBodyContent)
        then case filter (\candidate -> let value = txOutToValue $ snd candidate in onlyLovelace value && C.selectLovelace value >= C.selectLovelace fee) candidateCollateral of
          (txIn, _) : _ -> pure $ C.TxInsCollateral C.CollateralInBabbageEra [txIn]
          []            -> Left . CoinSelectionFailed $ "No collateral found in " <> show utxos <> "."
        else pure C.TxInsCollateralNone  -- No collateral is needed unless a Plutus script is being executed.

  -- Bound the lovelace that must be included with change
  -- Worst case scenario of how much ADA would be added to the native and non-native change outputs
  minUtxo <-
    (<>) <$> findMinUtxo protocol (changeAddress, Nothing, universe)  -- Output to native tokens.
         <*> findMinUtxo protocol (changeAddress, Nothing, mempty  )  -- Pure lovelace to change address.

  let
    -- Compute the value of the outputs.
    outputsFromBody :: C.Value
    outputsFromBody = foldMap txOutToValue $ C.txOuts txBodyContent

    -- Sum of value that's been placed in the tx body
    -- example: spending the value at the Marlowe script address, this will be here as an input when we get the txBodyContent
    inputTxIns = map fst $ C.txIns txBodyContent
    inputsFromBody = foldMap (txOutToValue . snd) $ filter (flip elem inputTxIns . fst) utxos

    mintValue = case C.txMintValue txBodyContent of
      C.TxMintValue _ value _ -> value
      _ -> mempty

    -- Find the net additional input that is needed; the extra input we need to find, worst case
    targetSelectionValue :: C.Value
    targetSelectionValue = outputsFromBody <> fee <> minUtxo <> (C.negateValue inputsFromBody <> C.negateValue mintValue)

    -- Remove the lovelace from a value.
    deleteLovelace :: C.Value -> C.Value
    deleteLovelace value = value <> C.negateValue (C.lovelaceToValue $ C.selectLovelace value)

    -- Compute the excess and missing tokens in a value.
    matchingCoins :: C.Value -> C.Value -> (Int, Int)
    matchingCoins required candidate =
      let
        delta :: [C.Quantity]
        delta =
          fmap snd
            . C.valueToList
            . deleteLovelace
            $ candidate <> C.negateValue required
        excess :: Int
        excess  = length $ filter (> 0) delta
        deficit :: Int
        deficit = length $ filter (< 0) delta
      in
        (excess, deficit)

  -- Ensure that coin selection for tokens is possible.
  unless (snd (matchingCoins targetSelectionValue universe) == 0)
    . Left
    . CoinSelectionFailed
    $ "Insufficient tokens available for coin selection: "
    <> show targetSelectionValue <> " required, but " <> show universe <> " available."

  -- Ensure that coin selection for lovelace is possible.
  -- unless (C.selectLovelace targetSelectionValue <= C.selectLovelace universe)
  unless (C.selectLovelace targetSelectionValue <= C.selectLovelace universe)
    . Left
    . CoinSelectionFailed
    $ "Insufficient lovelace available for coin selection: "
    <> show targetSelectionValue <> " required, but " <> show universe <> " available."

  -- Satisfy the native-token requirements.
  let
    -- Sort the UTxOs by their deficit, excess, and lovelace in priority order.
    priority :: C.Value -> (C.TxIn, C.TxOut C.CtxTx C.BabbageEra) -> (Int, Int, Bool, Bool)
    priority required candidate =
      let
        candidate' :: C.Value
        candidate' = txOutToValue $ snd candidate
        excess :: Int
        deficit :: Int
        (excess, deficit) = matchingCoins required candidate'
        notOnlyLovelace :: Bool
        notOnlyLovelace = not $ onlyLovelace candidate'
        insufficientLovelace :: Bool
        insufficientLovelace = C.selectLovelace candidate' < C.selectLovelace required
      in
        (
          deficit               -- It's most important to not miss any required coins,
        , excess                -- but we don't want extra coins;
        , notOnlyLovelace       -- prefer lovelace-only UTxOs if there is no deficit,
        , insufficientLovelace  -- and prefer UTxOs with sufficient lovelace.
        )

    -- Use a simple greedy algorithm to select coins.
    select
      :: C.Value
      -> [(C.TxIn, C.TxOut C.CtxTx C.BabbageEra)]
      -> [(C.TxIn, C.TxOut C.CtxTx C.BabbageEra)]
    select _ [] = []
    select required candidates =
      let
        -- Choose the best UTxO from the candidates.
        next :: (C.TxIn, C.TxOut C.CtxTx C.BabbageEra)
        -- next = minimumBy (compare `on` priority required) candidates
        next = minimumBy (compare `on` priority required) candidates
        -- Determine the remaining candidates.
        candidates' :: [(C.TxIn, C.TxOut C.CtxTx C.BabbageEra)]
        -- candidates' = delete next candidates
        candidates' = delete next candidates
        -- Ignore negative quantities.
        filterPositive :: C.Value -> C.Value
        filterPositive = C.valueFromList . filter ((> 0) . snd) . C.valueToList
        -- Compute the remaining requirement.
        required' :: C.Value
        required' = filterPositive $ required <> C.negateValue (txOutToValue $ snd next)
      in
        -- Decide whether to continue.
        if required' == mempty
          -- The requirements have been met.
          then pure next
          -- The requirements have not been met.
          else next : select required' candidates'

    -- Select the coins.
    selection :: [(C.TxIn, C.TxOut C.CtxTx C.BabbageEra)]
    selection = select targetSelectionValue $ filter (flip notElem inputTxIns . fst) utxos

    -- Compute the *native token* change, if any.
    change :: C.Value
    change =                                            -- This is the change required to balance native tokens.
      deleteLovelace                                    -- The lovelace are irrelevant because pure-lovelace change is handled during the final balancing.
        $ (mconcat $ txOutToValue . snd <$> selection)  -- The inputs selected by the algorithm for spending many include native tokens that weren't in the required `outputs`.
        <> C.negateValue targetSelectionValue           -- The tokens required by `outputs` (as represented in the `targetSelectionValue` requirement) shouldn't be included as change.
  -- Compute the change that contains native tokens used for balancing, omitting ones explicitly specified in the outputs.

    outputs = C.txOuts txBodyContent

  output <-
    if change == mempty
      then pure []
      else do
        (addr, val) <- ensureMinUtxo protocol (changeAddress, change)
        (: []) <$> makeTxOut addr C.TxOutDatumNone val C.ReferenceScriptNone

  let
    -- FIXME Generalize to include script witnesses
    addWitness
      :: (C.TxIn, C.TxOut C.CtxTx C.BabbageEra)
      -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))
    addWitness (txIn, _txOut) = (txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)

  -- Return the transaction with coin selection added
  pure $ txBodyContent
    { C.txInsCollateral = collateral
    , C.txIns = C.txIns txBodyContent <> fmap addWitness selection
    , C.txOuts = outputs <> (output :: [C.TxOut C.CtxTx C.BabbageEra])
    }
  -- FIXME: There are pathological failures that could happen if there are very many native tokens.

-- Ensure the fee is computed and covered, and that the excess input balance is
-- returned to the change address.
balanceTx
  :: forall v
  .  C.EraInMode C.BabbageEra C.CardanoMode
  -> SystemStart
  -> C.EraHistory C.CardanoMode
  -> C.ProtocolParameters
  -> Core.MarloweVersion v
  -> MarloweContext v
  -> WalletContext
  -> C.TxBodyContent C.BuildTx C.BabbageEra
  -> Either (ConstraintError v) (C.TxBody C.BabbageEra)
balanceTx era systemStart eraHistory protocol marloweVersion marloweCtx walletCtx@WalletContext{..} C.TxBodyContent{..} = do
  changeAddress' <- maybe
    (Left $ BalancingError "Failed to convert change address.")
    Right
    $ toCardanoAddressInEra C.cardanoEra changeAddress

  let
    -- Extract the value of a UTxO
    txOutToValue :: C.TxOut ctx C.BabbageEra -> C.Value
    txOutToValue (C.TxOut _ value _ _) = C.txOutValueToValue value

    -- Make the change output.
    mkChangeTxOut value = do
      let txOutValue = C.TxOutValue C.MultiAssetInBabbageEra value
      C.TxOut changeAddress' txOutValue C.TxOutDatumNone C.ReferenceScriptNone

    -- Repeatedly try to balance the transaction.
    balancingLoop
      :: Integer -> C.Value
      -> Either
          (ConstraintError v)
          (C.TxBodyContent C.BuildTx C.BabbageEra, C.BalancedTxBody C.BabbageEra)
    balancingLoop counter changeValue = do
      when (counter == 0) $ Left . BalancingError $
        "Unsuccessful transaction balancing: " <> show (C.TxBodyContent {..})
      let
        -- Recompute execution units with full set of UTxOs, including change.
        buildTxBodyContent = C.TxBodyContent{..} { C.txOuts = mkChangeTxOut changeValue : txOuts }
        dummyTxOut =
          C.makeTransactionBodyAutoBalance
            era
            systemStart
            eraHistory
            protocol
            mempty
            utxos
            buildTxBodyContent
            changeAddress'
            Nothing
      case dummyTxOut of
        -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
        Left (C.TxBodyErrorAdaBalanceNegative delta) -> do
          balancingLoop (counter - 1) (C.lovelaceToValue delta <> changeValue)
        Left err -> Left . BalancingError $ show err
        Right balanced@(C.BalancedTxBody (C.TxBody C.TxBodyContent { txFee = fee }) _ _) -> do
          pure (buildTxBodyContent { C.txFee = fee }, balanced)

    -- The available UTxOs.
    -- FIXME: This only needs to be the subset of available UTxOs that are actually `TxIns`, but including extras should be harmless.
    -- This time we call allUtxos we need to know the total cost, so we do want the reference script (passing True)
    utxos :: C.UTxO C.BabbageEra
    utxos = C.UTxO . SMap.fromList $ allUtxos marloweVersion marloweCtx walletCtx True

    -- Compute net of inputs and outputs, accounting for minting.
    totalIn =
      foldMap (txOutToValue . snd)
            -- 5. turn the right-side TxOutS that remain into ValueS
        . filter (flip elem (fst <$> txIns) . fst)
            -- 4. filter for membership of the TxIn in the ones from txIns
        . SMap.toList   -- 3. turn that Map into a [(TxIn, (TxOut CtxUTxO era))]
        . C.unUTxO      -- 2. extract the Map inside C.UTxO
        $ utxos         -- 1. :: C.UTxO C.BabbageEra
    totalOut = foldMap txOutToValue txOuts
    totalMint = case txMintValue of
      C.TxMintValue _ value _ -> value
      _ -> mempty

    -- Initial setup is `fee = 0` - we output all the difference as a change and expect balancing error ;-)
    initialChange = totalIn <> totalMint <> C.negateValue totalOut

  unless (onlyLovelace initialChange)
    $ Left (BalancingError "balanceTx: Change must be pure lovelace")

  -- Return the transaction body.
  (_, C.BalancedTxBody txBody _ _) <- balancingLoop 10 initialChange
  pure txBody


-- The available UTxOs.
allUtxos :: forall v ctx
         .  Core.MarloweVersion v
         -> MarloweContext v
         -> WalletContext
         -> Bool  -- False if we do not want to include the script reference
         -> [(C.TxIn, C.TxOut ctx C.BabbageEra)]
allUtxos marloweVersion MarloweContext{..} WalletContext{..} includeReferences =
  let

    -- Convert chain UTxOs to Cardano API ones.
    convertUtxo :: (Chain.TxOutRef, Chain.TransactionOutput) -> Maybe (C.TxIn, C.TxOut ctx C.BabbageEra)
    convertUtxo (txOutRef, transactionOutput) =
      (,) <$> toCardanoTxIn txOutRef <*> toCardanoTxOut' C.MultiAssetInBabbageEra transactionOutput Nothing

    -- Extra UTxO for the input from the script.
    mkMarloweUtxo :: Core.TransactionScriptOutput v -> Maybe (C.TxIn, C.TxOut ctx C.BabbageEra)
    mkMarloweUtxo Core.TransactionScriptOutput{..} =
      (,)
        <$> toCardanoTxIn utxo
        <*> (
              C.TxOut
                <$> toCardanoAddressInEra C.cardanoEra address
                <*> toCardanoTxOutValue C.MultiAssetInBabbageEra assets
                <*> pure (C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra . toCardanoScriptData $ Core.toChainDatum marloweVersion datum)
                <*> pure C.ReferenceScriptNone
            )

    mkPayoutUtxo :: (Chain.TxOutRef, Core.Payout v) -> Maybe (C.TxIn, C.TxOut ctx C.BabbageEra)
    mkPayoutUtxo (utxo, Core.Payout{..}) =
      (,)
        <$> toCardanoTxIn utxo
        <*> (
              C.TxOut
                <$> toCardanoAddressInEra C.cardanoEra address
                <*> toCardanoTxOutValue C.MultiAssetInBabbageEra assets
                <*> pure (C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra . toCardanoScriptData $ Core.toChainPayoutDatum marloweVersion datum)
                <*> pure C.ReferenceScriptNone
            )

    -- Extra UTxOs for reference scripts.
    mkReferenceUtxo :: ReferenceScriptUtxo -> Maybe (C.TxIn, C.TxOut ctx C.BabbageEra)
    mkReferenceUtxo ReferenceScriptUtxo{..} =
      (,)
        <$> toCardanoTxIn txOutRef
        <*> toCardanoTxOut' C.MultiAssetInBabbageEra txOut (Just . C.toScriptInAnyLang $ C.PlutusScript C.PlutusScriptV2 script)

  in

    mapMaybe convertUtxo (SMap.toList . Chain.unUTxOs $ availableUtxos)
      <> maybe mempty pure (mkMarloweUtxo =<< scriptOutput)
      <> mapMaybe mkPayoutUtxo (Map.toList payoutOutputs)
      <> mapMaybe mkReferenceUtxo (filter (const includeReferences) [marloweScriptUTxO, payoutScriptUTxO])


solveInitialTxBodyContent
  :: forall v
   . C.ProtocolParameters
  -> Core.MarloweVersion v
  -> MarloweContext v
  -> WalletContext
  -> TxConstraints v
  -> Either (ConstraintError v) (C.TxBodyContent C.BuildTx C.BabbageEra)
solveInitialTxBodyContent protocol marloweVersion MarloweContext{..} WalletContext{..} TxConstraints{..} = do
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
        _ <- note (MintingUtxoNotFound txOutRef) $ lookupUTxO txOutRef availableUtxos
        pure [(txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)]
      SpendRoleTokens roleTokens -> do
        let availTuples = map toUTxOTuple . toUTxOsList $ availableUtxos
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
      MarloweInput _ _ inputs -> fmap Just $ do
        Core.TransactionScriptOutput{..} <- note MissingMarloweInput scriptOutput
        txIn <- note ToCardanoError $ toCardanoTxIn utxo
        plutusScriptOrRefInput <- note ToCardanoError $ C.PReferenceScript
          <$> toCardanoTxIn (txOutRef marloweScriptUTxO)
          <*> (Just <$> toCardanoScriptHash marloweScriptHash)
        let
          scriptWitness = C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            plutusScriptOrRefInput
            (C.ScriptDatumForTxIn $ toCardanoScriptData $ Core.toChainDatum marloweVersion datum)
            ( toCardanoScriptData case marloweVersion of
                Core.MarloweV1 -> Chain.toDatum $ inputs <&> \case
                  V1.NormalInput content -> V1.Input content
                  V1.MerkleizedInput content hash _ -> V1.MerkleizedTxInput content hash
            )
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
          <$> toCardanoTxIn (Language.Marlowe.Runtime.Core.ScriptRegistry.txOutRef payoutScriptUTxO)
          <*> (Just <$> toCardanoScriptHash payoutScriptHash)
        let
          scriptWitness = C.PlutusScriptWitness
            C.PlutusScriptV2InBabbage
            C.PlutusScriptV2
            plutusScriptOrRefInput
            (C.ScriptDatumForTxIn $ toCardanoScriptData $ Core.toChainPayoutDatum marloweVersion datum)
            (C.ScriptDataConstructor 0 [])
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
      _ -> Just $ note ToCardanoError $ toCardanoTxIn (txOutRef marloweScriptUTxO)

    -- Only include the payout reference script if we are consuming any payout
    -- inputs.
    payoutTxInReference :: Maybe (Either (ConstraintError v) C.TxIn)
    payoutTxInReference
      | Set.null payoutInputConstraints = Nothing
      | otherwise = Just $ note ToCardanoError $ toCardanoTxIn (txOutRef payoutScriptUTxO)

    getMarloweOutput :: Maybe Chain.TransactionOutput
    getMarloweOutput = case marloweOutputConstraints of
      MarloweOutputConstraintsNone -> Nothing
      MarloweOutput assets datum -> Just
        $ Chain.TransactionOutput marloweAddress assets Nothing
        $ Just
        $ Core.toChainDatum marloweVersion datum

    getMerkleizedContinuationOutputs :: [Chain.TransactionOutput]
    getMerkleizedContinuationOutputs = case marloweInputConstraints of
      MarloweInput _ _ inputs -> case marloweVersion of
        Core.MarloweV1 -> flip mapMaybe inputs \case
          V1.MerkleizedInput _ _ continuation -> Just
            $ Chain.TransactionOutput changeAddress mempty Nothing
            $ Just
            $ Chain.toDatum continuation
          _ -> Nothing
      _ -> []

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
        let availTuples = map toUTxOTuple . toUTxOsList $ availableUtxos
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
        , getMerkleizedContinuationOutputs
        , roleTokenOutputs
        , getPayoutOutputs
        , getAddressOutputs
        ]

    solveTxValidityRange = case marloweInputConstraints of
      MarloweInputConstraintsNone ->
        pure ( C.TxValidityNoLowerBound
             , C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra
             )
      MarloweInput minSlotNo maxSlotNo _ -> do
        pure ( C.TxValidityLowerBound C.ValidityLowerBoundInBabbageEra minSlotNo
             , C.TxValidityUpperBound C.ValidityUpperBoundInBabbageEra maxSlotNo
             )

    Chain.TransactionMetadata encodedMetadata = encodeMarloweTransactionMetadata metadataConstraints

    txMetadata :: C.TxMetadataInEra C.BabbageEra
    txMetadata
      | Map.null encodedMetadata = C.TxMetadataNone
      | otherwise = C.TxMetadataInEra C.TxMetadataInBabbageEra $ C.TxMetadata $ toCardanoMetadata <$> encodedMetadata

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
