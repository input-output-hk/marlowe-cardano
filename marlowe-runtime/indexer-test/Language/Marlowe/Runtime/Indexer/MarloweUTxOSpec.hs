{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Indexer.MarloweUTxOSpec
  ( spec
  ) where

import Cardano.Api
  (CardanoMode, ConsensusMode(CardanoMode), EraHistory(EraHistory), SystemStart(SystemStart), hashScriptData)
import Control.Monad (guard, mfilter, zipWithM)
import Control.Monad.Trans.State (State, evalState, execState, runState)
import Control.Monad.Trans.Writer (WriterT, execWriterT)
import Data.Bifunctor (Bifunctor(bimap))
import Data.Foldable (Foldable(fold), asum)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.List (nub)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.SOP.Strict (K(..), NP(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Traversable (for)
import Language.Marlowe (POSIXTime(POSIXTime))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.ChainSync.Api as TransactionOutput (TransactionOutput(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as TxOutRef (TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api (emptyMarloweTransactionMetadata, encodeMarloweTransactionMetadata)
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.Core.Gen ()
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.History.Api
  (CreateStep(..), ExtractCreationError(..), ExtractMarloweTransactionError, SomeCreateStep(..))
import Language.Marlowe.Runtime.Indexer.Types
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusCurrencySymbol, fromPlutusTokenName, toPlutusCurrencySymbol)
import Language.Marlowe.Util (ada)
import Ouroboros.Consensus.BlockchainTime (RelativeTime(..), mkSlotLength)
import Ouroboros.Consensus.HardFork.History
  (Bound(..), EraEnd(..), EraParams(..), EraSummary(..), SafeZone(..), mkInterpreter)
import Ouroboros.Consensus.HardFork.History.Summary (summaryWithExactly)
import Ouroboros.Consensus.Util.Counting (Exactly(..))
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Arbitrary (SemiArbitrary(semiArbitrary), arbitraryValidInput)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Arbitrary(..), Gen, elements, forAll, genericShrink, infiniteListOf, listOf1, resize, sized, (===), (==>))
import Test.QuickCheck.Gen (sublistOf)

spec :: Spec
spec = describe "MarloweUTxO" do
  extractCreateTxSpec
  extractApplyInputsTxSpec
  extractWithdrawTxSpec

testBlockHeader :: Chain.BlockHeader
testBlockHeader = Chain.BlockHeader 6 "" 6

extractCreateTxSpec :: Spec
extractCreateTxSpec = describe "extractCreateTx" do
  prop "extracts the expected transaction" $ forAll (genCreateTx marloweScripts) \createTx utxo ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      evalState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo === [CreateTransaction createTx]
  prop "extracts nothing if the script hash is not found" $ forAll (genCreateTx marloweScripts) \createTx utxo hashes ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      evalState (execWriterT $ extractCreateTx hashes tx) utxo === []
  prop "if it extracts nothing, it doesn't change the UTxO" \tx utxo ->
    let
      (txs, utxo') = runState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo
    in
      null txs ==> utxo' === utxo
  prop "extracts nothing for apply inputs transactions" \utxo -> forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      evalState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo === []
  prop "Only adds new contracts to the UTxO" $ forAll (genCreateTx marloweScripts) \createTx utxo ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      let
        (txs, utxo') = runState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo
        addedContracts = flip foldMap txs \case
          CreateTransaction MarloweCreateTransaction{..} ->
            Map.mapKeys (Core.ContractId . Chain.TxOutRef txId) $ createStepToUnspentContractOutput <$> newContracts
          _ -> mempty
      in
        Map.difference (unspentContractOutputs utxo') (unspentContractOutputs utxo) === addedContracts
  prop "Emits invalid create transactions" $ forAll (genCreateTx marloweScripts) \createTx utxo ->
    forAll genCreateBug \bug ->
      forAll (createTxToChainTx (Just bug) createTx) \tx ->
        let
          txs = evalState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo
          failures = flip foldMap txs \case
            InvalidCreateTransaction _ err -> [err]
            _ -> mempty
        in
          failures === [bug]

extractApplyInputsTxSpec :: Spec
extractApplyInputsTxSpec = describe "extractApplyInputsTx" do
  prop "extracts the expected transaction" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        evalState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo === [ApplyInputsTransaction applyTx]
  prop "if it extracts nothing, it doesn't change the UTxO" \tx utxo ->
    let
      (txs, utxo') = runState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
    in
      null txs ==> utxo' === utxo
  prop "Preserves contracts in the UTxO if there is a script output" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          (result, utxo') = runState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
          applyTx' = asum $ result <&> \case
            ApplyInputsTransaction t -> Just t
            _ -> Nothing
          hasScriptOut = case applyTx' of
            Just MarloweApplyInputsTransaction{..} -> isJust $ Core.scriptOutput $ Core.output marloweTransaction
            Nothing -> False
        in
          hasScriptOut ==> Map.keysSet (unspentContractOutputs utxo') === Map.keysSet (unspentContractOutputs utxo)
  prop "Only modifies the contract it applies inputs to if it succeeds" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          (result, utxo') = runState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
          applyTx' = asum $ result <&> \case
            ApplyInputsTransaction t -> Just t
            _ -> Nothing
          contractId = case applyTx of MarloweApplyInputsTransaction{..} -> Core.contractId marloweTransaction
          isModified a b
            | a == b = Nothing
            | otherwise = Just a
          origOut = Map.lookup contractId $ unspentContractOutputs utxo
          newOut = Map.lookup contractId $ unspentContractOutputs utxo'
        in
          origOut /= newOut && isJust applyTx' ==>
            Map.keysSet (Map.differenceWith isModified (unspentContractOutputs utxo) (unspentContractOutputs utxo')) === Set.singleton contractId
  prop "Sets the unspent contract output to the expected value" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          (result, utxo') = runState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
          applyTx' = asum $ result <&> \case
            ApplyInputsTransaction t -> Just t
            _ -> Nothing
          contractId = case applyTx of MarloweApplyInputsTransaction{..} -> Core.contractId marloweTransaction
          expectedOut = case applyTx' of
            Nothing -> Nothing
            Just (MarloweApplyInputsTransaction Core.MarloweV1 UnspentContractOutput{..} marloweTransaction) ->
              Core.scriptOutput (Core.output marloweTransaction) <&> \Core.TransactionScriptOutput{utxo = scriptOut} -> UnspentContractOutput
                { marloweVersion
                , txOutRef = scriptOut
                , marloweAddress
                , payoutValidatorHash
                }
        in
          Map.lookup contractId (unspentContractOutputs utxo') === expectedOut
  prop "Only adds payouts" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          utxo' = execState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
          diffAndRemoveEmpty a b = mfilter (not . Set.null) $ Just $ Set.difference a b
        in
          Map.differenceWith diffAndRemoveEmpty (unspentPayoutOutputs utxo) (unspentPayoutOutputs utxo') === mempty
  prop "Only modifies payouts for the contract the inputs are applied to" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          utxo' = execState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
          contractId = case applyTx of MarloweApplyInputsTransaction{..} -> Core.contractId marloweTransaction
          isModified a b
            | a == b = Nothing
            | otherwise = Just a
          origOut = Map.lookup contractId $ unspentPayoutOutputs utxo
          newOut = Map.lookup contractId $ unspentPayoutOutputs utxo'
        in
          origOut /= newOut ==>
            Map.keysSet (Map.differenceWith isModified (unspentPayoutOutputs utxo') (unspentPayoutOutputs utxo)) === Set.singleton contractId
  prop "Doesn't leave empty payout sets" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          utxo' = execState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
        in
          Map.filter Set.null (unspentPayoutOutputs utxo') === mempty
  prop "Adds the payouts produced by the transaction" $ forAll (genApplyTx testBlockHeader) \(inputDatum, applyTx) ->
    forAll (applyTxToChainTx inputDatum Nothing applyTx) \tx ->
      forAll (addInput applyTx <$> arbitrary) \utxo ->
        let
          (result, utxo') = runState (execWriterT $ extractApplyInputsTx' testBlockHeader tx) utxo
          contractId = case applyTx of MarloweApplyInputsTransaction{..} -> Core.contractId marloweTransaction
          oldValue = fold $ Map.lookup contractId $ unspentPayoutOutputs utxo
          newValue = fold $ Map.lookup contractId $ unspentPayoutOutputs utxo'
          payouts = result & foldMap \case
            ApplyInputsTransaction MarloweApplyInputsTransaction{..} ->
              Map.keysSet $ Core.payouts $ Core.output marloweTransaction
            _ -> mempty
        in
          Set.difference newValue oldValue === payouts

addInput :: MarloweApplyInputsTransaction -> MarloweUTxO -> MarloweUTxO
addInput MarloweApplyInputsTransaction{..} utxo = utxo
  { unspentContractOutputs = Map.insert (Core.contractId marloweTransaction) marloweInput $ unspentContractOutputs utxo
  }

eraHistory :: EraHistory CardanoMode
eraHistory = EraHistory CardanoMode
  $ mkInterpreter
  $ summaryWithExactly
  $ Exactly
  $  K (oneMillisecondEraSummary 0) -- Byron lasted 1 ms
  :* K (oneMillisecondEraSummary 1) -- Shelley lasted 1 ms
  :* K (oneMillisecondEraSummary 2) -- Allegra lasted 1 ms
  :* K (oneMillisecondEraSummary 3) -- Mary lasted 1 ms
  :* K (oneMillisecondEraSummary 4) -- Alonzo lasted 1 ms
  :* K (unboundedEraSummary 5) -- Babbage never ends
  :* Nil

unboundedEraSummary :: Integer -> EraSummary
unboundedEraSummary i = EraSummary
  { eraStart = oneMillisecondBound i
  , eraEnd = EraUnbounded
  , eraParams = EraParams
    { eraEpochSize = 1
    , eraSlotLength = mkSlotLength 0.001
    , eraSafeZone = UnsafeIndefiniteSafeZone
    }
  }

oneMillisecondEraSummary :: Integer -> EraSummary
oneMillisecondEraSummary i = EraSummary
  { eraStart = oneMillisecondBound i
  , eraEnd = EraEnd $ oneMillisecondBound $ i + 1
  , eraParams = EraParams
    { eraEpochSize = 1
    , eraSlotLength = mkSlotLength 0.001
    , eraSafeZone = UnsafeIndefiniteSafeZone
    }
  }

oneMillisecondBound :: Integer -> Bound
oneMillisecondBound i = Bound
  { boundTime = RelativeTime $ fromInteger i / 1000
  , boundSlot = fromInteger i
  , boundEpoch = fromInteger i
  }

extractApplyInputsTx' :: Chain.BlockHeader -> Chain.Transaction -> WriterT [MarloweTransaction] (State MarloweUTxO) ()
extractApplyInputsTx' = extractApplyInputsTx
  (SystemStart $ posixSecondsToUTCTime $ secondsToNominalDiffTime 0)
  eraHistory

extractWithdrawTxSpec :: Spec
extractWithdrawTxSpec = describe "extractWithdrawTx" do
  prop "writes nothing if no payouts were withdrawn" \txId txIns utxo ->
    all (Set.null . Set.intersection (Set.map inputToTxOutRef txIns)) (unspentPayoutOutputs utxo) ==>
      let
        tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
      in
        evalState (execWriterT $ extractWithdrawTx tx) utxo === []
  prop "does not update the marlowe UTxO if it produces nothing" \txId txIns utxo ->
    let
      tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
      (result, utxo') = runState (execWriterT $ extractWithdrawTx tx) utxo
    in
      null result ==> utxo === utxo'
  prop "Tx contains all spent payouts" \txId utxo ->
    not (Map.null $ unspentPayoutOutputs utxo) ==>
      forAll (Set.fromList <$> (zipWith mkTxIn <$> infiniteListOf arbitrary <*> sublistOf (Map.elems (unspentPayoutOutputs utxo) >>= Set.toList))) \txIns ->
        let
          tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
          consumedPayouts = Map.filter (not . Set.null) $ flip Set.intersection (Set.map inputToTxOutRef txIns) <$> unspentPayoutOutputs utxo
          expected = WithdrawTransaction MarloweWithdrawTransaction
            { consumedPayouts
            , consumingTx = txId
            }
        in
          not (null txIns) ==>
            evalState (execWriterT $ extractWithdrawTx tx) utxo === [expected]
  prop "Payouts are conserved" \txId txIns utxo ->
    let
      tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
      (txs, utxo') = runState (execWriterT $ extractWithdrawTx tx) utxo
      consumed = flip foldMap txs \case
        WithdrawTransaction MarloweWithdrawTransaction{..} -> consumedPayouts
        _ -> mempty
    in
      Map.unionWith (<>) consumed (unspentPayoutOutputs utxo') == unspentPayoutOutputs utxo
  prop "Payouts aren't duplicated" \txId txIns utxo ->
    let
      tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
      (txs, utxo') = runState (execWriterT $ extractWithdrawTx tx) utxo
      consumed = flip foldMap txs \case
        WithdrawTransaction MarloweWithdrawTransaction{..} -> consumedPayouts
        _ -> mempty
    in
      Map.filter (not . Set.null) (Map.intersectionWith Set.intersection consumed (unspentPayoutOutputs utxo')) == mempty

marloweScriptHashes :: Set Chain.ScriptHash
marloweScriptHashes = Set.map ScriptRegistry.marloweScript marloweScripts

marloweScripts :: Set ScriptRegistry.MarloweScripts
marloweScripts = ScriptRegistry.getScripts Core.MarloweV1

scriptHashToAddress :: Chain.ScriptHash -> Chain.Address
scriptHashToAddress (Chain.ScriptHash hash) = Chain.Address $ Chain.unAddress "70" <> hash

mkTxIn :: Chain.Address -> Chain.TxOutRef -> Chain.TransactionInput
mkTxIn address Chain.TxOutRef{..} = Chain.TransactionInput{..}
  where
    datumBytes = Nothing
    redeemer = Nothing

inputToTxOutRef :: Chain.TransactionInput -> Chain.TxOutRef
inputToTxOutRef Chain.TransactionInput{..} = Chain.TxOutRef{..}

genCreateBug :: Gen ExtractCreationError
genCreateBug = elements [NoCreateDatum, InvalidCreateDatum]

createTxToChainTx :: Maybe ExtractCreationError -> MarloweCreateTransaction -> Gen Chain.Transaction
createTxToChainTx bug MarloweCreateTransaction{..} = do
  inputs <- Set.fromList <$> listOf1 arbitrary
  outputs' <- for [0..maximum (Map.keys newContracts)] \txIx -> case Map.lookup txIx newContracts of
    Nothing -> arbitrary
    Just (SomeCreateStep Core.MarloweV1 CreateStep{createOutput = Core.TransactionScriptOutput{..}}) -> pure $ Chain.TransactionOutput
      address
      assets
      (Just $ fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ Chain.toDatum datum)
      (Just $ Chain.toDatum datum)
  outputs <- case bug of
    Nothing -> pure outputs'
    Just err -> do
      affectedContract <- fromIntegral @_ @Int <$> elements (Map.keys newContracts)
      let modifyOutput f = zipWithM (\i -> if i == affectedContract then f else pure) [0..] outputs'
      modifyOutput \txOut -> case err of
        TxIxNotFound -> error "Cannot inject TxIxNotFound"
        ByronAddress -> error "Cannot inject ByronAddress"
        NonScriptAddress -> error "Cannot inject NonScriptAddress"
        InvalidScriptHash -> error "Cannot inject InvalidScriptHash"
        NoCreateDatum -> pure txOut { TransactionOutput.datum = Nothing }
        InvalidCreateDatum -> do
          datum <- arbitrary
          pure txOut { TransactionOutput.datum = Just datum }
        NotCreationTransaction -> error "Cannot inject NotCreationTransaction"
  pure Chain.Transaction
    { txId
    , validityRange = Chain.Unbounded
    , metadata = newContracts & foldMap \(SomeCreateStep _ CreateStep{..}) -> encodeMarloweTransactionMetadata metadata
    , inputs
    , outputs
    , mintedTokens = mempty
    }

applyTxToChainTx
  :: V1.MarloweData
  -> Maybe ExtractMarloweTransactionError
  -> MarloweApplyInputsTransaction
  -> Gen Chain.Transaction
applyTxToChainTx inputDatum _ MarloweApplyInputsTransaction{..} = case marloweVersion of
  Core.MarloweV1 -> do
    let
      Core.Transaction{..} = marloweTransaction
      Core.TransactionOutput{..} = output
      UnspentContractOutput{txOutRef, marloweAddress} = marloweInput
      marloweChainInput = Chain.TransactionInput
        (TxOutRef.txId txOutRef)
        (TxOutRef.txIx txOutRef)
        marloweAddress
        (Just $ Chain.toDatum inputDatum)
        (Just $ Chain.toRedeemer inputs)
      doesNotSpentMarloweInput Chain.TransactionInput{..} = Chain.TxOutRef{..} /= txOutRef
    chainInputs <- Set.insert marloweChainInput . Set.filter doesNotSpentMarloweInput <$> arbitrary
    -- This conversion works because of the way the era history is setup in
    -- this test suite. Byron started at the unix epoch and all slot lengths
    -- were 1 second, so slotNo = POSIX millisecond.
    let invalidBefore = Chain.SlotNo $ floor $ utcTimeToPOSIXSeconds validityLowerBound * 1000
    let invalidHereafter = Chain.SlotNo $ floor $ utcTimeToPOSIXSeconds validityUpperBound * 1000
    let
      scriptChainOutputs = maybeToList $ scriptOutput <&> \Core.TransactionScriptOutput{..} -> Chain.TransactionOutput
        { address
        , assets
        , datumHash = Just $ fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ Chain.toDatum datum
        , datum = Just $ Chain.toDatum datum
        }
      payoutOutputs = Map.toAscList payouts <&> \(_, Core.Payout{..}) ->
        let
          chainDatum = Core.toChainPayoutDatum Core.MarloweV1 datum
        in
          Chain.TransactionOutput
            { address
            , assets
            , datumHash = Just $ fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData chainDatum
            , datum = Just chainDatum
            }
    let outputs = scriptChainOutputs <> payoutOutputs
    pure Chain.Transaction
      { txId = transactionId
      , validityRange = Chain.MinMaxBound invalidBefore invalidHereafter
      , metadata = encodeMarloweTransactionMetadata metadata
      , inputs = chainInputs
      , outputs
      , mintedTokens = mempty
      }

genCreateTx :: Set ScriptRegistry.MarloweScripts -> Gen MarloweCreateTransaction
genCreateTx scripts = do
  txId <- arbitrary
  txIxs <- sized \size -> resize (min size 10) $ nub <$> listOf1 arbitrary
  newContracts <- Map.fromList <$> for txIxs \txIx -> do
    let txOut = Chain.TxOutRef{..}
    (txIx,) <$> genSomeCreateStep scripts txOut
  pure $ MarloweCreateTransaction txId newContracts

genApplyTx :: Chain.BlockHeader -> Gen (V1.MarloweData, MarloweApplyInputsTransaction)
genApplyTx blockHeader = do
  marloweInput@UnspentContractOutput{..} <- arbitrary
  rolesCurrency <- arbitrary
  txId <- arbitrary
  contractId <- arbitrary
  let
    marloweParams = V1.MarloweParams $ toPlutusCurrencySymbol rolesCurrency
    genInputs = do
      context <- arbitrary
      marloweState <- semiArbitrary context
      marloweContract <- semiArbitrary context
      let
        boundTimes V1.TransactionInput{..} = V1.TransactionInput
          { txInterval = bimap (max (POSIXTime 0)) (max (POSIXTime 0)) txInterval
          , txInputs
          }
      txInput <- boundTimes <$> arbitraryValidInput marloweState marloweContract
      case V1.computeTransaction txInput marloweState marloweContract of
        V1.Error _ -> genInputs -- try again because the contract had no valid inputs to produce
        V1.TransactionOutput _ payments state contract -> pure (marloweState, marloweContract, txInput, payments, state, contract)
  (marloweState, marloweContract, V1.TransactionInput{..}, outPayments, outState, outContract) <- genInputs
  -- Should be let isClose = outContract == V1.Close but this seems to be
  -- always true... apparently generated contracts are only 1 layer deep at
  -- most. This simulates a transaction with a script output well enough for
  -- the purposes of this test though.
  isClose <- arbitrary
  let
    (minPOSIXTime, maxPOSIXTime) = txInterval
    inputDatum = V1.MarloweData{..}
    payoutAddress = scriptHashToAddress payoutValidatorHash
    payouts = Map.fromList $ zip (Chain.TxOutRef txId <$> [if isClose then 0 else 1..]) $ mapMaybe (paymentToPayout rolesCurrency payoutAddress) outPayments
    marloweTransaction = Core.Transaction
      txId
      contractId
      emptyMarloweTransactionMetadata
      blockHeader
      (posixSecondsToUTCTime $ fromIntegral minPOSIXTime / 1000)
      (posixSecondsToUTCTime $ fromIntegral maxPOSIXTime / 1000)
      txInputs
      Core.TransactionOutput
        { payouts
        , scriptOutput = guard (not isClose) $> Core.TransactionScriptOutput
            { address = marloweAddress
            , assets = assetsFromAccounts $ V1.accounts outState
            , utxo = Chain.TxOutRef txId 0
            , datum = V1.MarloweData marloweParams outState outContract
            }
        }
  pure
    ( inputDatum
    , MarloweApplyInputsTransaction
      { marloweVersion = Core.MarloweV1
      , marloweInput
      , marloweTransaction
      }
    )

paymentToPayout :: Chain.PolicyId -> Chain.Address -> V1.Payment -> Maybe (Core.Payout 'Core.V1)
paymentToPayout rolesCurrency payoutAddress (V1.Payment _ payee token@(V1.Token cs tn) quantity) = case payee of
  V1.Party (V1.Role tokenName) -> Just $ Core.Payout payoutAddress assets $ Chain.AssetId rolesCurrency $ fromPlutusTokenName tokenName
  _ -> Nothing
  where
    assets
      | token == ada = Chain.Assets (fromIntegral quantity) mempty
      | otherwise = Chain.Assets 0 $ Chain.Tokens $ Map.singleton (Chain.AssetId (fromPlutusCurrencySymbol cs) (fromPlutusTokenName tn)) $ fromIntegral quantity

genSomeCreateStep :: Set ScriptRegistry.MarloweScripts -> Chain.TxOutRef -> Gen SomeCreateStep
genSomeCreateStep scripts txOut = SomeCreateStep Core.MarloweV1
  <$> genCreateStep scripts txOut arbitrary (assetsFromAccounts . V1.accounts . V1.marloweState)

genCreateStep
  :: Set ScriptRegistry.MarloweScripts
  -> Chain.TxOutRef
  -> Gen (Core.Datum v)
  -> (Core.Datum v -> Chain.Assets)
  -> Gen (CreateStep v)
genCreateStep scripts txOut genMarloweDatum assetsFromDatum = do
  ScriptRegistry.MarloweScripts{..} <- elements $ Set.toList scripts
  CreateStep
    <$> genTransactionScriptOutput (scriptHashToAddress marloweScript) txOut genMarloweDatum assetsFromDatum
    <*> pure emptyMarloweTransactionMetadata
    <*> pure payoutScript

genTransactionScriptOutput
  :: Chain.Address
  -> Chain.TxOutRef
  -> Gen (Core.Datum v)
  -> (Core.Datum v -> Chain.Assets)
  -> Gen (Core.TransactionScriptOutput v)
genTransactionScriptOutput address txOut genMarloweDatum assetsFromDatum = do
  datum <- genMarloweDatum
  pure $ Core.TransactionScriptOutput address (assetsFromDatum datum) txOut datum

assetsFromAccounts :: V1.Accounts -> Chain.Assets
assetsFromAccounts = foldMap assetsFromAccount . AM.toList
  where
    assetsFromAccount ((_, token@(V1.Token cs tn)), quantity)
      | token == ada = Chain.Assets (fromIntegral quantity) mempty
      | otherwise = Chain.Assets 0
          $ Chain.Tokens
          $ Map.singleton (Chain.AssetId (fromPlutusCurrencySymbol cs) (fromPlutusTokenName tn))
          $ fromIntegral quantity

instance Arbitrary MarloweUTxO where
  arbitrary = MarloweUTxO <$> arbitrary <*> (Map.filter (not . Set.null) <$> arbitrary)
  shrink = genericShrink

instance Arbitrary UnspentContractOutput where
  arbitrary = UnspentContractOutput (Core.SomeMarloweVersion Core.MarloweV1)
    <$> arbitrary
    <*> arbitrary
    <*> elements (ScriptRegistry.payoutScript <$> Set.toList marloweScripts)
  shrink = genericShrink
