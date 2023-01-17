{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.Indexer.MarloweUTxOSpec
  ( spec
  ) where

import Cardano.Api (hashScriptData)
import Control.Monad (zipWithM)
import Control.Monad.Trans.State (evalState, runState)
import Control.Monad.Trans.Writer (execWriterT)
import qualified Data.ByteString as B
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (for)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.ChainSync.Api as TransactionInput (TransactionInput(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as TransactionOutput (TransactionOutput(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as ScriptRegistry
import Language.Marlowe.Runtime.History.Api (CreateStep(..), ExtractCreationError(..), SomeCreateStep(..))
import Language.Marlowe.Runtime.Indexer.Types
import Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusCurrencySymbol, fromPlutusTokenName, toPlutusCurrencySymbol)
import Language.Marlowe.Util (ada)
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Arbitrary (SemiArbitrary(semiArbitrary))
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  (Gen, arbitrary, elements, forAll, infiniteListOf, listOf, listOf1, oneof, resize, sized, (===), (==>))
import Test.QuickCheck.Gen (sublistOf)

spec :: Spec
spec = describe "MarloweUTxO" do
  extractCreateTxSpec
  extractApplyInputsTxSpec
  extractWithdrawTxSpec

extractCreateTxSpec :: Spec
extractCreateTxSpec = describe "extractCreateTx" do
  prop "extracts the expected transaction" $ forAll (genCreateTx marloweScripts) \createTx ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      forAll genMarloweUTxO \utxo ->
        evalState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo === [CreateTransaction createTx]
  prop "extracts nothing if the script hash is not found" $ forAll (genCreateTx marloweScripts) \createTx ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      forAll genMarloweUTxO \utxo ->
        forAll (Set.fromList <$> listOf genScriptHash) \hashes ->
          evalState (execWriterT $ extractCreateTx hashes tx) utxo === []
  prop "if it extracts nothing, it doesn't change the UTxO" $ forAll genTx \tx ->
    forAll genMarloweUTxO \utxo ->
      let
        (txs, utxo') = runState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo
      in
        null txs ==> utxo' === utxo
  prop "extracts nothing if there is a marlowe script input" $ forAll (genCreateTx marloweScripts) \createTx ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      forAll genMarloweUTxO \utxo ->
        forAll genTxIn \txIn ->
          forAll (elements $ Set.toList marloweScriptHashes) \scriptHash ->
            let
              marloweInput = txIn { TransactionInput.address = scriptHashToAddress scriptHash }
              tx' = tx { Chain.inputs = Set.insert marloweInput $ Chain.inputs tx }
            in
              evalState (execWriterT $ extractCreateTx marloweScriptHashes tx') utxo === []
  prop "Only adds new contracts to the UTxO" $ forAll (genCreateTx marloweScripts) \createTx ->
    forAll (createTxToChainTx Nothing createTx) \tx ->
      forAll genMarloweUTxO \utxo ->
        let
          (txs, utxo') = runState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo
          addedContracts = flip foldMap txs \case
            CreateTransaction MarloweCreateTransaction{..} ->
              Map.mapKeys (Core.ContractId . Chain.TxOutRef txId) $ createStepToUnspentContractOutput <$> newContracts
            _ -> mempty
        in
          Map.difference (unspentContractOutputs utxo') (unspentContractOutputs utxo) === addedContracts
  prop "Emits invalid create transactions" $ forAll (genCreateTx marloweScripts) \createTx ->
    forAll genCreateBug \bug ->
      forAll (createTxToChainTx (Just bug) createTx) \tx ->
        forAll genMarloweUTxO \utxo ->
          let
            txs = evalState (execWriterT $ extractCreateTx marloweScriptHashes tx) utxo
            failures = flip foldMap txs \case
              InvalidCreateTransaction _ err -> [err]
              _ -> mempty
          in
            failures === [bug]

extractApplyInputsTxSpec :: Spec
extractApplyInputsTxSpec = describe "extractApplyInputsTx" do
  pure ()

extractWithdrawTxSpec :: Spec
extractWithdrawTxSpec = describe "extractWithdrawTx" do
  prop "writes nothing if no payouts were withdrawn" $ forAll genTxId \txId ->
    forAll (Set.fromList <$> listOf1 genTxIn) \txIns ->
      forAll genMarloweUTxO \utxo -> all (Set.null . Set.intersection (Set.map inputToTxOutRef txIns)) (unspentPayoutOutputs utxo) ==>
        let
          tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
        in
          evalState (execWriterT $ extractWithdrawTx tx) utxo === []
  prop "does not update the marlowe UTxO if it produces nothing" $ forAll genTxId \txId ->
    forAll (Set.fromList <$> listOf1 genTxIn) \txIns ->
      forAll genMarloweUTxO \utxo ->
        let
          tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
          (result, utxo') = runState (execWriterT $ extractWithdrawTx tx) utxo
        in
          null result ==> utxo === utxo'
  prop "Tx contains all spent payouts" $ forAll genTxId \txId ->
    forAll genMarloweUTxO \utxo -> not (Map.null $ unspentPayoutOutputs utxo) ==>
      forAll (Set.fromList <$> (zipWith mkTxIn <$> infiniteListOf genAddress <*> sublistOf (Map.elems (unspentPayoutOutputs utxo) >>= Set.toList))) \txIns ->
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
  prop "Payouts are conserved" $ forAll genTxId \txId ->
    forAll (Set.fromList <$> listOf1 genTxIn) \txIns ->
      forAll genMarloweUTxO \utxo ->
        let
          tx = Chain.Transaction txId Chain.Unbounded mempty txIns [] mempty
          (txs, utxo') = runState (execWriterT $ extractWithdrawTx tx) utxo
          consumed = flip foldMap txs \case
            WithdrawTransaction MarloweWithdrawTransaction{..} -> consumedPayouts
            _ -> mempty
        in
          Map.unionWith (<>) consumed (unspentPayoutOutputs utxo') == unspentPayoutOutputs utxo
  prop "Payouts aren't duplicated" $ forAll genTxId \txId ->
    forAll (Set.fromList <$> listOf1 genTxIn) \txIns ->
      forAll genMarloweUTxO \utxo ->
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
  inputs <- Set.fromList <$> listOf1 genTxIn
  outputs' <- for [0..maximum (Map.keys newContracts)] \txIx -> case Map.lookup txIx newContracts of
    Nothing -> genTxOut
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
          datum <- genDatum
          pure txOut { TransactionOutput.datum = Just datum }
        NotCreationTransaction -> error "Cannot inject NotCreationTransaction"
  pure Chain.Transaction
    { txId
    , validityRange = Chain.Unbounded
    , metadata
    , inputs
    , outputs
    , mintedTokens = mempty
    }

genDatum :: Gen Chain.Datum
genDatum = sized \case
  0 -> oneof [Chain.I <$> arbitrary, Chain.B . B.pack <$> listOf arbitrary]
  size -> resize (size `div` 2) $ oneof
    [ Chain.I <$> arbitrary
    , Chain.B . B.pack <$> listOf arbitrary
    , Chain.Map <$> (listOf $ (,) <$> genDatum <*> genDatum)
    , Chain.List <$> listOf genDatum
    , Chain.Constr <$> arbitrary <*> listOf genDatum
    ]

genTxOut :: Gen Chain.TransactionOutput
genTxOut = Chain.TransactionOutput
  <$> genAddress
  <*> genAssets
  <*> pure Nothing
  <*> pure Nothing

genAssets :: Gen Chain.Assets
genAssets = Chain.Assets <$> (Chain.Lovelace <$> arbitrary) <*> (Chain.Tokens . Map.fromList <$> listOf ((,) <$> genAssetId <*> (Chain.Quantity <$> arbitrary)))

genAssetId :: Gen Chain.AssetId
genAssetId = Chain.AssetId <$> genPolicyId <*> genTokenName

genTokenName :: Gen Chain.TokenName
genTokenName = Chain.TokenName . B.pack <$> listOf arbitrary

genCreateTx :: Set ScriptRegistry.MarloweScripts -> Gen MarloweCreateTransaction
genCreateTx scripts = do
  txId <- genTxId
  txIxs <- sized \size -> resize (min size 10) $ nub <$> listOf1 genTxIx
  newContracts <- Map.fromList <$> for txIxs \txIx -> do
    let txOut = Chain.TxOutRef{..}
    (txIx,) <$> genSomeCreateStep scripts txOut
  pure $ MarloweCreateTransaction txId newContracts mempty

genSomeCreateStep :: Set ScriptRegistry.MarloweScripts -> Chain.TxOutRef -> Gen SomeCreateStep
genSomeCreateStep scripts txOut = SomeCreateStep Core.MarloweV1
  <$> genCreateStep scripts txOut genV1Datum (assetsFromAccounts . V1.accounts . V1.marloweState)

genV1Datum :: Gen V1.MarloweData
genV1Datum = do
  context <- arbitrary
  V1.MarloweData
    <$> (V1.MarloweParams . toPlutusCurrencySymbol <$> genPolicyId)
    <*> semiArbitrary context
    <*> semiArbitrary context

genCreateStep
  :: Set ScriptRegistry.MarloweScripts
  -> Chain.TxOutRef
  -> Gen (Core.Datum v)
  -> (Core.Datum v -> Chain.Assets)
  -> Gen (CreateStep v)
genCreateStep scripts txOut genMarloweDatum assetsFromDatum = do
  ScriptRegistry.MarloweScripts{..} <- elements $ Set.toList scripts
  CreateStep
    <$> genTransactionScriptHash (scriptHashToAddress marloweScript) txOut genMarloweDatum assetsFromDatum
    <*> pure payoutScript

genTransactionScriptHash
  :: Chain.Address
  -> Chain.TxOutRef
  -> Gen (Core.Datum v)
  -> (Core.Datum v -> Chain.Assets)
  -> Gen (Core.TransactionScriptOutput v)
genTransactionScriptHash address txOut genMarloweDatum assetsFromDatum = do
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

genTx :: Gen Chain.Transaction
genTx = Chain.Transaction
  <$> genTxId
  <*> genValidityRange
  <*> pure mempty
  <*> (Set.fromList <$> listOf1 genTxIn)
  <*> listOf1 genTxOut
  <*> pure mempty

genValidityRange :: Gen Chain.ValidityRange
genValidityRange = oneof
  [ pure Chain.Unbounded
  , Chain.MinBound <$> (Chain.SlotNo <$> arbitrary)
  , Chain.MaxBound <$> (Chain.SlotNo <$> arbitrary)
  , do
      a <- Chain.SlotNo <$> arbitrary
      b <- Chain.SlotNo <$> arbitrary
      pure $ Chain.MinMaxBound (min a b) (max a b)
  ]

genTxIn :: Gen Chain.TransactionInput
genTxIn = Chain.TransactionInput
  <$> genTxId
  <*> genTxIx
  <*> genAddress
  <*> pure Nothing
  <*> pure Nothing

genTxId :: Gen Chain.TxId
genTxId = Chain.TxId . B.pack <$> listOf1 arbitrary

genTxIx :: Gen Chain.TxIx
genTxIx = Chain.TxIx <$> arbitrary

genAddress :: Gen Chain.Address
genAddress = Chain.Address . B.pack <$> listOf1 arbitrary

genMarloweUTxO :: Gen MarloweUTxO
genMarloweUTxO = MarloweUTxO
  <$> genContractIdMapOf genUnspentContractOutput
  <*> genContractIdMapOf (Set.fromList <$> listOf1 genTxOutRef)

genContractIdMapOf :: Gen a -> Gen (Map Core.ContractId a)
genContractIdMapOf genItem = Map.fromList <$> listOf ((,) <$> genContractId <*> genItem)

genContractId :: Gen Core.ContractId
genContractId = Core.ContractId <$> genTxOutRef

genTxOutRef :: Gen Chain.TxOutRef
genTxOutRef = Chain.TxOutRef <$> genTxId <*> genTxIx

genUnspentContractOutput :: Gen UnspentContractOutput
genUnspentContractOutput = UnspentContractOutput (Core.SomeMarloweVersion Core.MarloweV1)
  <$> genTxOutRef
  <*> genAddress
  <*> genScriptHash

genScriptHash :: Gen Chain.ScriptHash
genScriptHash = Chain.ScriptHash . B.pack <$> listOf1 arbitrary

genPolicyId :: Gen Chain.PolicyId
genPolicyId = Chain.PolicyId . B.pack <$> listOf arbitrary
