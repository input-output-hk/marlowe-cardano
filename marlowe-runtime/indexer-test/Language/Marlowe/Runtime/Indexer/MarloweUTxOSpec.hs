module Language.Marlowe.Runtime.Indexer.MarloweUTxOSpec
  ( spec
  ) where

import Control.Monad.Trans.State (evalState, runState)
import Control.Monad.Trans.Writer (execWriterT)
import qualified Data.ByteString as B
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersion(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Indexer.Types (MarloweUTxO(..), UnspentContractOutput(..), extractWithdrawTx)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, arbitrary, forAll, listOf, listOf1, (===), (==>))

spec :: Spec
spec = describe "MarloweUTxO" do
  extractWithdrawTxSpec

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

inputToTxOutRef :: Chain.TransactionInput -> Chain.TxOutRef
inputToTxOutRef Chain.TransactionInput{..} = Chain.TxOutRef{..}

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

genContractIdMapOf :: Gen a -> Gen (Map ContractId a)
genContractIdMapOf genItem = Map.fromList <$> listOf ((,) <$> genContractId <*> genItem)

genContractId :: Gen ContractId
genContractId = ContractId <$> genTxOutRef

genTxOutRef :: Gen Chain.TxOutRef
genTxOutRef = Chain.TxOutRef <$> genTxId <*> genTxIx

genUnspentContractOutput :: Gen UnspentContractOutput
genUnspentContractOutput = UnspentContractOutput (SomeMarloweVersion MarloweV1)
  <$> genTxOutRef
  <*> genAddress
  <*> genScriptHash

genScriptHash :: Gen Chain.ScriptHash
genScriptHash = Chain.ScriptHash . B.pack <$> listOf1 arbitrary
