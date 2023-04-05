{-# LANGUAGE DataKinds #-}
module Language.Marlowe.Runtime.Integration.ApplyInputs
  where

import Cardano.Api (BabbageEra, CardanoEra(..), TxBody(..), TxBodyContent(..), TxOut(..), getTxId)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Set as Set
import Data.Time (getCurrentTime)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressInEra, fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (TxOutRef(..))
import Language.Marlowe.Runtime.Client (applyInputs, createContract)
import Language.Marlowe.Runtime.Core.Api hiding (Contract)
import Language.Marlowe.Runtime.Integration.Common
  (Integration, Wallet(..), expectRight, getGenesisWallet, runIntegrationTest, submit)
import Language.Marlowe.Runtime.Transaction.Api
import Test.Hspec
import Test.Integration.Marlowe (MarloweRuntime(..), withLocalMarloweRuntime)

spec :: Spec
spec = focus $ describe "ApplyInputs" do
  closedSpec
  closeSpec

closedSpec :: Spec
closedSpec = describe "Closed contract" $ aroundAll setup do
  it "Should fail" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <- applyInputs
      MarloweV1
      (addresses wallet)
      contractId
      emptyMarloweTransactionMetadata
      []
    liftIO $ result `shouldBe` Left (ApplyInputsLoadMarloweContextFailed LoadMarloweContextErrorNotFound)
  where
    setup :: ActionWith (MarloweRuntime, ContractId) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      wallet <- getGenesisWallet 0
      ContractCreated{ txBody = createBody, contractId } <-
        expectRight "Failed to create contract" =<< createContract
          Nothing
          MarloweV1
          (addresses wallet)
          RoleTokensNone
          emptyMarloweTransactionMetadata
          2_000_000
          Close
      _ <- submit wallet createBody
      InputsApplied { txBody = applyBody } <-
        expectRight "Failed to close contract" =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          []
      _ <- submit wallet applyBody
      runtime <- ask
      liftIO $ runTests (runtime, contractId)

-- Not to be confused with closedSpec - this one tests a Close contract that is
-- still open.
closeSpec :: Spec
closeSpec = describe "Close contract" $ aroundAll setup do
  it "should contain the correct contractId" $ runAsIntegration \(ContractCreated{contractId = originalContractId}, InputsApplied{..}) -> do
    liftIO $ contractId `shouldBe` originalContractId
  it "should contain the correct metadata" $ runAsIntegration \(_, InputsApplied{..}) -> do
    liftIO $ metadata `shouldBe` emptyMarloweTransactionMetadata
  it "should contain the correct input" $ runAsIntegration \(ContractCreated{..}, InputsApplied{input}) -> do
    let address = marloweScriptAddress
    let txId = fromCardanoTxId $ getTxId txBody
    let utxo = TxOutRef txId 1
    liftIO $ input `shouldBe` TransactionScriptOutput {..}
  it "should contain no output" $ runAsIntegration \(_, InputsApplied{..}) -> do
    liftIO $ output `shouldBe` Nothing
  it "should specify invalid before in the past" $ runAsIntegration \(_, InputsApplied{..}) -> liftIO do
    now <- getCurrentTime
    compare invalidBefore now `shouldBe` LT
  it "should specify invalid hereafter in the future" $ runAsIntegration \(_, InputsApplied{..}) -> liftIO do
    now <- getCurrentTime
    compare invalidHereafter now `shouldBe` GT
  it "should specify the correct inputs" $ runAsIntegration \(_, InputsApplied{..}) -> liftIO do
    inputs `shouldBe` []
  it "should only output to the deposit address" $ runAsIntegration \(_, InputsApplied{..}) -> do
    wallet <- getGenesisWallet 0
    let getAddress (TxOut address _ _ _) = fromCardanoAddressInEra BabbageEra address
    let paidAddresses = case txBody of TxBody TxBodyContent{..} -> Set.fromList $ getAddress <$> txOuts
    liftIO $ paidAddresses `shouldBe` Set.singleton (changeAddress $ addresses wallet)
  where
    setup :: ActionWith (MarloweRuntime, (ContractCreated BabbageEra 'V1, InputsApplied BabbageEra 'V1)) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      wallet <- getGenesisWallet 0
      created@ContractCreated{ txBody = createBody, contractId } <-
        expectRight "Failed to create contract" =<< createContract
          Nothing
          MarloweV1
          (addresses wallet)
          RoleTokensNone
          emptyMarloweTransactionMetadata
          2_000_000
          Close
      _ <- submit wallet createBody
      inputsApplied <- expectRight "Failed to close contract" =<< applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        []
      runtime <- ask
      liftIO $ runTests (runtime, (created, inputsApplied))

runAsIntegration :: (a -> Integration ()) -> ActionWith (MarloweRuntime, a)
runAsIntegration action (runtime, a) = runIntegrationTest (action a) runtime
