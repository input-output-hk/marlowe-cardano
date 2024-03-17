{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Integration.ApplyInputs where

import Cardano.Api (
  BabbageEraOnwards (BabbageEraOnwardsBabbage),
  CardanoEra (..),
  TxBody (..),
  TxBodyContent (..),
  TxOut (..),
  babbageEraOnwardsToCardanoEra,
  getTxId,
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Functor (void)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time (UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import qualified Language.Marlowe.Core.V1.Semantics as Semantics
import Language.Marlowe.Core.V1.Semantics.Types hiding (TokenName)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Types
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Address
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressInEra,
  fromCardanoTxId,
  fromCardanoTxOutValue,
 )
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (AssetId), Assets (Assets), TokenName, TxOutRef (..))
import Language.Marlowe.Runtime.Client (applyInputs, createContract)
import Language.Marlowe.Runtime.Core.Api hiding (Contract)
import Language.Marlowe.Runtime.Integration.Common (
  Integration,
  Wallet (..),
  deposit,
  expectJust,
  expectLeft,
  expectRight,
  getGenesisWallet,
  runIntegrationTest,
  submit,
  submit',
 )
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Util (dataHash)
import PlutusLedgerApi.V2 (POSIXTime (POSIXTime))
import Test.Hspec
import Test.Integration.Marlowe (MarloweRuntime (..), withLocalMarloweRuntime)
import UnliftIO (Concurrently (Concurrently, runConcurrently))

spec :: Spec
spec = describe "ApplyInputs" do
  closedSpec
  closeSpec
  paySpec
  whenSpec

closedSpec :: Spec
closedSpec = parallel $ describe "Closed contract" $ aroundAll setup do
  it "Should fail" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
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
      ContractCreated era0 ContractCreatedInEra{txBody = createBody, contractId} <-
        expectRight "Failed to create contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            (Left Close)
      _ <- submit wallet era0 createBody
      applyInputs MarloweV1 (addresses wallet) contractId emptyMarloweTransactionMetadata []
        >>= expectRight "Failed to close contract"
        >>= \case
          (InputsApplied era1 InputsAppliedInEra{txBody = applyBody}) -> do
            _ <- submit wallet era1 applyBody
            runtime <- ask
            liftIO $ runTests (runtime, contractId)

-- Not to be confused with closedSpec - this one tests a Close contract that is
-- still open.
closeSpec :: Spec
closeSpec = parallel $ describe "Close contract" $ aroundAll setup do
  it "should contain the correct contractId" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId = originalContractId}, InputsApplied _ InputsAppliedInEra{..}) -> do
    liftIO $ contractId `shouldBe` originalContractId
  it "should contain the correct metadata" $ runAsIntegration \(_, InputsApplied _ InputsAppliedInEra{..}) -> do
    liftIO $ metadata `shouldBe` emptyMarloweTransactionMetadata
  it "should contain the correct input" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}, InputsApplied _ InputsAppliedInEra{input}) -> do
    let address = marloweScriptAddress
    let txId = fromCardanoTxId $ getTxId txBody
    let utxo = TxOutRef txId 1
    liftIO $ input `shouldBe` TransactionScriptOutput{..}
  it "should contain no output" $ runAsIntegration \(_, InputsApplied _ InputsAppliedInEra{..}) -> do
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should specify invalid before in the past" $ runAsIntegration \(_, InputsApplied _ InputsAppliedInEra{..}) -> liftIO do
    now <- getCurrentTime
    compare invalidBefore now `shouldBe` LT
  it "should specify invalid hereafter in the future" $ runAsIntegration \(_, InputsApplied _ InputsAppliedInEra{..}) -> liftIO do
    now <- getCurrentTime
    compare invalidHereafter now `shouldBe` GT
  it "should specify the correct inputs" $ runAsIntegration \(_, InputsApplied _ InputsAppliedInEra{..}) -> liftIO do
    inputs `shouldBe` []
  it "should only output to the deposit address" $ runAsIntegration \(_, InputsApplied era InputsAppliedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    let getAddress (TxOut address _ _ _) = fromCardanoAddressInEra (babbageEraOnwardsToCardanoEra era) address
    let paidAddresses = case txBody of TxBody TxBodyContent{..} -> Set.fromList $ getAddress <$> txOuts
    liftIO $ paidAddresses `shouldBe` Set.singleton (changeAddress $ addresses wallet)
  where
    setup :: ActionWith (MarloweRuntime, (ContractCreated 'V1, InputsApplied 'V1)) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      wallet <- getGenesisWallet 0
      createContract
        Nothing
        MarloweV1
        (addresses wallet)
        Nothing
        RoleTokensNone
        emptyMarloweTransactionMetadata
        Nothing
        mempty
        (Left Close)
        >>= expectRight "Failed to create contract"
        >>= \created@(ContractCreated era0 ContractCreatedInEra{txBody = createBody, contractId}) -> do
          _ <- submit wallet era0 createBody
          inputsApplied <-
            expectRight "Failed to close contract"
              =<< applyInputs
                MarloweV1
                (addresses wallet)
                contractId
                emptyMarloweTransactionMetadata
                []
          runtime <- ask
          liftIO $ runTests (runtime, (created, inputsApplied))

data PayTestData = PayTestData
  { payRoleAccountCreated :: ContractCreated 'V1
  , payRoleAccountApplied :: InputsApplied 'V1
  , payAddressAccountCreated :: ContractCreated 'V1
  , payAddressAccountApplied :: InputsApplied 'V1
  , payRolePartyCreated :: ContractCreated 'V1
  , payRolePartyApplied :: InputsApplied 'V1
  , payAddressPartyCreated :: ContractCreated 'V1
  , payAddressPartyApplied :: InputsApplied 'V1
  , payDepth1Created :: ContractCreated 'V1
  , payDepth1Applied :: InputsApplied 'V1
  , payDepth2AccountCreated :: ContractCreated 'V1
  , payDepth2AccountApplied :: InputsApplied 'V1
  , payDepth2PartyCreated :: ContractCreated 'V1
  , payDepth2PartyApplied :: InputsApplied 'V1
  , startTime :: UTCTime
  }

paySpec :: Spec
paySpec = parallel $ describe "Pay contracts" $ aroundAll setup do
  describe "Pay to role account" do
    it "should contain no output" $ runAsIntegration \PayTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure payRoleAccountApplied
      liftIO $ scriptOutput output `shouldBe` Nothing
    it "should send a payout to the role validator" $ runAsIntegration \PayTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{payoutScriptAddress, rolesCurrency} = payRoleAccountCreated
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payRoleAccountApplied
      liftIO $
        Map.elems (payouts output)
          `shouldBe` [Payout payoutScriptAddress (Assets 2_000_000 mempty) $ AssetId rolesCurrency "Role"]
  describe "Pay to address account" do
    it "should contain no output" $ runAsIntegration \PayTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure payAddressAccountApplied
      liftIO $ scriptOutput output `shouldBe` Nothing
    it "should send a payout to the wallet" $ runAsIntegration \PayTestData{..} -> do
      wallet2 <- getGenesisWallet 1
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payAddressAccountApplied
      let isPayout (TxOut address _ _ _) = fromCardanoAddressInEra BabbageEra address == changeAddress (addresses wallet2)
      let getValue (TxOut _ value _ _) = fromCardanoTxOutValue value
      let payoutOutputs = getValue <$> case txBody of TxBody TxBodyContent{..} -> filter isPayout txOuts
      liftIO $ payoutOutputs `shouldBe` [Assets 2_000_000 mempty]
  describe "Pay to role party" do
    it "should contain no output" $ runAsIntegration \PayTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure payRolePartyApplied
      liftIO $ scriptOutput output `shouldBe` Nothing
    it "should send a payout to the role validator" $ runAsIntegration \PayTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{payoutScriptAddress, rolesCurrency} = payRolePartyCreated
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payRolePartyApplied
      liftIO $
        Map.elems (payouts output)
          `shouldBe` [Payout payoutScriptAddress (Assets 2_000_000 mempty) $ AssetId rolesCurrency "Role"]
  describe "Pay to address party" do
    it "should contain no output" $ runAsIntegration \PayTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure payAddressPartyApplied
      liftIO $ scriptOutput output `shouldBe` Nothing
    it "should send a payout to the wallet" $ runAsIntegration \PayTestData{..} -> do
      wallet2 <- getGenesisWallet 1
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payAddressPartyApplied
      let isPayout (TxOut address _ _ _) = fromCardanoAddressInEra BabbageEra address == changeAddress (addresses wallet2)
      let getValue (TxOut _ value _ _) = fromCardanoTxOutValue value
      let payoutOutputs = getValue <$> case txBody of TxBody TxBodyContent{..} -> filter isPayout txOuts
      liftIO $ payoutOutputs `shouldBe` [Assets 2_000_000 mempty]
  describe "Pay with input inside" do
    it "should contain no output" $ runAsIntegration \PayTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure payDepth1Applied
      liftIO $ scriptOutput output `shouldBe` Nothing
    it "should send a payout to the role validator" $ runAsIntegration \PayTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{payoutScriptAddress, rolesCurrency} = payDepth1Created
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payDepth1Applied
      liftIO $
        Map.elems (payouts output)
          `shouldBe` [Payout payoutScriptAddress (Assets 2_000_000 mempty) $ AssetId rolesCurrency "Role"]
  describe "Pay to account with two inputs inside" do
    it "should contain the correct output" $ runAsIntegration \PayTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{marloweScriptAddress, assets} = payDepth2AccountCreated
      InputsApplied _ InputsAppliedInEra{..} <- pure payDepth2AccountApplied
      TransactionScriptOutput address assets' utxo' MarloweData{..} <- expectJust "Expected an output" $ scriptOutput output
      liftIO $ address `shouldBe` marloweScriptAddress
      liftIO $ assets' `shouldBe` assets
      liftIO $ utxo' `shouldBe` TxOutRef (fromCardanoTxId $ getTxId txBody) 1
      liftIO $
        marloweContract
          `shouldBe` When
            [ Case (Notify TrueObs) Close
            ]
            (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 200) startTime)
            Close
    it "should send no payout" $ runAsIntegration \PayTestData{..} -> do
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payDepth2AccountApplied
      liftIO $ payouts output `shouldBe` mempty
  describe "Pay to party with two inputs inside" do
    it "should contain the correct output" $ runAsIntegration \PayTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{marloweScriptAddress} = payDepth2PartyCreated
      InputsApplied _ InputsAppliedInEra{..} <- pure payDepth2PartyApplied
      TransactionScriptOutput address assets' utxo' MarloweData{..} <- expectJust "Expected an output" $ scriptOutput output
      liftIO $ address `shouldBe` marloweScriptAddress
      liftIO $ assets' `shouldBe` Assets 8_000_000 mempty
      liftIO $ utxo' `shouldBe` TxOutRef (fromCardanoTxId $ getTxId txBody) 1
      liftIO $
        marloweContract
          `shouldBe` When
            [ Case (Notify TrueObs) Close
            ]
            (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 200) startTime)
            Close
    it "should send a payout to the role validator" $ runAsIntegration \PayTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{payoutScriptAddress, rolesCurrency} = payDepth2PartyCreated
      InputsApplied BabbageEraOnwardsBabbage InputsAppliedInEra{..} <- pure payDepth2PartyApplied
      liftIO $
        Map.elems (payouts output)
          `shouldBe` [Payout payoutScriptAddress (Assets 2_000_000 mempty) $ AssetId rolesCurrency "Role"]
  where
    setup :: ActionWith (MarloweRuntime, PayTestData) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      startTime <- liftIO getCurrentTime
      wallet1 <- getGenesisWallet 0
      wallet2 <- getGenesisWallet 1
      let timeout1 = addUTCTime (secondsToNominalDiffTime 100) startTime
      let timeout2 = addUTCTime (secondsToNominalDiffTime 200) startTime
      let walletParty = Address Address.testnet . fromJust . toPlutusAddress . changeAddress . addresses
      let mkPay = Pay $ walletParty wallet1
      payRoleAccountCreated <-
        expectRight "Failed to create pay role account contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            (mkRoleTokens [("Role", wallet2)])
            emptyMarloweTransactionMetadata
            (Just 2_000_000)
            mempty
            (Left $ mkPay (Account $ Role "Role") ada (Constant 2_000_000) Close)
      submitCreate wallet1 payRoleAccountCreated
      payAddressAccountCreated <-
        expectRight "Failed to create pay address account contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            (Just 2_000_000)
            mempty
            (Left $ mkPay (Account $ walletParty wallet2) ada (Constant 2_000_000) Close)
      submitCreate wallet1 payAddressAccountCreated
      payRolePartyCreated <-
        expectRight "Failed to create pay role party contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            (mkRoleTokens [("Role", wallet2)])
            emptyMarloweTransactionMetadata
            (Just 2_000_000)
            mempty
            (Left $ mkPay (Party $ Role "Role") ada (Constant 2_000_000) Close)
      submitCreate wallet1 payRolePartyCreated
      payAddressPartyCreated <-
        expectRight "Failed to create pay address party contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            (Just 2_000_000)
            mempty
            (Left $ mkPay (Party $ walletParty wallet2) ada (Constant 2_000_000) Close)
      submitCreate wallet1 payAddressPartyCreated
      payDepth1Created <-
        expectRight "Failed to create pay depth 1 contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            (mkRoleTokens [("Role", wallet2)])
            emptyMarloweTransactionMetadata
            (Just 2_000_000)
            mempty
            ( Left $
                mkPay (Account $ Role "Role") ada (Constant 2_000_000) $
                  When
                    [ Case (Notify TrueObs) Close
                    ]
                    (utcTimeToPOSIXTime timeout1)
                    Close
            )
      submitCreate wallet1 payDepth1Created
      payDepth2AccountCreated <-
        expectRight "Failed to create pay to account depth 2 contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            (mkRoleTokens [("Role", wallet2)])
            emptyMarloweTransactionMetadata
            (Just 10_000_000)
            mempty
            ( Left $
                mkPay (Account $ Role "Role") ada (Constant 2_000_000) $
                  When
                    [ Case (Notify TrueObs) $
                        When
                          [ Case (Notify TrueObs) Close
                          ]
                          (utcTimeToPOSIXTime timeout2)
                          Close
                    ]
                    (utcTimeToPOSIXTime timeout1)
                    Close
            )
      submitCreate wallet1 payDepth2AccountCreated
      payDepth2PartyCreated <-
        expectRight "Failed to create pay to party depth 2 contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            (mkRoleTokens [("Role", wallet2)])
            emptyMarloweTransactionMetadata
            (Just 10_000_000)
            mempty
            ( Left $
                mkPay (Party $ Role "Role") ada (Constant 2_000_000) $
                  When
                    [ Case (Notify TrueObs) $
                        When
                          [ Case (Notify TrueObs) Close
                          ]
                          (utcTimeToPOSIXTime timeout2)
                          Close
                    ]
                    (utcTimeToPOSIXTime timeout1)
                    Close
            )
      submitCreate wallet1 payDepth2PartyCreated
      runtime <- ask
      liftIO =<< runConcurrently do
        payRoleAccountApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payRoleAccountCreated in contractId)
                emptyMarloweTransactionMetadata
                []
        payAddressAccountApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payAddressAccountCreated in contractId)
                emptyMarloweTransactionMetadata
                []
        payRolePartyApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payRolePartyCreated in contractId)
                emptyMarloweTransactionMetadata
                []
        payAddressPartyApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payAddressPartyCreated in contractId)
                emptyMarloweTransactionMetadata
                []
        payDepth1Applied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payDepth1Created in contractId)
                emptyMarloweTransactionMetadata
                [NormalInput INotify]
        payDepth2AccountApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payDepth2AccountCreated in contractId)
                emptyMarloweTransactionMetadata
                [NormalInput INotify]
        payDepth2PartyApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet1)
                (let ContractCreated _ ContractCreatedInEra{contractId} = payDepth2PartyCreated in contractId)
                emptyMarloweTransactionMetadata
                [NormalInput INotify]
        pure $ runTests (runtime, PayTestData{..})

whenSpec :: Spec
whenSpec = describe "When contracts" do
  whenTimeoutSpec
  whenEmptySpec
  whenNonEmptySpec
  merkleizedSpec
  multiInputsSpec

data TimeoutTestData = TimeoutTestData
  { depth1Created :: ContractCreated 'V1
  , depth1Applied :: InputsApplied 'V1
  , depth2InnerTimeoutCreated :: ContractCreated 'V1
  , depth2InnerTimeoutApplied :: InputsApplied 'V1
  , depth2Created :: ContractCreated 'V1
  , depth2Applied :: InputsApplied 'V1
  , startTime :: UTCTime
  }

whenTimeoutSpec :: Spec
whenTimeoutSpec = parallel $ describe "Timed out contracts" $ aroundAll setup do
  describe "Close continuation" do
    it "should contain no output" $ runAsIntegration \TimeoutTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure depth1Applied
      liftIO $ scriptOutput output `shouldBe` Nothing
    it "should not accept any otherwise valid inputs" $ runAsIntegration \TimeoutTestData{..} -> do
      ContractCreated _ ContractCreatedInEra{..} <- pure depth1Created
      wallet <- getGenesisWallet 0
      result <-
        applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput INotify]
      liftIO $
        result
          `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEApplyNoMatchError)
  describe "Timed out continuation" do
    it "should contain no output" $ runAsIntegration \TimeoutTestData{..} -> do
      InputsApplied _ InputsAppliedInEra{..} <- pure depth2InnerTimeoutApplied
      liftIO $ scriptOutput output `shouldBe` Nothing
  describe "Non-timed out continuation" do
    it "should contain the correct output" $ runAsIntegration \TimeoutTestData{..} -> do
      let ContractCreated _ ContractCreatedInEra{marloweScriptAddress, assets} = depth2Created
      InputsApplied _ InputsAppliedInEra{..} <- pure depth2Applied
      TransactionScriptOutput address assets' utxo' MarloweData{..} <- expectJust "Expected an output" $ scriptOutput output
      liftIO $ address `shouldBe` marloweScriptAddress
      liftIO $ assets' `shouldBe` assets
      liftIO $ utxo' `shouldBe` TxOutRef (fromCardanoTxId $ getTxId txBody) 1
      liftIO $
        marloweContract `shouldBe` When [] (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 200) startTime) Close
  where
    setup :: ActionWith (MarloweRuntime, TimeoutTestData) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      startTime <- liftIO getCurrentTime
      wallet <- getGenesisWallet 0
      depth1Created <-
        expectRight "Failed to create depth1 contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            (Left $ When [Case (Notify TrueObs) Close] (utcTimeToPOSIXTime startTime) Close)
      submitCreate wallet depth1Created
      depth2InnerTimeoutCreated <-
        expectRight "Failed to create depth 2 contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            ( Left $
                When [Case (Notify TrueObs) Close] (utcTimeToPOSIXTime startTime) $
                  When [] (utcTimeToPOSIXTime startTime) Close
            )
      submitCreate wallet depth2InnerTimeoutCreated
      depth2Created <-
        expectRight "Failed to create depth 2 contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            ( Left $
                When [Case (Notify TrueObs) Close] (utcTimeToPOSIXTime startTime) $
                  When [] (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 200) startTime) Close
            )
      submitCreate wallet depth2Created
      runtime <- ask
      liftIO =<< runConcurrently do
        depth1Applied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet)
                (let ContractCreated _ ContractCreatedInEra{contractId} = depth1Created in contractId)
                emptyMarloweTransactionMetadata
                []
        depth2InnerTimeoutApplied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet)
                (let ContractCreated _ ContractCreatedInEra{contractId} = depth2InnerTimeoutCreated in contractId)
                emptyMarloweTransactionMetadata
                []
        depth2Applied <-
          Concurrently $
            expectRight "Failed to apply inputs"
              =<< applyInputs
                MarloweV1
                (addresses wallet)
                (let ContractCreated _ ContractCreatedInEra{contractId} = depth2Created in contractId)
                emptyMarloweTransactionMetadata
                []
        pure $ runTests (runtime, TimeoutTestData{..})

whenEmptySpec :: Spec
whenEmptySpec = parallel $ describe "Empty When contracts" $ aroundAll setup do
  it "should not accept empty inputs" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        []
    liftIO $
      result
        `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEUselessTransaction)
  it "should not accept a notify" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput INotify]
    liftIO $
      result
        `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEApplyNoMatchError)
  it "should not accept a deposit" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IDeposit (Role "Role") (Role "Role") ada 1_000_000]
    liftIO $
      result
        `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEApplyNoMatchError)
  it "should not accept a choice" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IChoice (ChoiceId "Choice" (Role "Role")) 0]
    liftIO $
      result
        `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEApplyNoMatchError)
  where
    setup :: ActionWith (MarloweRuntime, ContractId) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      startTime <- liftIO getCurrentTime
      wallet <- getGenesisWallet 0
      ContractCreated era ContractCreatedInEra{..} <-
        expectRight "Failed to create contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            (Left $ When [] (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 200) startTime) Close)
      submitCreate wallet $ ContractCreated era ContractCreatedInEra{..}
      runtime <- ask
      liftIO $ runTests (runtime, contractId)

whenNonEmptySpec :: Spec
whenNonEmptySpec = parallel $ describe "Non-Empty When contracts" $ aroundAll setup do
  it "should not accept empty inputs" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        []
    liftIO $
      result
        `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEUselessTransaction)
  it "should accept a notify" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput INotify]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should accept a Role1 deposit from wallet 1" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IDeposit (Role "Role1") (Role "Role1") ada 1_000_000]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject a Role2 deposit from wallet 1" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IDeposit (Role "Role2") (Role "Role2") ada 1_000_000]
    liftIO $ result `shouldBe` Left (ApplyInputsConstraintError $ RoleTokenNotFound $ AssetId rolesCurrency "Role2")
  it "should reject wallet3's deposit from wallet 1" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId}) -> do
    wallet <- getGenesisWallet 0
    wallet3 <- getGenesisWallet 2
    let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet3
    InputsApplied era InputsAppliedInEra{txBody} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IDeposit address address ada 1_000_000]
    SubmitFailed msg <- expectLeft "Expected a failure" =<< submit' wallet era txBody
    liftIO $ msg `shouldContain` "MissingVKeyWitnessesUTXOW"
  it "should accept a Role2 deposit from wallet 2" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 1
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IDeposit (Role "Role2") (Role "Role2") ada 1_000_000]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject a Role1 deposit from wallet 2" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 1
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IDeposit (Role "Role1") (Role "Role1") ada 1_000_000]
    liftIO $ result `shouldBe` Left (ApplyInputsConstraintError $ RoleTokenNotFound $ AssetId rolesCurrency "Role1")
  it "should reject wallet3's deposit from wallet 2" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId}) -> do
    wallet <- getGenesisWallet 1
    wallet3 <- getGenesisWallet 2
    let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet3
    InputsApplied era InputsAppliedInEra{txBody} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IDeposit address address ada 1_000_000]
    SubmitFailed msg <- expectLeft "Expected a failure" =<< submit' wallet era txBody
    liftIO $ msg `shouldContain` "MissingVKeyWitnessesUTXOW"
  it "should accept wallet3's deposit from wallet 3" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId}) -> do
    wallet <- getGenesisWallet 2
    let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IDeposit address address ada 1_000_000]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should accept a Role1 deposit from wallet 3" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 2
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IDeposit (Role "Role1") (Role "Role1") ada 1_000_000]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject a Role2 deposit from wallet 3" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 2
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IDeposit (Role "Role2") (Role "Role2") ada 1_000_000]
    liftIO $ result `shouldBe` Left (ApplyInputsConstraintError $ RoleTokenNotFound $ AssetId rolesCurrency "Role2")
  it "should accept a Role1 choice from wallet 1" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IChoice (ChoiceId "choice1" (Role "Role1")) 0]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject a Role2 choice from wallet 1" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IChoice (ChoiceId "choice2" (Role "Role2")) 0]
    liftIO $ result `shouldBe` Left (ApplyInputsConstraintError $ RoleTokenNotFound $ AssetId rolesCurrency "Role2")
  it "should reject wallet3's choice from wallet 1" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId}) -> do
    wallet <- getGenesisWallet 0
    wallet3 <- getGenesisWallet 2
    let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet3
    InputsApplied era InputsAppliedInEra{txBody} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IChoice (ChoiceId "choice3" address) 0]
    SubmitFailed msg <- expectLeft "Expected a failure" =<< submit' wallet era txBody
    liftIO $ msg `shouldContain` "MissingVKeyWitnessesUTXOW"
  it "should accept a Role2 choice from wallet 2" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 1
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IChoice (ChoiceId "choice2" (Role "Role2")) 0]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject a Role1 choice from wallet 2" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 1
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IChoice (ChoiceId "choice1" (Role "Role1")) 0]
    liftIO $ result `shouldBe` Left (ApplyInputsConstraintError $ RoleTokenNotFound $ AssetId rolesCurrency "Role1")
  it "should reject wallet3's choice from wallet 2" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId}) -> do
    wallet <- getGenesisWallet 1
    wallet3 <- getGenesisWallet 2
    let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet3
    InputsApplied era InputsAppliedInEra{txBody} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IChoice (ChoiceId "choice3" address) 0]
    SubmitFailed msg <- expectLeft "Expected a failure" =<< submit' wallet era txBody
    liftIO $ msg `shouldContain` "MissingVKeyWitnessesUTXOW"
  it "should accept the correct choice from wallet 3" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{contractId}) -> do
    wallet <- getGenesisWallet 2
    let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IChoice (ChoiceId "choice3" address) 0]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should accept a Role1 choice from wallet 3" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 2
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [NormalInput $ IChoice (ChoiceId "choice1" (Role "Role1")) 0]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject a Role2 choice from wallet 3" $ runAsIntegration \(ContractCreated _ ContractCreatedInEra{..}) -> do
    wallet <- getGenesisWallet 2
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [NormalInput $ IChoice (ChoiceId "choice2" (Role "Role2")) 0]
    liftIO $ result `shouldBe` Left (ApplyInputsConstraintError $ RoleTokenNotFound $ AssetId rolesCurrency "Role2")
  where
    setup :: ActionWith (MarloweRuntime, ContractCreated 'V1) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      startTime <- liftIO getCurrentTime
      wallet1 <- getGenesisWallet 0
      wallet2 <- getGenesisWallet 1
      wallet3 <- getGenesisWallet 2
      let address = Types.Address Address.testnet $ fromJust $ toPlutusAddress $ changeAddress $ addresses wallet3
          deposit1 = Deposit (Role "Role1") (Role "Role1") ada (Constant 1_000_000)
          deposit2 = Deposit (Role "Role2") (Role "Role2") ada (Constant 1_000_000)
          deposit3 = Deposit address address ada (Constant 1_000_000)
          choice1 = Choice (ChoiceId "choice1" (Role "Role1")) [Bound 0 0]
          choice2 = Choice (ChoiceId "choice2" (Role "Role2")) [Bound 0 0]
          choice3 = Choice (ChoiceId "choice3" address) [Bound 0 0]
          notify1 = Notify FalseObs
          notify2 = Notify TrueObs
          cases =
            [ Case deposit1 Close
            , Case choice1 Close
            , Case deposit2 Close
            , Case deposit3 Close
            , Case notify1 (When [] (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 200) startTime) Close)
            , Case notify2 Close
            , Case choice2 Close
            , Case choice3 Close
            ]
      contract <-
        expectRight "Failed to create contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet1)
            Nothing
            ( mkRoleTokens
                [ ("Role1", wallet1)
                , ("Role1", wallet3)
                , ("Role2", wallet2)
                ]
            )
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            (Left $ When cases (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 100) startTime) Close)
      submitCreate wallet1 contract
      runtime <- ask
      liftIO $ runTests (runtime, contract)

merkleizedSpec :: Spec
merkleizedSpec = parallel $ describe "Merkleized contracts" $ aroundAll setup do
  it "should accept an input with the correct continuation and hash" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [MerkleizedInput INotify hash Close]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject an input with an incorrect hash" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [MerkleizedInput INotify wrongHash Close]
    liftIO $
      result `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEHashMismatch)
  it "should reject an input with an incorrect continuation" $ runAsIntegration \contractId -> do
    wallet <- getGenesisWallet 0
    ApplyInputsConstraintError (BalancingError msg) <-
      expectLeft "Expected a failure"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [MerkleizedInput INotify hash $ If TrueObs Close Close]
    liftIO $ msg `shouldContain` "TxBodyScriptExecutionError"
  where
    hash = dataHash Close
    wrongHash = dataHash $ If TrueObs Close Close

    setup :: ActionWith (MarloweRuntime, ContractId) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      startTime <- liftIO getCurrentTime
      wallet <- getGenesisWallet 0
      contract <-
        expectRight "Failed to create contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            RoleTokensNone
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            ( Left $
                When
                  [MerkleizedCase (Notify TrueObs) hash]
                  (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 100) startTime)
                  Close
            )
      submitCreate wallet contract
      runtime <- ask
      liftIO $ runTests (runtime, let ContractCreated _ ContractCreatedInEra{contractId} = contract in contractId)

multiInputsSpec :: Spec
multiInputsSpec = parallel $ describe "Multi inputs" $ aroundAll setup do
  it "should accept one input" $ runAsIntegration \(startTime, contractId) -> do
    wallet <- getGenesisWallet 0
    InputsApplied _ InputsAppliedInEra{output} <- deposit wallet contractId (Role "role") (Role "role") ada 1_000_000
    TransactionScriptOutput{..} <- expectJust "Expected an output" $ scriptOutput output
    liftIO $
      marloweContract datum
        `shouldBe` When
          [Case (Choice (ChoiceId "choice" (Role "role")) [Bound 0 0]) Close]
          (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 100) startTime)
          Close
  it "should accept two inputs" $ runAsIntegration \(_, contractId) -> do
    wallet <- getGenesisWallet 0
    InputsApplied _ InputsAppliedInEra{output} <-
      expectRight "Failed to apply inputs"
        =<< applyInputs
          MarloweV1
          (addresses wallet)
          contractId
          emptyMarloweTransactionMetadata
          [ NormalInput $ IDeposit (Role "role") (Role "role") ada 1_000_000
          , NormalInput $ IChoice (ChoiceId "choice" (Role "role")) 0
          ]
    liftIO $ scriptOutput output `shouldBe` Nothing
  it "should reject an invalid second input" $ runAsIntegration \(_, contractId) -> do
    wallet <- getGenesisWallet 0
    result <-
      applyInputs
        MarloweV1
        (addresses wallet)
        contractId
        emptyMarloweTransactionMetadata
        [ NormalInput $ IDeposit (Role "role") (Role "role") ada 1_000_000
        , NormalInput $ IDeposit (Role "role") (Role "role") ada 1_000_000
        ]
    liftIO $
      result
        `shouldBe` Left (ApplyInputsConstraintsBuildupFailed $ MarloweComputeTransactionFailed Semantics.TEApplyNoMatchError)
  where
    setup :: ActionWith (MarloweRuntime, (UTCTime, ContractId)) -> IO ()
    setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
      startTime <- liftIO getCurrentTime
      wallet <- getGenesisWallet 0
      let timeout = utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 100) startTime
          action1 = Deposit (Role "role") (Role "role") ada (Constant 1_000_000)
          action2 = Choice (ChoiceId "choice" (Role "role")) [Bound 0 0]
      contract <-
        expectRight "Failed to create contract"
          =<< createContract
            Nothing
            MarloweV1
            (addresses wallet)
            Nothing
            (mkRoleTokens [("role", wallet)])
            emptyMarloweTransactionMetadata
            Nothing
            mempty
            ( Left $
                When
                  [Case action1 $ When [Case action2 Close] timeout Close, Case action2 $ When [Case action1 Close] timeout Close]
                  timeout
                  Close
            )
      submitCreate wallet contract
      runtime <- ask
      liftIO $
        runTests (runtime, let ContractCreated _ ContractCreatedInEra{contractId} = contract in (startTime, contractId))

utcTimeToPOSIXTime :: UTCTime -> POSIXTime
utcTimeToPOSIXTime = POSIXTime . floor . (* 1000) . utcTimeToPOSIXSeconds

mkRoleTokens :: [(TokenName, Wallet)] -> RoleTokensConfig
mkRoleTokens =
  RoleTokensMint
    . mkMint
    . fmap (\(token, Wallet{..}) -> (token, Nothing, ToAddress $ changeAddress addresses, 1))
    . NE.fromList

submitCreate :: Wallet -> ContractCreated 'V1 -> Integration ()
submitCreate wallet (ContractCreated era ContractCreatedInEra{..}) = void $ submit wallet era txBody

runAsIntegration :: (a -> Integration ()) -> ActionWith (MarloweRuntime, a)
runAsIntegration action (runtime, a) = runIntegrationTest (action a) runtime
