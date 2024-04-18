{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Integration.Withdraw (spec) where

import Cardano.Api (BabbageEraOnwards (..), getTxId)
import Control.Arrow ((&&&))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Marlowe.Class (withdraw)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (PayoutState (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (TxIx (..), TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api (
  ContractId (unContractId),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (..),
  TransactionOutput (..),
  toChainDatum,
 )
import Language.Marlowe.Runtime.Integration.Common (
  Integration,
  Wallet (..),
  expectRight,
  getGenesisWallet,
  runIntegrationTest,
 )
import Language.Marlowe.Runtime.Integration.StandardContract (
  StandardContractChoiceMade (sendNotify),
  StandardContractClosed (
    StandardContractClosed,
    returnDeposited,
    withdrawPartyAFunds
  ),
  StandardContractFundsDeposited (chooseGimmeTheMoney),
  StandardContractLifecycleInit (
    StandardContractLifecycleInit,
    contractCreated,
    createdBlock,
    makeInitialDeposit
  ),
  StandardContractNotified (makeReturnDeposit),
  createStandardContract,
 )
import Language.Marlowe.Runtime.Transaction.Api (
  ConstraintError (..),
  ContractCreated (..),
  ContractCreatedInEra (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (WithdrawTx),
  WithdrawTxInEra (..),
 )
import Test.Hspec (
  ActionWith,
  Spec,
  aroundAll,
  describe,
  it,
  shouldBe,
 )
import Test.Integration.Marlowe.Local (MarloweRuntime, withLocalMarloweRuntime)

spec :: Spec
spec = describe "Withdraw" $ aroundAll setup do
  it "Fails on empty payouts" noPayoutsTest
  it "Fails on non-payout script outputs" nonPayoutTest
  it "Fails on non-script outputs" nonScriptPayoutTest
  it "Fails on made-up payouts" nonExistentPayoutTest
  it "Fails on withdrawn payouts" withdrawnPayoutTest
  it "Fails when the wallet doesn't have the necessary role token" missingRoleTokenTest
  it "Withdraws 1 payout from 1 contract" singlePayoutTest
  it "Withdraws 2 payouts from 2 contracts" multiPayoutTest
  it "Withdraws 3 payouts from 3 contracts for 2 wallets" multiPayoutMultiWalletTest

noPayoutsTest :: ActionWith TestData
noPayoutsTest TestData{..} = flip runIntegrationTest runtime do
  result <- withdraw MarloweV1 (addresses wallet1) mempty
  liftIO $ result `shouldBe` Left EmptyPayouts

nonPayoutTest :: ActionWith TestData
nonPayoutTest TestData{..} = flip runIntegrationTest runtime do
  ContractCreated _ ContractCreatedInEra{..} <- pure randomCreation
  let fakePayout = unContractId contractId
  let realPayout = payoutId wallet1AvailablePayout1
  result <- withdraw MarloweV1 (addresses wallet1) $ Set.fromList [fakePayout, realPayout]
  liftIO $
    result `shouldBe` Left (WithdrawConstraintError $ InvalidPayoutDatum fakePayout $ Just $ toChainDatum MarloweV1 datum)

nonScriptPayoutTest :: ActionWith TestData
nonScriptPayoutTest TestData{..} = flip runIntegrationTest runtime do
  ContractCreated _ ContractCreatedInEra{..} <- pure randomCreation
  let fakePayout = (unContractId contractId){txIx = TxIx 0}
  let realPayout = payoutId wallet1AvailablePayout1
  result <- withdraw MarloweV1 (addresses wallet1) $ Set.fromList [fakePayout, realPayout]
  liftIO $
    result `shouldBe` Left (WithdrawConstraintError $ InvalidPayoutDatum fakePayout Nothing)

nonExistentPayoutTest :: ActionWith TestData
nonExistentPayoutTest TestData{..} = flip runIntegrationTest runtime do
  let fakePayout = TxOutRef "0000000000000000000000000000000000000000000000000000000000000000" (TxIx 0)
  let realPayout = payoutId wallet1AvailablePayout1
  result <- withdraw MarloweV1 (addresses wallet1) $ Set.fromList [fakePayout, realPayout]
  liftIO $ result `shouldBe` Left (WithdrawConstraintError $ PayoutNotFound fakePayout)

withdrawnPayoutTest :: ActionWith TestData
withdrawnPayoutTest TestData{..} = flip runIntegrationTest runtime do
  result <-
    withdraw MarloweV1 (addresses wallet1) $
      Set.fromList $
        payoutId <$> [wallet1AvailablePayout1, wallet1AvailablePayout2, wallet1WithdrawnPayout]
  liftIO $ result `shouldBe` Left (WithdrawConstraintError $ PayoutNotFound $ payoutId wallet1WithdrawnPayout)

missingRoleTokenTest :: ActionWith TestData
missingRoleTokenTest TestData{..} = flip runIntegrationTest runtime do
  result <-
    withdraw MarloweV1 (addresses wallet1) $
      Set.fromList $
        payoutId <$> [wallet1AvailablePayout1, wallet1AvailablePayout2, wallet2AvailablePayout]
  liftIO $
    result
      `shouldBe` Left (WithdrawConstraintError $ RoleTokenNotFound case payout wallet2AvailablePayout of Payout{..} -> datum)

singlePayoutTest :: ActionWith TestData
singlePayoutTest TestData{..} = flip runIntegrationTest runtime do
  WithdrawTx BabbageEraOnwardsBabbage WithdrawTxInEra{..} <-
    expectRight "expected withdraw to succeed"
      =<< withdraw MarloweV1 (addresses wallet1) (Set.singleton $ payoutId wallet1AvailablePayout1)
  let expectedInputs = Map.singleton (payoutId wallet1AvailablePayout1) (payout wallet1AvailablePayout1)
  liftIO $ inputs `shouldBe` expectedInputs

multiPayoutTest :: ActionWith TestData
multiPayoutTest TestData{..} = flip runIntegrationTest runtime do
  WithdrawTx BabbageEraOnwardsBabbage WithdrawTxInEra{..} <-
    expectRight "expected withdraw to succeed"
      =<< withdraw
        MarloweV1
        (addresses wallet1)
        (Set.fromList $ payoutId <$> [wallet1AvailablePayout1, wallet1AvailablePayout2])
  let expectedInputs = Map.fromList $ (payoutId &&& payout) <$> [wallet1AvailablePayout1, wallet1AvailablePayout2]
  liftIO $ inputs `shouldBe` expectedInputs

multiPayoutMultiWalletTest :: ActionWith TestData
multiPayoutMultiWalletTest TestData{..} = flip runIntegrationTest runtime do
  let wallet1Addresses = addresses wallet1
  let wallet2Addresses = addresses wallet2
  WithdrawTx BabbageEraOnwardsBabbage WithdrawTxInEra{..} <-
    expectRight "expected withdraw to succeed"
      =<< withdraw
        MarloweV1
        wallet1Addresses
          { extraAddresses =
              Set.insert
                (changeAddress wallet2Addresses)
                (extraAddresses wallet1Addresses <> extraAddresses wallet2Addresses)
          }
        (Set.fromList $ payoutId <$> [wallet1AvailablePayout1, wallet1AvailablePayout2, wallet2AvailablePayout])
  let expectedInputs =
        Map.fromList $
          (payoutId &&& payout) <$> [wallet1AvailablePayout1, wallet1AvailablePayout2, wallet2AvailablePayout]
  liftIO $ inputs `shouldBe` expectedInputs

data TestData = TestData
  { wallet1AvailablePayout1 :: PayoutState 'V1
  , wallet1AvailablePayout2 :: PayoutState 'V1
  , wallet1WithdrawnPayout :: PayoutState 'V1
  , wallet2AvailablePayout :: PayoutState 'V1
  , randomCreation :: ContractCreated 'V1
  , wallet1 :: Wallet
  , wallet2 :: Wallet
  , runtime :: MarloweRuntime
  }

setup :: ActionWith TestData -> IO ()
setup runTests = withLocalMarloweRuntime $ runIntegrationTest do
  runtime <- ask
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 2
  (wallet1AvailablePayout1, wallet1AvailablePayout2, wallet1WithdrawnPayout) <- setupPayments wallet1 wallet2
  wallet2AvailablePayout <- createAndExecuteStandardContractWithoutWithdrawing wallet2 wallet1
  StandardContractLifecycleInit{contractCreated = randomCreation} <- createStandardContract wallet1 wallet2
  liftIO $ runTests TestData{..}

setupPayments :: Wallet -> Wallet -> Integration (PayoutState 'V1, PayoutState 'V1, PayoutState 'V1)
setupPayments partyA partyB = do
  (,,)
    <$> createAndExecuteStandardContractWithoutWithdrawing partyA partyB
    <*> createAndExecuteStandardContractWithoutWithdrawing partyA partyB
    <*> createAndExecuteStandardContract partyA partyB

createAndExecuteStandardContractWithoutWithdrawing :: Wallet -> Wallet -> Integration (PayoutState 'V1)
createAndExecuteStandardContractWithoutWithdrawing partyA partyB = do
  StandardContractLifecycleInit{..} <- createStandardContract partyA partyB
  ContractCreated _ ContractCreatedInEra{..} <- pure contractCreated
  step2 <- makeInitialDeposit
  step3 <- chooseGimmeTheMoney step2
  step4 <- sendNotify step3
  StandardContractClosed{returnDeposited = InputsApplied _ InputsAppliedInEra{output}} <- makeReturnDeposit step4
  case Map.toList $ payouts output of
    [(payoutId, payout)] ->
      pure
        PayoutState
          { contractId
          , payoutId
          , withdrawalId = Nothing
          , payout
          }
    _ -> fail $ "Expected 1 payout, got: " <> show (payouts output)

createAndExecuteStandardContract :: Wallet -> Wallet -> Integration (PayoutState 'V1)
createAndExecuteStandardContract partyA partyB = do
  StandardContractLifecycleInit{..} <- createStandardContract partyA partyB
  ContractCreated _ ContractCreatedInEra{contractId} <- pure contractCreated
  step2 <- makeInitialDeposit
  step3 <- chooseGimmeTheMoney step2
  step4 <- sendNotify step3
  step5 <- makeReturnDeposit step4
  (WithdrawTx _ WithdrawTxInEra{inputs, txBody}, _) <- withdrawPartyAFunds step5
  case Map.toList inputs of
    [(payoutId, payout)] ->
      pure
        PayoutState
          { contractId
          , payoutId
          , withdrawalId = Just $ fromCardanoTxId $ getTxId txBody
          , payout
          }
    _ -> fail $ "Expected 1 payout, got: " <> show inputs
