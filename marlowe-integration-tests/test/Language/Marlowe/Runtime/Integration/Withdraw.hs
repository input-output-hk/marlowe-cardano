{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Integration.Withdraw where

import Cardano.Api.Shelley (
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (ReferenceTxInsScriptsInlineDatumsInBabbageEra),
 )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe.Class (withdraw)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), Assets (Assets))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), Payout (Payout), TransactionOutput (..))
import Language.Marlowe.Runtime.Integration.Common (Wallet (..), expectRight, getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (
  ConstraintError (RoleTokenNotFound),
  ContractCreatedInEra (..),
  InputsAppliedInEra (..),
  WithdrawError (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
 )
import Test.Hspec
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "Withdraw" do
  it "Withdraw missing role token" missingRoleTokenTest
  it "Withdraw no payouts" noPayoutsTest
  it "Withdraws funds" payoutsTest

missingRoleTokenTest :: IO ()
missingRoleTokenTest = withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  step1 <- createStandardContract wallet1 wallet2
  step2 <- makeInitialDeposit step1
  step3 <- chooseGimmeTheMoney step2
  step4 <- sendNotify step3
  StandardContractClosed{returnDeposited = InputsAppliedInEra{output}} <- makeReturnDeposit step4
  let TransactionOutput{payouts} = output
  result <- withdraw MarloweV1 (addresses wallet2) $ Map.keysSet payouts
  let policyId = case contractCreated step1 of ContractCreatedInEra{..} -> rolesCurrency
  liftIO $ result `shouldBe` Left (WithdrawConstraintError $ RoleTokenNotFound $ AssetId policyId "Party A")

noPayoutsTest :: IO ()
noPayoutsTest = withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  step1 <- createStandardContract wallet1 wallet2
  step2 <- makeInitialDeposit step1
  step3 <- chooseGimmeTheMoney step2
  _ <- sendNotify step3
  result <- withdraw MarloweV1 (addresses wallet1) mempty
  liftIO $ result `shouldBe` Left EmptyPayouts

payoutsTest :: IO ()
payoutsTest = withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  StandardContractInit{..} <- createStandardContract wallet1 wallet2
  let ContractCreatedInEra{..} = contractCreated
  step2 <- makeInitialDeposit
  step3 <- chooseGimmeTheMoney step2
  step4 <- sendNotify step3
  StandardContractClosed{returnDeposited = InputsAppliedInEra{output}} <- makeReturnDeposit step4
  let TransactionOutput{payouts} = output
  WithdrawTx ReferenceTxInsScriptsInlineDatumsInBabbageEra WithdrawTxInEra{inputs} <-
    expectRight "failed to withdraw payouts" =<< withdraw MarloweV1 (addresses wallet1) (Map.keysSet payouts)
  liftIO $
    Map.elems inputs `shouldBe` [Payout payoutScriptAddress (Assets 100_000_000 mempty) $ AssetId rolesCurrency "Party A"]
