module Language.Marlowe.Runtime.Integration.Withdraw where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe.Class (withdraw)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), Assets (Assets))
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), Payout (Payout))
import Language.Marlowe.Runtime.Integration.Common (Wallet (..), expectRight, getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (
  ConstraintError (RoleTokenNotFound),
  ContractCreated (..),
  WithdrawError (..),
  WithdrawTx (..),
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
  _ <- makeReturnDeposit step4
  result <- case contractCreated step1 of ContractCreated{..} -> withdraw MarloweV1 (addresses wallet2) contractId "Party A"
  let policyId = case contractCreated step1 of ContractCreated{..} -> rolesCurrency
  liftIO $ result `shouldBe` Left (WithdrawConstraintError $ RoleTokenNotFound $ AssetId policyId "Party A")

noPayoutsTest :: IO ()
noPayoutsTest = withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  step1 <- createStandardContract wallet1 wallet2
  step2 <- makeInitialDeposit step1
  step3 <- chooseGimmeTheMoney step2
  _ <- sendNotify step3
  result <- case contractCreated step1 of ContractCreated{..} -> withdraw MarloweV1 (addresses wallet1) contractId "Party A"
  liftIO $ result `shouldBe` Left (UnableToFindPayoutForAGivenRole "Party A")

payoutsTest :: IO ()
payoutsTest = withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  StandardContractInit{..} <- createStandardContract wallet1 wallet2
  let ContractCreated{..} = contractCreated
  step2 <- makeInitialDeposit
  step3 <- chooseGimmeTheMoney step2
  step4 <- sendNotify step3
  _ <- makeReturnDeposit step4
  WithdrawTx{inputs, roleToken = AssetId{..}} <-
    expectRight "failed to withdraw payouts" =<< withdraw MarloweV1 (addresses wallet1) contractId "Party A"
  liftIO $ tokenName `shouldBe` "Party A"
  liftIO $ Map.elems inputs `shouldBe` [Payout payoutScriptAddress (Assets 100_000_000 mempty) AssetId{..}]
