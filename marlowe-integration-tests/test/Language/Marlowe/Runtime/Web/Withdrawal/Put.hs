module Language.Marlowe.Runtime.Web.Withdrawal.Put where

import Language.Marlowe.Runtime.Integration.Common (
  getGenesisWallet,
  runIntegrationTest,
  runWebClient,
 )

import Language.Marlowe.Runtime.Web.StandardContract (
  StandardContractChoiceMade (..),
  StandardContractClosed (..),
  StandardContractFundsDeposited (..),
  StandardContractLifecycleInit (..),
  StandardContractNotified (..),
  createStandardContract,
 )

import Test.Hspec (Spec, describe, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "PUT /contracts/{contractId}/withdrawals/{withdrawalId}" do
  it "successfully submits a withdrawal" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1

    result <- runWebClient do
      StandardContractLifecycleInit{makeInitialDeposit} <- createStandardContract partyAWallet partyBWallet
      StandardContractFundsDeposited{chooseGimmeTheMoney} <- makeInitialDeposit
      StandardContractChoiceMade{sendNotify} <- chooseGimmeTheMoney
      StandardContractNotified{makeReturnDeposit} <- sendNotify
      StandardContractClosed{withdrawPartyAPayout} <- makeReturnDeposit
      _ <- withdrawPartyAPayout
      return ()

    case result of
      Left _ -> fail $ "Expected 200 response code - got " <> show result
      Right () -> pure ()
