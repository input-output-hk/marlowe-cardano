module Language.Marlowe.Runtime.Web.Withdrawal.Post where

import Data.Functor (void)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Integration.Common (
  Wallet (addresses),
  getGenesisWallet,
  runIntegrationTest,
  runWebClient,
 )
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))

import Language.Marlowe.Runtime.Web.Client (Page (..), getPayouts, postWithdrawal)

import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Payout.API (PayoutHeader (..), PayoutStatus (..))
import Language.Marlowe.Runtime.Web.StandardContract (
  StandardContractChoiceMade (..),
  StandardContractClosed (..),
  StandardContractFundsDeposited (..),
  StandardContractInit (..),
  StandardContractNotified (..),
  createStandardContract,
 )

import qualified Language.Marlowe.Runtime.Web.Tx.API as Web
import qualified Language.Marlowe.Runtime.Web.Withdrawal.API as Web
import Test.Hspec (Spec, describe, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "POST /contracts/{contractId}/withdrawal" do
  it "returns a withdraw TX Body" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1

    result <- runWebClient do
      let WalletAddresses{..} = addresses partyAWallet
      let webChangeAddress = toDTO changeAddress
      let webExtraAddresses = Set.map toDTO extraAddresses
      let webCollateralUtxos = Set.map toDTO collateralUtxos

      StandardContractInit{contractCreated, makeInitialDeposit} <- createStandardContract partyAWallet partyBWallet
      StandardContractFundsDeposited{chooseGimmeTheMoney} <- makeInitialDeposit
      StandardContractChoiceMade{sendNotify} <- chooseGimmeTheMoney
      StandardContractNotified{makeReturnDeposit} <- sendNotify
      StandardContractClosed{} <- makeReturnDeposit
      contractId <- case contractCreated of
        Web.CreateTxEnvelope{contractId} -> pure contractId
      Page{..} <- getPayouts (Just $ Set.singleton contractId) Nothing (Just Available) Nothing
      let payouts = Set.fromList $ payoutId <$> items
      void $
        postWithdrawal webChangeAddress (Just webExtraAddresses) (Just webCollateralUtxos) Web.PostWithdrawalsRequest{..}

    case result of
      Left _ -> fail $ "Expected 200 response code - got " <> show result
      Right _ -> pure ()
