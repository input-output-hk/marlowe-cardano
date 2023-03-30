module Language.Marlowe.Runtime.Web.PutWithdrawal
  where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Language.Marlowe.Runtime.Integration.Common
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (putWithdrawal)
import Language.Marlowe.Runtime.Web.Common (signShelleyTransaction')
import Language.Marlowe.Runtime.Web.StandardContract
  ( StandardContractChoiceMade(..)
  , StandardContractClosed(..)
  , StandardContractFundsDeposited(..)
  , StandardContractInit(..)
  , StandardContractNotified(..)
  , createStandardContract
  )
import Test.Hspec (Spec, describe, focus, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = focus $ describe "PUT /contracts/{contractId}/withdrawals/{withdrawalId}" do
  it "processes a withdrawal successfully with a valid Withdrawal Id" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet@Wallet{signingKeys} <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1

    result <- runWebClient do
      StandardContractInit{makeInitialDeposit} <- createStandardContract partyAWallet partyBWallet
      StandardContractFundsDeposited{chooseGimmeTheMoney} <- makeInitialDeposit
      StandardContractChoiceMade{sendNotify} <- chooseGimmeTheMoney
      StandardContractNotified{makeReturnDeposit} <- sendNotify
      StandardContractClosed{withdrawPartyAFunds} <- makeReturnDeposit

      (Web.WithdrawTxBody{txBody = withdrawTxBody, withdrawalId}, _) <- withdrawPartyAFunds
      signedWithdrawalTx <- liftIO $ signShelleyTransaction' withdrawTxBody signingKeys
      putWithdrawal withdrawalId signedWithdrawalTx
    case result of
      Left _ ->  fail $ "Expected 200 response code - got " <> show result
      Right () ->  pure ()
