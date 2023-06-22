module Language.Marlowe.Runtime.Web.Withdrawal.Put where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (postWithdrawal, putWithdrawal)
import Language.Marlowe.Runtime.Web.Common (signShelleyTransaction')
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract
  ( StandardContractChoiceMade(..)
  , StandardContractClosed(..)
  , StandardContractFundsDeposited(..)
  , StandardContractInit(..)
  , StandardContractNotified(..)
  , createStandardContract
  )
import Test.Hspec (Spec, describe, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "PUT /contracts/{contractId}/withdrawals/{withdrawalId}" do
  it "successfully submits a withdrawal" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet@Wallet{signingKeys} <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1

    result <- runWebClient do
      let WalletAddresses{..} = addresses partyAWallet
      let webChangeAddress = toDTO changeAddress
      let webExtraAddresses = Set.map toDTO extraAddresses
      let webCollataralUtxos = Set.map toDTO collateralUtxos
      StandardContractInit{contractCreated, makeInitialDeposit} <- createStandardContract partyAWallet partyBWallet
      StandardContractFundsDeposited{chooseGimmeTheMoney} <- makeInitialDeposit
      StandardContractChoiceMade{sendNotify} <- chooseGimmeTheMoney
      StandardContractNotified{makeReturnDeposit} <- sendNotify
      StandardContractClosed{} <- makeReturnDeposit

      contractId <- case contractCreated of
        Web.CreateTxEnvelope{contractId} -> pure contractId

      Web.WithdrawTxEnvelope{withdrawalId, txEnvelope} <- postWithdrawal
        webChangeAddress
        (Just webExtraAddresses)
        (Just webCollataralUtxos)
        Web.PostWithdrawalsRequest
          { role = "Party A"
          , contractId
          }
      signedWithdrawalTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
      putWithdrawal withdrawalId signedWithdrawalTx

    case result of
      Left _ ->  fail $ "Expected 200 response code - got " <> show result
      Right () ->  pure ()
