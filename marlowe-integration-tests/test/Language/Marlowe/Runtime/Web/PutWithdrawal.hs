module Language.Marlowe.Runtime.Web.PutWithdrawal
  where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Set as Set
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (postWithdrawal, putWithdrawal)
import Language.Marlowe.Runtime.Web.Common (MarloweWebTestData(..), setup, signShelleyTransaction')
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Language.Marlowe.Runtime.Web.StandardContract
  ( StandardContractChoiceMade(..)
  , StandardContractClosed(..)
  , StandardContractFundsDeposited(..)
  , StandardContractInit(..)
  , StandardContractNotified(..)
  , createStandardContract
  )
import Test.Hspec (Spec, SpecWith, aroundAll, describe, it)

spec :: Spec
spec = describe "PUT /contracts/{contractId}/withdrawals/{withdrawalId}" $ aroundAll setup do
  putContractWithdrawalValidSpec

putContractWithdrawalValidSpec :: SpecWith MarloweWebTestData
putContractWithdrawalValidSpec = describe "Valid PUT /contracts/{contractId}" do
  it "successfully submits a withdrawal" \MarloweWebTestData{..} -> flip runIntegrationTest runtime do
    let
      partyAWallet@Wallet{signingKeys} = wallet1
      partyBWallet = wallet2

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
