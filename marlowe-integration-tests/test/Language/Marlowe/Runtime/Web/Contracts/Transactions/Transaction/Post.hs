module Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.Post where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor (void)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Language.Marlowe.Core.V1.Semantics.Types (Input (NormalInput), InputContent (IDeposit))
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web (ContractOrSourceId (..), RoleTokenConfig (RoleTokenSimple))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (postContract, postTransaction)
import Language.Marlowe.Runtime.Web.Common (submitContract)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO (toDTO))
import Test.Hspec (Spec, describe, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "POST /contracts/{contractId}/transactions" do
  it "returns the transaction header" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1

    result <- runWebClient do
      let partyAWalletAddresses = addresses partyAWallet
      let partyAWebChangeAddress = toDTO $ changeAddress partyAWalletAddresses
      let partyAWebExtraAddresses = Set.map toDTO $ extraAddresses partyAWalletAddresses
      let partyAWebCollateralUtxos = Set.map toDTO $ collateralUtxos partyAWalletAddresses

      let partyBWalletAddresses = addresses partyBWallet

      partyBAddress <-
        liftIO $ expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress partyBWalletAddresses
      now <- liftIO getCurrentTime

      let (contract, partyA, _) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

      contractCreated@Web.CreateTxEnvelope{contractId} <-
        postContract
          Nothing
          partyAWebChangeAddress
          (Just partyAWebExtraAddresses)
          (Just partyAWebCollateralUtxos)
          Web.PostContractsRequest
            { metadata = mempty
            , version = Web.V1
            , roles = Just $ Web.Mint $ Map.singleton "Party A" $ RoleTokenSimple partyAWebChangeAddress
            , contract = ContractOrSourceId $ Left contract
            , minUTxODeposit = 2_000_000
            , tags = mempty
            }

      _ <- submitContract partyAWallet contractCreated

      let inputs = [NormalInput $ IDeposit partyA partyA ada 100_000_000]

      void $
        postTransaction
          partyAWebChangeAddress
          (Just partyAWebExtraAddresses)
          (Just partyAWebCollateralUtxos)
          contractId
          Web.PostTransactionsRequest
            { version = Web.V1
            , metadata = mempty
            , invalidBefore = Nothing
            , invalidHereafter = Nothing
            , inputs
            , tags = mempty
            }
    case result of
      Left _ -> fail $ "Expected 200 response code - got " <> show result
      Right _ -> pure ()
