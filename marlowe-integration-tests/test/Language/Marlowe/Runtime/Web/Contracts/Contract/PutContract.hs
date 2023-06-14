module Language.Marlowe.Runtime.Web.Contracts.Contract.PutContract where

import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import Language.Marlowe.Runtime.Web (RoleTokenConfig(RoleTokenSimple))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (postContract, putContract)
import Language.Marlowe.Runtime.Web.Common (signShelleyTransaction')
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import Test.Hspec (Spec, describe, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "POST /contracts/{contractId}/transactions" do
  it "returns the transaction header" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet@Wallet{signingKeys} <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1

    result <- runWebClient do
      let partyAWalletAddresses = addresses partyAWallet
      let partyAWebChangeAddress = toDTO $ changeAddress partyAWalletAddresses
      let partyAWebExtraAddresses = Set.map toDTO $ extraAddresses partyAWalletAddresses
      let partyAWebCollataralUtxos = Set.map toDTO $ collateralUtxos partyAWalletAddresses

      let partyBWalletAddresses = addresses partyBWallet

      partyBAddress <- liftIO $ expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress partyBWalletAddresses
      now <- liftIO getCurrentTime

      let (contract, _, _) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

      Web.CreateTxEnvelope{contractId, txEnvelope} <- postContract
        partyAWebChangeAddress
        (Just partyAWebExtraAddresses)
        (Just partyAWebCollataralUtxos)
        Web.PostContractsRequest
          { metadata = mempty
          , version = Web.V1
          , roles = Just $ Web.Mint $ Map.singleton "PartyA" $ RoleTokenSimple partyAWebChangeAddress
          , contract = contract
          , minUTxODeposit = 2_000_000
          , tags = mempty
          }
      signedCreateTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
      putContract contractId signedCreateTx
    case result of
      Left _ ->  fail $ "Expected 200 response code - got " <> show result
      Right () ->  pure ()
