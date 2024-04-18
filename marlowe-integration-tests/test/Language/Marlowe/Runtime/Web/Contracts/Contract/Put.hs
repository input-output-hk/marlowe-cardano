module Language.Marlowe.Runtime.Web.Contracts.Contract.Put where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Language.Marlowe.Runtime.Integration.Common (
  Wallet (Wallet, addresses, signingKeys),
  expectJust,
  getGenesisWallet,
  runIntegrationTest,
  runWebClient,
 )
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Client (postContract, putContract)
import Language.Marlowe.Runtime.Web.Common (signShelleyTransaction')
import qualified Language.Marlowe.Runtime.Web.Core.MarloweVersion as Web

import Language.Marlowe.Runtime.Web.Contract.API (
  ContractOrSourceId (ContractOrSourceId),
  PostContractsRequest (
    accounts,
    contract,
    metadata,
    minUTxODeposit,
    roles,
    tags,
    threadTokenName,
    version
  ),
 )
import qualified Language.Marlowe.Runtime.Web.Contract.API as Web
import Language.Marlowe.Runtime.Web.Core.Roles (
  RoleTokenConfig (RoleTokenConfig),
  RoleTokenRecipient (ClosedRole),
 )
import qualified Language.Marlowe.Runtime.Web.Core.Roles as Web

import qualified Language.Marlowe.Runtime.Web.Tx.API as Web
import Test.Hspec (Spec, describe, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "PUT /contracts/{contractId}" do
  it "returns the transaction header" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet@Wallet{signingKeys} <- getGenesisWallet 0
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

      let (contract, _, _) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

      Web.CreateTxEnvelope{contractId, txEnvelope} <-
        postContract
          Nothing
          partyAWebChangeAddress
          (Just partyAWebExtraAddresses)
          (Just partyAWebCollateralUtxos)
          Web.PostContractsRequest
            { metadata = mempty
            , version = Web.V1
            , threadTokenName = Nothing
            , roles =
                Just
                  . Web.Mint
                  . Map.singleton "PartyA"
                  $ RoleTokenConfig (Map.singleton (ClosedRole partyAWebChangeAddress) 1) Nothing
            , contract = ContractOrSourceId $ Left contract
            , accounts = mempty
            , minUTxODeposit = Nothing
            , tags = mempty
            }
      signedCreateTx <- liftIO $ signShelleyTransaction' txEnvelope signingKeys
      putContract contractId signedCreateTx
    case result of
      Left _ -> fail $ "Expected 200 response code - got " <> show result
      Right () -> pure ()
