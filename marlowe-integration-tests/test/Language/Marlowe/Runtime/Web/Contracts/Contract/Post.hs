{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Contracts.Contract.Post where

import Control.Monad.IO.Class (MonadIO (liftIO))

import Cardano.Api (
  AsType (..),
  TxBody (..),
  TxBodyContent (..),
  TxMetadata (TxMetadata),
  TxMetadataInEra (..),
  TxMetadataSupportedInEra (TxMetadataInBabbageEra),
  TxMetadataValue (..),
  deserialiseFromTextEnvelope,
 )
import Data.Aeson (Value (String))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Functor (void)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Integration.ApplyInputs (utcTimeToPOSIXTime)
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web (ContractOrSourceId (..), CreateTxEnvelope (..))
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (postContract)
import Language.Marlowe.Runtime.Web.Server.DTO (FromDTO (..), ToDTO (toDTO))
import Network.URI (parseURI)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "POST /contracts" do
  it "returns the contract header"
    . specWithRolesConfig Nothing
    $ Web.Mint . Map.singleton "PartyA" . flip Web.RoleTokenConfig Nothing . flip Map.singleton 1 . Web.ClosedRole
  it "returns the contract header (open roles)"
    . specWithRolesConfig (Just "Thread")
    . const
    . Web.Mint
    $ Map.fromList
      [ ("PartyA", Web.RoleTokenConfig (Map.singleton Web.OpenRole 1) Nothing)
      ]
  bugPLT8712
  bugPLT8713

specWithRolesConfig :: Maybe Text -> (Web.Address -> Web.RolesConfig) -> IO ()
specWithRolesConfig threadTokenName roles =
  withLocalMarloweRuntime $ runIntegrationTest do
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
      let (contract, _, _) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

      void $
        postContract
          Nothing
          partyAWebChangeAddress
          (Just partyAWebExtraAddresses)
          (Just partyAWebCollateralUtxos)
          Web.PostContractsRequest
            { metadata = mempty
            , version = Web.V1
            , threadTokenName
            , roles = Just $ roles partyAWebChangeAddress
            , contract = ContractOrSourceId $ Left contract
            , accounts = mempty
            , minUTxODeposit = Nothing
            , tags = mempty
            }
    case result of
      Left _ -> fail $ "Expected 200 response code - got " <> show result
      Right _ -> pure ()

bugPLT8712 :: Spec
bugPLT8712 = do
  describe "[BUG] PLT-8712: Runtime drops field from minting metadata" do
    it "Marlowe Runtime supports additional properties" $ withLocalMarloweRuntime $ runIntegrationTest do
      wallet <- getGenesisWallet 0
      either (fail . show) pure =<< runWebClient do
        let walletAddress = toDTO $ changeAddress $ addresses wallet
        CreateTxEnvelope{..} <-
          postContract
            Nothing
            walletAddress
            Nothing
            Nothing
            Web.PostContractsRequest
              { metadata = mempty
              , version = Web.V1
              , threadTokenName = Nothing
              , roles =
                  Just $
                    Web.Mint $
                      Map.singleton "Test Role" $
                        Web.RoleTokenConfig
                          { recipients = Map.singleton Web.OpenRole 1
                          , metadata =
                              Just
                                Web.TokenMetadata
                                  { name = "Name"
                                  , image = fromJust $ parseURI "https://example.com"
                                  , mediaType = Just "image/png"
                                  , description = Just "Test description"
                                  , files = Nothing
                                  , additionalProps =
                                      KeyMap.fromList
                                        [ (Key.fromText "url", String "https://example.com")
                                        ]
                                  }
                          }
              , contract = ContractOrSourceId $ Left V1.Close
              , accounts = mempty
              , minUTxODeposit = Nothing
              , tags = mempty
              }
        liftIO do
          textEnvelope <- expectJust "Failed to convert text envelope" $ fromDTO txEnvelope
          TxBody TxBodyContent{..} <-
            expectRight "Failed to deserialise tx body" $
              deserialiseFromTextEnvelope (AsTxBody AsBabbageEra) textEnvelope
          case txMetadata of
            TxMetadataNone -> fail "expected metadata"
            TxMetadataInEra TxMetadataInBabbageEra (TxMetadata m) -> do
              TxMetaMap [(TxMetaBytes _, tokenMetadata)] <- expectJust "Failed to lookup metadata" $ Map.lookup 721 m
              let expected =
                    TxMetaMap
                      [
                        ( TxMetaBytes "Test Role"
                        , TxMetaMap
                            [ (TxMetaText "name", TxMetaText "Name")
                            , (TxMetaText "image", TxMetaList [TxMetaText "https://example.com"])
                            , (TxMetaText "mediaType", TxMetaText "image/png")
                            , (TxMetaText "description", TxMetaList [TxMetaText "Test description"])
                            , (TxMetaText "url", TxMetaText "https://example.com")
                            ]
                        )
                      ]
              tokenMetadata `shouldBe` expected

bugPLT8713 :: Spec
bugPLT8713 = do
  describe "[BUG] PLT-8713: Open-role input application fails when contract has stake address" do
    it "Marlowe Runtime supports stake addresses for open role contracts" $ withLocalMarloweRuntime $ runIntegrationTest do
      wallet <- getGenesisWallet 0
      now <- liftIO getCurrentTime
      either (fail . show) pure =<< runWebClient do
        let walletAddress = toDTO $ changeAddress $ addresses wallet
        let contract =
              V1.When
                [ V1.Case
                    (V1.Deposit (V1.Role "Test Role") (V1.Role "Test Role") (V1.Token "" "") (V1.Constant 1))
                    ( V1.When
                        [ V1.Case (V1.Notify V1.TrueObs) V1.Close
                        ]
                        (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 1000) now)
                        V1.Close
                    )
                ]
                (utcTimeToPOSIXTime $ addUTCTime (secondsToNominalDiffTime 1000) now)
                V1.Close
        CreateTxEnvelope{..} <-
          postContract
            (Just $ Web.StakeAddress "stake_test1uqx07tvec6fuff78s7v456fx94ukmpvh4x6tynjhmqwta8cg0kx6f")
            walletAddress
            Nothing
            Nothing
            Web.PostContractsRequest
              { metadata = mempty
              , version = Web.V1
              , threadTokenName = Nothing
              , roles =
                  Just $
                    Web.Mint $
                      Map.singleton "Test Role" $
                        Web.RoleTokenConfig
                          { recipients = Map.singleton Web.OpenRole 1
                          , metadata = Nothing
                          }
              , contract = ContractOrSourceId $ Left contract
              , accounts = mempty
              , minUTxODeposit = Nothing
              , tags = mempty
              }
        liftIO $ safetyErrors `shouldBe` []
