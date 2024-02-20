{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Contracts.Contract.Post where

import Control.Monad.IO.Class (MonadIO (liftIO))

import Cardano.Api (
  AsType (..),
  ShelleyBasedEra (ShelleyBasedEraBabbage),
  TxBody (..),
  TxBodyContent (..),
  TxMetadata (TxMetadata),
  TxMetadataInEra (..),
  TxMetadataValue (..),
  deserialiseFromTextEnvelope,
 )
import Control.Error (note)
import Data.Aeson (Value (String), (.:))
import qualified Data.Aeson as A
import Data.Aeson.Decoding (decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import Data.Functor (void, (<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..))
import Language.Marlowe.Core.V1.Plate (extractAll)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Integration.ApplyInputs (utcTimeToPOSIXTime)
import Language.Marlowe.Runtime.Integration.Common (
  Integration,
  Wallet (addresses),
  expectJust,
  expectRight,
  getGenesisWallet,
  runIntegrationTest,
  runWebClient,
 )
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (FromDTO (..), ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Client (postContract)
import qualified Language.Marlowe.Runtime.Web.Core.Address as Web
import qualified Language.Marlowe.Runtime.Web.Core.MarloweVersion as Web

import Language.Marlowe.Runtime.Web.Contract.API (ContractOrSourceId (..))
import qualified Language.Marlowe.Runtime.Web.Contract.API as Web
import qualified Language.Marlowe.Runtime.Web.Core.Roles as Web

import Language.Marlowe.Runtime.Web.Tx.API (
  CreateTxEnvelope (
    CreateTxEnvelope,
    contractId,
    safetyErrors,
    txEnvelope
  ),
 )
import Language.Marlowe.Runtime.Web.Server.DTO (FromDTO (..), ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Types (ApiError (..))
import Network.URI (parseURI)
import qualified PlutusLedgerApi.V2 as PV2
import qualified Servant.Client as Servant
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

-- Because `ApiError` is not a proper `DTO` type we have to define decoding
-- instance manually.
apiErrorFromResponse :: Servant.Response -> Either String ApiError
apiErrorFromResponse Servant.Response{..} = do
  json <- note "Failed to parse JSON" $ decode responseBody
  case json of
    A.Object o -> note "Failed to decode ApiError props" $ flip A.parseMaybe () $ const do
      message <- o .: "message"
      errorCode <- o .: "errorCode"
      details <- o .: "details"
      pure $ ApiError message errorCode details (fromEnum responseStatusCode)
    _ -> Left "Failed to parse JSON"

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

-- safetyErrorsReportsSpec

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

walletMarloweParty :: (MonadFail m) => Wallet -> m V1.Party
walletMarloweParty wallet = do
  let walletAddresses = addresses wallet
  plutusAddress <- expectJust "Failed to convert wallet address" $ toPlutusAddress $ changeAddress walletAddresses
  pure $ V1.Address (toEnum 0x00) plutusAddress

data OpenRoleUsage = UseOpenRoles | UseClosedRoles

safetyErrorsReportsSpec :: Spec
safetyErrorsReportsSpec = do
  let -- Because of the `NFData` constraint over runWebClient we have to do this gimnastics and pass check function to
      -- this helper.
      ada = V1.Token PV2.adaSymbol PV2.adaToken
      alice = V1.Role "Alice"
      bob = V1.Role "Bob"
      isTransactionValidationError = \case
        TransactionValidationError _ _ -> True
        _ -> False
      isTransactionWarning = \case
        TransactionWarning _ _ -> True
        _ -> False
      postContract'
        :: V1.Contract
        -> Map Web.Party Web.Assets
        -> OpenRoleUsage
        -> (Either ApiError (CreateTxEnvelope Web.CardanoTxBody) -> IO ())
        -> Integration ()
      postContract' contract accounts openRoleUsage checkSafetyErrors = do
        wallet <- getGenesisWallet 0
        let contractRoles = Set.toList (extractAll contract)
            contractRoleParties :: [Web.Party]
            contractRoleParties =
              catMaybes $
                contractRoles <&> \case
                  role@(V1.Role _) -> pure $ toDTO role
                  _ -> Nothing
            allRoles = Map.keys accounts <> contractRoleParties
        res <- runWebClient do
          let walletAddress = toDTO $ changeAddress $ addresses wallet
          createTxEnvelope <-
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
                      Web.Mint $ do
                        let roleTokenConfig =
                              Web.RoleTokenConfig
                                { recipients = do
                                    let recipient = case openRoleUsage of
                                          UseOpenRoles -> Web.OpenRole
                                          UseClosedRoles -> Web.ClosedRole walletAddress
                                    Map.singleton recipient 1
                                , metadata = Nothing
                                }
                        Map.fromList $
                          allRoles <&> \(Web.Party party) ->
                            (party, roleTokenConfig)
                , contract = ContractOrSourceId $ Left contract
                , accounts
                , minUTxODeposit = Nothing
                , tags = mempty
                }
          liftIO $ checkSafetyErrors (Right createTxEnvelope)
        case res of
          Left (Servant.FailureResponse _ response@Servant.Response{..}) -> liftIO do
            case apiErrorFromResponse response of
              Left err -> do
                let responseBody' = TLE.decodeUtf8 responseBody
                fail $ "Failed to parse JSON: " <> err <> ". JSON: " <> TL.unpack responseBody'
              Right apiError -> checkSafetyErrors (Left apiError)
          Left err -> liftIO $ fail $ "Unexpected servant client error: " <> show err
          Right _ -> pure ()

  describe "POST /contracts safety report" do
    it "reports safety errors in different scenarios" $ withLocalMarloweRuntime $ runIntegrationTest do
      do
        liftIO $ putStrLn "reporting errors when contract overspends the tx budget during decoding"
        let contract = do
              V1.When
                [ V1.Case (V1.Deposit alice alice ada $ V1.Constant i) $ V1.Pay alice (V1.Party alice) ada (V1.Constant i) V1.Close
                | i <- [1 .. 80]
                ]
                1000
                V1.Close
        postContract' contract mempty UseClosedRoles \case
          Right (CreateTxEnvelope{safetyErrors = errs}) | not (null errs) && all isTransactionValidationError errs -> pure ()
          errs -> fail $ "Expected validation error but got: " <> show errs

      do
        liftIO $ putStrLn "reporting errors when contract overspends the tx budget on closure"
        let contract = V1.When [] 0 V1.Close
            oneLovelace = Web.Assets{lovelace = 1, tokens = mempty}
            accounts =
              Map.fromList $
                [ (Web.Party $ T.pack (show i), oneLovelace)
                | i <- [1 .. 10] :: [Int]
                ]
        postContract' contract accounts UseClosedRoles \case
          Left ApiError{errorCode = "SafetyAnalysisFailed"} -> pure ()
          -- Right  errs | not (null errs) && all isTransactionValidationError errs -> pure ()
          res -> fail $ "Expected safety errors but got: " <> show res
      do
        liftIO $ putStrLn "reporting errors when contract failes during exectuion"
        let -- `bob` has empty account
            invalidPay = V1.Pay bob (V1.Account alice) ada (V1.Constant 10) V1.Close
            contract = do
              V1.When
                [ V1.Case (V1.Deposit alice alice ada $ V1.Constant 100) invalidPay
                ]
                1000
                V1.Close
        postContract' contract mempty UseClosedRoles \case
          Right (CreateTxEnvelope{safetyErrors = errs}) | not (null errs) && all isTransactionWarning errs -> pure ()
          errs -> fail $ "Expected transaction validation error but got: " <> show errs

      do
        liftIO $ putStrLn "reporting errors when contract overspends the tx budget during execution"
        let pays = do
              let step _ = V1.Pay alice (V1.Account bob) ada (V1.Constant 5)
              foldr step V1.Close [1 .. 20 :: Int]

            contract = do
              V1.When
                [ V1.Case (V1.Deposit alice alice ada $ V1.Constant 100) pays
                ]
                1000
                V1.Close
        postContract' contract mempty UseClosedRoles \case
          Right (CreateTxEnvelope{safetyErrors = [TransactionValidationError _ _]}) -> pure ()
          errs -> fail $ "Expected transaction validation error but got: " <> show errs

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
            TxMetadataInEra ShelleyBasedEraBabbage (TxMetadata m) -> do
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
