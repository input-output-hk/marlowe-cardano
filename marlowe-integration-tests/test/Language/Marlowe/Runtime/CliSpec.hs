{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.CliSpec where

import Cardano.Api (
  AsType (..),
  BabbageEra,
  CardanoEra (BabbageEra),
  TxBody,
  deserialiseFromCBOR,
  readFileTextEnvelope,
  serialiseToCBOR,
 )
import qualified Cardano.Api.Shelley
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import Data.Foldable (for_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as POSIX
import Data.Void (Void)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync.Api
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import qualified Language.Marlowe.Runtime.Client as Runtime.Client
import Language.Marlowe.Runtime.Core.Api (
  MarloweMetadata (..),
  MarloweMetadataTag,
  MarloweTransactionMetadata (..),
  MarloweVersion (MarloweV1),
  MarloweVersionTag (V1),
  renderContractId,
 )
import Language.Marlowe.Runtime.Integration.Common (
  Integration,
  Wallet (..),
  getGenesisWallet,
  runIntegrationTest,
 )
import qualified Language.Marlowe.Runtime.Integration.Common as Runtime.Integration.Common
import Language.Marlowe.Runtime.Transaction.Api (
  MarloweTxCommand (..),
  WalletAddresses (..),
  WithdrawTx (..),
 )
import qualified Language.Marlowe.Runtime.Transaction.Api as Runtime.Transaction.Api
import Language.Marlowe.Util (ada)
import qualified Network.Protocol.Job.Client as JobClient
import qualified Plutus.V2.Ledger.Api
import Test.Hspec (
  Spec,
  describe,
  focus,
  it,
  shouldBe,
 )
import qualified Test.Hspec as Hspec
import Test.Integration.Marlowe (
  LocalTestnet (..),
  MarloweRuntime,
  resolveWorkspacePath,
  testnet,
  withLocalMarloweRuntime,
  writeWorkspaceFileJSON,
 )
import UnliftIO (
  concurrently,
  liftIO,
 )

data CLISpecTestData = CLISpecTestData
  { partyAWallet :: Wallet
  , partyBWallet :: Wallet
  , runtime :: MarloweRuntime
  }

spec :: Spec
spec = Hspec.describe "Marlowe runtime CLI" $ Hspec.aroundAll setup do
  createSpec
  depositSpec
  chooseSpec
  notifySpec
  applySpec
  withdrawSpec
  bugPLT6773
  where
    setup :: Hspec.ActionWith CLISpecTestData -> IO ()
    setup runSpec = withLocalMarloweRuntime $ runIntegrationTest do
      runtime <- Reader.ask
      partyAWallet <- getGenesisWallet 0
      partyBWallet <- getGenesisWallet 1
      liftIO $ runSpec CLISpecTestData{..}

serializeAddress :: ChainSync.Api.Address -> String
serializeAddress = Text.unpack . Maybe.fromJust . ChainSync.Api.toBech32

toCliArgs :: MarloweTxCommand Void err result -> [String]
toCliArgs = \case
  Create _ MarloweV1 WalletAddresses{changeAddress, extraAddresses} _ _ (ChainSync.Api.Lovelace minUTXO) _ ->
    ["create", "--change-address", serializeAddress changeAddress]
      <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
      <> ["--min-utxo", show minUTXO]
  ApplyInputs MarloweV1 WalletAddresses{changeAddress, extraAddresses} contractId _metadata _ _ inputs ->
    let tokenNotAda :: V1.Token -> Maybe (Plutus.V2.Ledger.Api.CurrencySymbol, Plutus.V2.Ledger.Api.TokenName)
        tokenNotAda = \case V1.Token "" "" -> Nothing; V1.Token a b -> Just (a, b)
     in case inputs of
          [V1.NormalInput (V1.IDeposit toParty fromParty (tokenNotAda -> Nothing) quantity)] ->
            ["deposit", "--change-address", serializeAddress changeAddress]
              <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
              <> ["--contract", Text.unpack $ renderContractId contractId]
              <> ["--to-party", removeQuotes $ show toParty]
              <> ["--from-party", removeQuotes $ show fromParty]
              <> ["--lovelace", show quantity]
          [V1.NormalInput (V1.IChoice (V1.ChoiceId choice party) value)] ->
            ["choose", "--change-address", serializeAddress changeAddress]
              <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
              <> ["--contract", Text.unpack $ renderContractId contractId]
              <> ["--choice", removeQuotes $ show choice]
              <> ["--party", removeQuotes $ show party]
              <> ["--value", show value]
          [V1.NormalInput V1.INotify] ->
            ["notify", "--change-address", serializeAddress changeAddress]
              <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
              <> ["--contract", Text.unpack $ renderContractId contractId]
          _ : _ : _ ->
            ["apply", "--change-address", serializeAddress changeAddress]
              <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
              <> ["--contract", Text.unpack $ renderContractId contractId]
          _ -> undefined
  Withdraw MarloweV1 WalletAddresses{changeAddress, extraAddresses} contractId tokenName ->
    ["withdraw", "--change-address", serializeAddress changeAddress]
      <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
      <> ["--contract", Text.unpack $ renderContractId contractId]
      <> ["--role", removeQuotes $ show tokenName]
  where
    removeQuotes :: String -> String
    removeQuotes = init . tail

marloweRuntimeJobClient :: MarloweTxCommand Void err result -> Integration (TxBody BabbageEra)
marloweRuntimeJobClient = \case
  cmd@(Create _ MarloweV1 _ _ _ _ _) ->
    runMarloweTxClient (JobClient.liftCommand cmd) >>= \case
      Left err -> error ("Some JobClient create error: " <> show err)
      Right Runtime.Transaction.Api.ContractCreated{txBody} -> pure txBody
  cmd@(ApplyInputs MarloweV1 _ _ _ _ _ _) ->
    runMarloweTxClient (JobClient.liftCommand cmd) >>= \case
      Left err -> error ("Some JobClient input error: " <> show err)
      Right Runtime.Transaction.Api.InputsApplied{txBody} -> pure txBody
  cmd@(Withdraw MarloweV1 _ _ _) ->
    runMarloweTxClient (JobClient.liftCommand cmd) >>= \case
      Left err -> error ("Some JobClient withdraw error: " <> show err)
      Right Runtime.Transaction.Api.WithdrawTx{txBody} -> pure txBody

expectSameResultFromCLIAndJobClient :: String -> [String] -> MarloweTxCommand Void err result -> Integration ()
expectSameResultFromCLIAndJobClient outputFile extraCliArgs command = do
  workspace <- Reader.asks $ workspace . testnet

  let txBodyEnvelopeFilePath :: FilePath
      txBodyEnvelopeFilePath = resolveWorkspacePath workspace outputFile

      cliEffect :: Integration String
      cliEffect =
        Runtime.Integration.Common.execMarlowe $
          toCliArgs command <> extraCliArgs <> ["--manual-sign", txBodyEnvelopeFilePath]

      jobClientEffect :: Integration (TxBody BabbageEra)
      jobClientEffect =
        either (error . show) id . deserialiseFromCBOR (AsTxBody AsBabbageEra) . serialiseToCBOR
          <$> marloweRuntimeJobClient command

  (_, expected) <- concurrently cliEffect jobClientEffect

  (either (error . show) id -> actual) <-
    liftIO $ readFileTextEnvelope (AsTxBody (cardanoEraToAsType BabbageEra)) txBodyEnvelopeFilePath

  liftIO do
    let Cardano.Api.Shelley.ShelleyTxBody
          actualEra
          -- FIXME compare all fields except _vldt in _actualShelleyLedgerBabbageTxBody with fields in _expectedShelleyLedgerBabbageTxBody
          _actualShelleyLedgerBabbageTxBody -- Cardano.Ledger.Babbage.TxBody.TxBody (Cardano.Ledger.Babbage.BabbageEra Cardano.Ledger.Crypto.StandardCrypto)
          actualLedgerScripts
          actualTxBodyScriptData
          actualAuxiliaryData
          actualTxScriptValidity = actual

        Cardano.Api.Shelley.ShelleyTxBody
          expectedEra
          _expectedShelleyLedgerBabbageTxBody
          expectedLedgerScripts
          expectedTxBodyScriptData
          expectedAuxiliaryData
          expectedTxScriptValidity = expected

    actualEra `shouldBe` expectedEra
    actualLedgerScripts `shouldBe` expectedLedgerScripts
    actualTxBodyScriptData `shouldBe` expectedTxBodyScriptData
    actualAuxiliaryData `shouldBe` expectedAuxiliaryData
    actualTxScriptValidity `shouldBe` expectedTxScriptValidity

toPosixTime :: Time.UTCTime -> Plutus.V2.Ledger.Api.POSIXTime
toPosixTime t = Plutus.V2.Ledger.Api.POSIXTime $ floor $ 1000 * Time.nominalDiffTimeToSeconds (POSIX.utcTimeToPOSIXSeconds t)

standardMetadata :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata) -> MarloweTransactionMetadata
standardMetadata tags =
  let continuations = Nothing
      transactionMetadata = mempty
      marloweMetadata = if Map.null tags then Nothing else Just MarloweMetadata{..}
   in MarloweTransactionMetadata{..}

createSpec :: Hspec.SpecWith CLISpecTestData
createSpec = describe "create" $
  it "generates a create tx body envelope" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
    workspace <- Reader.asks $ workspace . testnet

    let tags :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata)
        tags =
          Map.fromList
            [ ("Tag1", Nothing)
            , ("Tag2", Nothing)
            ]

        contract :: V1.Contract
        contract = V1.Close

        md@MarloweTransactionMetadata{transactionMetadata} = standardMetadata tags

    contractFilePath <- writeWorkspaceFileJSON workspace "close-contract.json" contract
    transactionMetadataFilePath <- writeWorkspaceFileJSON workspace "transaction-metadata.json" transactionMetadata
    tagsFilePath <- writeWorkspaceFileJSON workspace "tags.json" tags

    let Wallet{addresses} = partyAWallet

        extraCliArgs =
          [ "--core-file"
          , contractFilePath
          , "--metadata-file"
          , transactionMetadataFilePath
          , "--tags-file"
          , tagsFilePath
          ]

        creationCommand
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.CreateError 'V1)
              (Runtime.Transaction.Api.ContractCreated BabbageEra 'V1)
        creationCommand =
          Create
            Nothing
            MarloweV1
            addresses
            Runtime.Transaction.Api.RoleTokensNone
            md
            (ChainSync.Api.Lovelace 2_000_000)
            (Left contract)

    expectSameResultFromCLIAndJobClient "create-tx-body.json" extraCliArgs creationCommand

depositSpec :: Hspec.SpecWith CLISpecTestData
depositSpec = describe "deposit" $
  it "generates a deposit tx body envelope" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
    now <- liftIO Time.getCurrentTime

    let partyA :: V1.Party
        partyA = V1.Role "Party A"

        contract :: V1.Contract
        contract =
          V1.When
            [V1.Case (V1.Deposit partyA partyA ada (V1.Constant 100_000_000)) V1.Close]
            (toPosixTime $ Time.secondsToNominalDiffTime 100 `Time.addUTCTime` now)
            V1.Close

        tags :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata)
        tags = Map.empty

    Runtime.Transaction.Api.ContractCreated{contractId, txBody} <-
      Runtime.Integration.Common.expectRight "failed to create deposit contract"
        =<< Runtime.Client.createContract
          Nothing
          MarloweV1
          (addresses partyAWallet)
          ( Runtime.Transaction.Api.RoleTokensMint $
              Runtime.Transaction.Api.mkMint $
                pure ("Party A", (changeAddress $ addresses partyAWallet, Nothing))
          )
          (standardMetadata tags)
          2_000_000
          (Left contract)

    _ <- Runtime.Integration.Common.submit partyAWallet txBody

    let Wallet{addresses} = partyAWallet

        extraCliArgs = []

        command
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.ApplyInputsError 'V1)
              (Runtime.Transaction.Api.InputsApplied BabbageEra 'V1)
        command =
          ApplyInputs
            MarloweV1
            addresses
            contractId
            (standardMetadata Map.empty)
            Nothing
            Nothing
            [V1.NormalInput $ V1.IDeposit partyA partyA ada 100_000_000]

    expectSameResultFromCLIAndJobClient "deposit-tx-body.json" extraCliArgs command

chooseSpec :: Hspec.SpecWith CLISpecTestData
chooseSpec = describe "choose" $
  it "generates a choose tx body envelope" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
    now <- liftIO Time.getCurrentTime

    let partyA :: V1.Party
        partyA = V1.Role "Party A"

        contract :: V1.Contract
        contract =
          V1.When
            [V1.Case (V1.Choice (V1.ChoiceId "my choice" partyA) [V1.Bound 0 0]) V1.Close]
            (toPosixTime $ Time.secondsToNominalDiffTime 100 `Time.addUTCTime` now)
            V1.Close

        tags :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata)
        tags = Map.empty

    Runtime.Transaction.Api.ContractCreated{contractId, txBody} <-
      Runtime.Integration.Common.expectRight "failed to create choose contract"
        =<< Runtime.Client.createContract
          Nothing
          MarloweV1
          (addresses partyAWallet)
          ( Runtime.Transaction.Api.RoleTokensMint $
              Runtime.Transaction.Api.mkMint $
                pure ("Party A", (changeAddress $ addresses partyAWallet, Nothing))
          )
          (standardMetadata tags)
          2_000_000
          (Left contract)

    _ <- Runtime.Integration.Common.submit partyAWallet txBody

    let Wallet{addresses} = partyAWallet

        extraCliArgs = []

        command
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.ApplyInputsError 'V1)
              (Runtime.Transaction.Api.InputsApplied BabbageEra 'V1)
        command =
          ApplyInputs
            MarloweV1
            addresses
            contractId
            (standardMetadata Map.empty)
            Nothing
            Nothing
            [V1.NormalInput $ V1.IChoice (V1.ChoiceId "my choice" partyA) 0]

    expectSameResultFromCLIAndJobClient "choose-tx-body.json" extraCliArgs command

notifySpec :: Hspec.SpecWith CLISpecTestData
notifySpec = describe "notify" $
  it "generates a notify tx body envelope" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
    now <- liftIO Time.getCurrentTime

    let contract :: V1.Contract
        contract =
          V1.When
            [V1.Case (V1.Notify V1.TrueObs) V1.Close]
            (toPosixTime $ Time.secondsToNominalDiffTime 100 `Time.addUTCTime` now)
            V1.Close

        tags :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata)
        tags = Map.empty

    Runtime.Transaction.Api.ContractCreated{contractId, txBody} <-
      Runtime.Integration.Common.expectRight "failed to create notify contract"
        =<< Runtime.Client.createContract
          Nothing
          MarloweV1
          (addresses partyAWallet)
          ( Runtime.Transaction.Api.RoleTokensMint $
              Runtime.Transaction.Api.mkMint $
                pure ("Party A", (changeAddress $ addresses partyAWallet, Nothing))
          )
          (standardMetadata tags)
          2_000_000
          (Left contract)

    _ <- Runtime.Integration.Common.submit partyAWallet txBody

    let Wallet{addresses} = partyAWallet

        extraCliArgs = []

        command
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.ApplyInputsError 'V1)
              (Runtime.Transaction.Api.InputsApplied BabbageEra 'V1)
        command =
          ApplyInputs
            MarloweV1
            addresses
            contractId
            (standardMetadata Map.empty)
            Nothing
            Nothing
            [V1.NormalInput V1.INotify]

    expectSameResultFromCLIAndJobClient "notify-tx-body.json" extraCliArgs command

applySpec :: Hspec.SpecWith CLISpecTestData
applySpec = describe "apply" $
  it "generates a deposit-choose-notify tx body envelope" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
    now <- liftIO Time.getCurrentTime

    let partyA :: V1.Party
        partyA = V1.Role "Party A"

        contract :: V1.Contract
        contract =
          V1.When
            [ V1.Case (V1.Deposit partyA partyA ada (V1.Constant 100_000_000)) $
                V1.When
                  [ V1.Case (V1.Choice (V1.ChoiceId "my choice" partyA) [V1.Bound 0 0]) $
                      V1.When
                        [ V1.Case
                            (V1.Notify V1.TrueObs)
                            V1.Close
                        ]
                        (toPosixTime $ Time.secondsToNominalDiffTime 300 `Time.addUTCTime` now)
                        V1.Close
                  ]
                  (toPosixTime $ Time.secondsToNominalDiffTime 200 `Time.addUTCTime` now)
                  V1.Close
            ]
            (toPosixTime $ Time.secondsToNominalDiffTime 100 `Time.addUTCTime` now)
            V1.Close

        tags :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata)
        tags = Map.empty

        inputs :: [V1.Input]
        inputs =
          [ V1.NormalInput $ V1.IDeposit partyA partyA ada 100_000_000
          , V1.NormalInput $ V1.IChoice (V1.ChoiceId "my choice" partyA) 0
          , V1.NormalInput V1.INotify
          ]

    Runtime.Transaction.Api.ContractCreated{contractId, txBody} <-
      Runtime.Integration.Common.expectRight "failed to create deposit-choose-notify contract"
        =<< Runtime.Client.createContract
          Nothing
          MarloweV1
          (addresses partyAWallet)
          ( Runtime.Transaction.Api.RoleTokensMint $
              Runtime.Transaction.Api.mkMint $
                pure ("Party A", (changeAddress $ addresses partyAWallet, Nothing))
          )
          (standardMetadata tags)
          2_000_000
          (Left contract)

    _ <- Runtime.Integration.Common.submit partyAWallet txBody

    inputsFilePath <- do
      workspace <- Reader.asks $ workspace . testnet
      writeWorkspaceFileJSON workspace "deposit-choose-notify-inputs.json" inputs

    let Wallet{addresses} = partyAWallet

        extraCliArgs = ["--inputs-file", inputsFilePath]

        command
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.ApplyInputsError 'V1)
              (Runtime.Transaction.Api.InputsApplied BabbageEra 'V1)
        command =
          ApplyInputs
            MarloweV1
            addresses
            contractId
            (standardMetadata Map.empty)
            Nothing
            Nothing
            inputs

    expectSameResultFromCLIAndJobClient "deposit-choose-notify-tx-body.json" extraCliArgs command

withdrawSpec :: Hspec.SpecWith CLISpecTestData
withdrawSpec = describe "withdraw" $
  it "generates a withdraw tx body envelope" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
    now <- liftIO Time.getCurrentTime

    let partyA :: V1.Party
        partyA = V1.Role "Party A"

        contract :: V1.Contract
        contract =
          V1.When
            [V1.Case (V1.Deposit partyA partyA ada (V1.Constant 100_000_000)) V1.Close]
            (toPosixTime $ Time.secondsToNominalDiffTime 100 `Time.addUTCTime` now)
            V1.Close

        tags :: Map MarloweMetadataTag (Maybe ChainSync.Api.Metadata)
        tags = Map.empty

    Runtime.Transaction.Api.ContractCreated{contractId, txBody} <-
      Runtime.Integration.Common.expectRight "failed to create withdraw contract"
        =<< Runtime.Client.createContract
          Nothing
          MarloweV1
          (addresses partyAWallet)
          ( Runtime.Transaction.Api.RoleTokensMint $
              Runtime.Transaction.Api.mkMint $
                pure ("Party A", (changeAddress $ addresses partyAWallet, Nothing))
          )
          (standardMetadata tags)
          2_000_000
          (Left contract)

    _ <- Runtime.Integration.Common.submit partyAWallet txBody

    let Wallet{addresses} = partyAWallet

        depositCommand
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.ApplyInputsError 'V1)
              (Runtime.Transaction.Api.InputsApplied BabbageEra 'V1)
        depositCommand =
          ApplyInputs
            MarloweV1
            addresses
            contractId
            (standardMetadata Map.empty)
            Nothing
            Nothing
            [V1.NormalInput $ V1.IDeposit partyA partyA ada 100_000_000]

    _ <- Runtime.Integration.Common.submit partyAWallet =<< marloweRuntimeJobClient depositCommand

    let extraCliArgs = []

        command
          :: MarloweTxCommand
              Void
              (Runtime.Transaction.Api.WithdrawError 'V1)
              (WithdrawTx BabbageEra 'V1)
        command =
          Withdraw
            MarloweV1
            addresses
            contractId
            "Party A"

    expectSameResultFromCLIAndJobClient "withdraw-tx-body.json" extraCliArgs command

bugPLT6773 :: Hspec.SpecWith CLISpecTestData
bugPLT6773 = focus $
  describe "[BUG] PLT-6773: Marlowe runtime cannot load any contracts" $
    it "Marlowe runtime can load any contracts" \CLISpecTestData{..} -> flip runIntegrationTest runtime do
      workspace <- Reader.asks $ workspace . testnet
      let contractHashRelation :: [(String, V1.Contract, Aeson.Value)]
          contractHashRelation =
            [ ("923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec", V1.Close, Aeson.String "close")
            ,
              ( "ee5ab3bfda75834c3c1503ec7cd0b7fccbce7ceb3909e5404910bfd9e09b1be4"
              , V1.Assert V1.TrueObs V1.Close
              , Aeson.object [("assert", Aeson.Bool True), ("then", Aeson.String "close")]
              )
            ]

      for_ contractHashRelation \(expectedHash :: String, contract :: V1.Contract, expectedContract :: Aeson.Value) -> do
        contractFilePath <- writeWorkspaceFileJSON workspace "contract.json" contract

        do
          (code, stdout, stderr) <- Runtime.Integration.Common.execMarlowe' ["load", "--read-json", contractFilePath]

          liftIO do
            putStrLn stderr
            (code, stdout) `shouldBe` (ExitSuccess, expectedHash ++ "\n")

        do
          (code, stdout, stderr) <- Runtime.Integration.Common.execMarlowe' ["query", "store", "contract", expectedHash]

          let actualContractJSON :: Maybe Aeson.Value = Aeson.decode $ fromString stdout

          liftIO do
            putStrLn stderr
            (code, actualContractJSON) `shouldBe` (ExitSuccess, Just expectedContract)
