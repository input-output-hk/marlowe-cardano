{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Marlowe.Runtime.Web.Contracts.Transactions.Transaction.Post where

import qualified Cardano.Api as C
import Contrib.Data.Time.Units (toNominalDiffTime)
import Control.Category ((<<<))
import Control.Concurrent (threadDelay)
import Control.Error.Util (hush, note)
import Control.Lens (view)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (ask)
import Data.Foldable (find)
import Data.Functor (void)
import qualified Data.List.NonEmpty as NonEmptyList
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time (addUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Time.Units (Second)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (..), Transaction (..))
import qualified Language.Marlowe.CLI.Test as CLI
import qualified Language.Marlowe.CLI.Test.CLI.Types as CLI
import qualified Language.Marlowe.CLI.Test.Contrib.Monad.Loops as CLI
import qualified Language.Marlowe.CLI.Test.ExecutionMode as CLI
import qualified Language.Marlowe.CLI.Test.Types as CLI
import qualified Language.Marlowe.CLI.Test.Wallet.Types as CLI
import qualified Language.Marlowe.CLI.Types as CLI
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import Language.Marlowe.Extended.V1 (ada)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Integration.Common (
  Integration,
  Wallet (addresses, signingKeys),
  expectJust,
  expectJustM,
  getGenesisWallet,
  mkEmptyWallet,
  runIntegrationTest,
  runWebClient,
  runWebClient',
 )
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web (
  ApplyInputsTxEnvelope (ApplyInputsTxEnvelope),
  ContractOrSourceId (..),
  RoleTokenConfig (..),
  RoleTokenRecipient (ClosedRole),
 )
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (
  Page (..),
  getContract,
  getContracts,
  postContract,
  postTransaction,
  putTransaction,
 )
import Language.Marlowe.Runtime.Web.Common (
  signShelleyTransaction',
  submitContract,
  submitTransaction,
  waitUntilConfirmed,
 )
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO (toDTO))
import qualified PlutusLedgerApi.V1.Value as PLA
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusTx.AssocMap as AM
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Integration.Marlowe.Local (localTestnetToLocalNodeConnectInfo, testnet, withLocalMarloweRuntime)

spec :: Spec
spec = do
  describe "POST to /contracts/transactions" do
    reportErrorOnBudgetOverspendingSpec
    reportErrorOnClosureBudgetOverspendingSpec
    reportErrorOnInvalidAddressInTheState
  describe "POST /contracts/{contractId}/transactions" do
    it "returns the transaction header" $ withLocalMarloweRuntime $ runIntegrationTest do
      partyAWallet <- getGenesisWallet' WalletZero
      partyBWallet <- getGenesisWallet' WalletOne

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
              , threadTokenName = Nothing
              , roles =
                  Just
                    . Web.Mint
                    . Map.singleton "Party A"
                    $ RoleTokenConfig (Map.singleton (ClosedRole partyAWebChangeAddress) 1) Nothing
              , contract = ContractOrSourceId $ Left contract
              , accounts = mempty
              , minUTxODeposit = Nothing
              , tags = mempty
              }

        _ <- submitContract partyAWallet contractCreated

        let inputs = [V1.NormalInput $ V1.IDeposit partyA partyA ada 100_000_000]

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

-- | We use CLI to submit unsafe contracts/states because we want to test the safety report.
cliSubmitContract
  :: V1.Contract -> Maybe V1.State -> Maybe (NonEmptyList.NonEmpty CLI.TokenAssignment) -> Integration ContractId
cliSubmitContract contract initialState tokenAssignment = do
  wallet <- getGenesisWallet' WalletZero
  marloweRuntime <- ask
  let connection = localTestnetToLocalNodeConnectInfo . testnet $ marloweRuntime
      operations = case tokenAssignment of
        Just assignment ->
          let rolesCurrencyNickname = "RoleCurrency"
           in [ CLI.WalletOperation $ CLI.mint rolesCurrencyNickname assignment 2_000_000
              , CLI.CLIOperation $ CLI.initialize' contract (Just rolesCurrencyNickname) (note 2_000_000 initialState)
              , CLI.CLIOperation CLI.autoRun
              ]
        Nothing ->
          [ CLI.CLIOperation $ CLI.initialize' contract Nothing (note 2_000_000 initialState)
          , CLI.CLIOperation CLI.autoRun
          ]
  walletSigningKey <- do
    let possibleSigningKey = do
          skey <- listToMaybe . signingKeys $ wallet
          CLI.txWitnessSigningKeyToSomePaymentSigningKey skey
    expectJust "Failed to convert signing key" possibleSigningKey

  possibleResult <-
    liftIO $
      CLI.runOperations @C.BabbageEra connection (CLI.UseOnChainMode 60) (CLI.FundingFaucet walletSigningKey) operations
  case possibleResult of
    Left err -> fail $ "CLI failed submitting the contract: " <> show err
    Right cliExecutionResult -> do
      contractInfo <- case cliExecutionResult of
        CLI.TestSucceeded interpretState -> do
          let CLI.CLIContracts contractsMap = view CLI.isCLIContracts interpretState
          expectJust "Failed to get contract info from the result" . listToMaybe . Map.elems $ contractsMap
        CLI.TestFailed (CLI.FailureReport err _) _ -> fail $ show err
        CLI.TestSkipped _ -> fail "Operations were skipped"

      contractId <- expectJust "Failed to get contract id from the result - missing on-chain thread" do
        th <- view CLI.ciThread contractInfo
        pure $ CLI.cliMarloweThreadContractId th

      _ <- do
        let fetchStatus = runWebClient do
              Web.ContractState{status} <- getContract (toDTO contractId)
              pure status
        flip waitUntilConfirmed fetchStatus \case
          Right status -> status
          _ -> Web.Submitted
      pure contractId

tokenNameFromString :: String -> V1.TokenName
tokenNameFromString = PLA.tokenName . Text.encodeUtf8 . Text.pack

rolePartyFromString :: String -> V1.Party
rolePartyFromString = V1.Role . tokenNameFromString

data GenesisWalletIndex = WalletZero | WalletOne

getGenesisWallet' :: GenesisWalletIndex -> Integration Wallet
getGenesisWallet' = \case
  WalletZero -> getGenesisWallet 0
  WalletOne -> getGenesisWallet 1

mkMainnetWalletParty :: Wallet -> Integration V1.Party
mkMainnetWalletParty = do
  expectJust "Failed to convert wallet address"
    <<< fmap (V1.Address V1.mainnet)
    <<< toPlutusAddress
    <<< changeAddress
    <<< addresses

mkWalletParty :: Wallet -> Integration V1.Party
mkWalletParty =
  expectJust "Failed to convert wallet address"
    <<< fmap (V1.Address V1.testnet)
    <<< toPlutusAddress
    <<< changeAddress
    <<< addresses

mkWalletWebAddress :: Wallet -> Web.Address
mkWalletWebAddress = toDTO . changeAddress . addresses

mkWalletBech32Address :: Wallet -> Integration Text
mkWalletBech32Address wallet = do
  expectJust "Failed to convert wallet address" . Chain.toBech32 $ changeAddress $ addresses wallet

mkRelativeTimeout :: Second -> IO PV2.POSIXTime
mkRelativeTimeout seconds = do
  now <- liftIO getCurrentTime
  let toPosixTime t = PV2.POSIXTime $ floor $ 1000 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t)
      delta = toNominalDiffTime seconds
  pure $ toPosixTime $ delta `addUTCTime` now

reportErrorOnBudgetOverspendingSpec :: Spec
reportErrorOnBudgetOverspendingSpec = do
  it "reports future contract budget overspending" $ withLocalMarloweRuntime $ runIntegrationTest do
    wallet <- getGenesisWallet' WalletZero
    walletParty <- mkWalletParty wallet
    timeout <- liftIO $ mkRelativeTimeout 3600
    let contract = do
          let step _ = V1.Pay walletParty (V1.Account walletParty) ada (V1.Constant 5)
              notifyTrue continuation = V1.When [V1.Case (V1.Notify V1.TrueObs) continuation] timeout V1.Close
              pays = foldr step V1.Close [1 .. 20 :: Int]
          V1.When
            [ V1.Case
                (V1.Deposit walletParty walletParty ada $ V1.Constant 10_000_000)
                (notifyTrue pays)
            ]
            timeout
            V1.Close
    contractId <- cliSubmitContract contract Nothing Nothing

    let inputs = [V1.NormalInput $ V1.IDeposit walletParty walletParty ada 10_000_000]
        walletWebAddress = mkWalletWebAddress wallet

    safetyErrors <- runWebClient' do
      ApplyInputsTxEnvelope{safetyErrors} <-
        postTransaction
          walletWebAddress
          Nothing
          Nothing
          (toDTO contractId)
          Web.PostTransactionsRequest
            { version = Web.V1
            , metadata = mempty
            , invalidBefore = Nothing
            , invalidHereafter = Nothing
            , inputs
            , tags = mempty
            }
      pure safetyErrors

    liftIO $
      safetyErrors `shouldSatisfy` \case
        [TransactionValidationError t _] -> do
          let transactionMarloweInputs (Transaction _ _ txInput _ _) = do
                let V1.TransactionInput{txInputs} = txInput
                txInputs
          -- The report should contain two paths of execution which fail on closure - on timeout and on input.
          transactionMarloweInputs t == [V1.NormalInput V1.INotify]
        _ -> False

reportErrorOnClosureBudgetOverspendingSpec :: Spec
reportErrorOnClosureBudgetOverspendingSpec = do
  it "reports closure budget overspending" $ withLocalMarloweRuntime $ runIntegrationTest do
    timeout <- liftIO $ mkRelativeTimeout 3600
    wallet <- getGenesisWallet' WalletZero
    walletAddress <- mkWalletBech32Address wallet

    let roleNames = ["Role " <> show i | i <- [1 .. 20 :: Int]]
        walletWebAddress = mkWalletWebAddress wallet
        tokenDistribution =
          NonEmptyList.singleton $
            CLI.TokenAssignment
              (CLI.AddressRecipient walletAddress)
              [(tokenNameFromString roleName, 1) | roleName <- roleNames]
        notifyTrue continuation = V1.When [V1.Case (V1.Notify V1.TrueObs) continuation] timeout V1.Close
        -- should report the future budget overspending.
        contract = notifyTrue $ notifyTrue V1.Close

        -- This should cover min ADA as a side effect: 0.5 ADA * 20 = 5 ADA
        initialAccounts :: V1.Accounts
        initialAccounts =
          AM.fromList $
            [ ((accountId, ada), 500_000)
            | roleName <- roleNames
            , let accountId = rolePartyFromString roleName
            ]
        state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = initialAccounts}

    contractId <- cliSubmitContract contract (Just state) (Just tokenDistribution)

    -- FIXME: Safety error is reported but possibly locking fund (if we used Deposit) submission is feasible!
    safetyErrors <- runWebClient' do
      let inputs = [V1.NormalInput V1.INotify]
      tx@ApplyInputsTxEnvelope{safetyErrors} <-
        postTransaction
          walletWebAddress
          Nothing
          Nothing
          (toDTO contractId)
          Web.PostTransactionsRequest
            { version = Web.V1
            , metadata = mempty
            , invalidBefore = Nothing
            , invalidHereafter = Nothing
            , inputs
            , tags = mempty
            }
      _ <- submitTransaction wallet tx
      pure safetyErrors

    liftIO $ print safetyErrors
    -- We expect the transaction to succeed, but the safety errors to be reported.
    liftIO $
      safetyErrors `shouldSatisfy` \case
        [TransactionValidationError t1 _, TransactionValidationError t2 _] -> do
          let transactionMarloweInputs (Transaction _ _ txInput _ _) = do
                let V1.TransactionInput{txInputs} = txInput
                txInputs
          -- The report should contain two paths of execution which fail on closure - on timeout and on input.
          (transactionMarloweInputs <$> [t1, t2]) == [[V1.NormalInput V1.INotify], []]
        _ -> False

    response' <- runWebClient do
      let inputs = [V1.NormalInput V1.INotify]
      _ <-
        postTransaction
          walletWebAddress
          Nothing
          Nothing
          (toDTO contractId)
          Web.PostTransactionsRequest
            { version = Web.V1
            , metadata = mempty
            , invalidBefore = Nothing
            , invalidHereafter = Nothing
            , inputs
            , tags = mempty
            }
      pure ()
    liftIO $ hush response' `shouldBe` Nothing

reportErrorOnInvalidAddressInTheState :: Spec
reportErrorOnInvalidAddressInTheState = do
  it "reports invalid address in the state" $ withLocalMarloweRuntime $ runIntegrationTest do
    timeout <- liftIO $ mkRelativeTimeout 3600
    wallet <- getGenesisWallet' WalletZero
    invalidParty <- mkMainnetWalletParty wallet
    validParty <- mkWalletParty wallet
    let notifyTrue continuation = V1.When [V1.Case (V1.Notify V1.TrueObs) continuation] timeout V1.Close
        contract = notifyTrue $ notifyTrue V1.Close
        walletWebAddress = mkWalletWebAddress wallet
        initialAccounts :: V1.Accounts
        initialAccounts = AM.fromList [((invalidParty, ada), 2000_000), ((validParty, ada), 2000_000)]
        initialState = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = initialAccounts}

    contractId <- cliSubmitContract contract (Just initialState) Nothing

    -- FIXME: Safety error is reported but possibly locking fund (if we used Deposit) submission is feasible!
    safetyErrors <- runWebClient' do
      let inputs = [V1.NormalInput V1.INotify]
      tx@ApplyInputsTxEnvelope{safetyErrors} <-
        postTransaction
          walletWebAddress
          Nothing
          Nothing
          (toDTO contractId)
          Web.PostTransactionsRequest
            { version = Web.V1
            , metadata = mempty
            , invalidBefore = Nothing
            , invalidHereafter = Nothing
            , inputs
            , tags = mempty
            }
      _ <- submitTransaction wallet tx
      pure safetyErrors

    -- We expect the transaction to succeed, but the safety errors to be reported.
    liftIO $ safetyErrors `shouldBe` [WrongNetwork, InconsistentNetworks]

    response <- runWebClient do
      let inputs = [V1.NormalInput V1.INotify]
      tx <-
        postTransaction
          walletWebAddress
          Nothing
          Nothing
          (toDTO contractId)
          Web.PostTransactionsRequest
            { version = Web.V1
            , metadata = mempty
            , invalidBefore = Nothing
            , invalidHereafter = Nothing
            , inputs
            , tags = mempty
            }
      _ <- submitTransaction wallet tx
      pure ()

    -- Transaction sholud failed.
    -- This behaviour will change when network "fixing": https://github.com/input-output-hk/marlowe-cardano/issues/831
    liftIO $ hush response `shouldBe` Nothing
