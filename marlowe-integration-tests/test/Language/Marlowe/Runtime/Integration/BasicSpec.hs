{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Integration.BasicSpec
  where

import Cardano.Api (AsType(..), BabbageEra, ShelleyWitnessSigningKey(..), TxBody, getTxId, signShelleyTransaction)
import Cardano.Api.Byron (deserialiseFromTextEnvelope)
import Control.Concurrent (threadDelay)
import Data.Aeson (decodeFileStrict)
import Data.Bifunctor (first)
import Data.String (fromString)
import Data.Time
  (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient(..))
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HeaderSync
import qualified Language.Marlowe.Protocol.Sync.Client as MarloweSync
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api
  (Address, Assets(Assets), BlockHeader, TransactionMetadata(TransactionMetadata), fromBech32)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(unContractId)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , SomeMarloweVersion(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.History.Api (ContractStep(..), CreateStep(..))
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api
  ( ContractCreated(..)
  , InputsApplied(..)
  , MarloweTxCommand(..)
  , RoleTokensConfig(..)
  , WalletAddresses(WalletAddresses)
  , mkMint
  )
import Network.Protocol.Job.Client (liftCommand, liftCommandWait)
import qualified Plutus.V2.Ledger.Api as PV2
import System.Directory (withCurrentDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local
  (LocalTestnet(..), MarloweRuntime(..), PaymentKeyPair(..), execCli, withLocalMarloweRuntime)

spec :: Spec
spec = describe "Marlowe runtime API" do
  it "Basic e2e scenario" $ withCurrentDirectory ".." $ withLocalMarloweRuntime \runtime -> do
    (partyAAddress, partyASigningWitness) <- getGenesisWallet runtime 0
    (partyBAddress, partyBSigningWitness) <- getGenesisWallet runtime 1
    let
      partyAWalletAddresses = WalletAddresses partyAAddress mempty mempty
      partyASigningWitnesses = [partyASigningWitness]
      _partyBWalletAddresses = WalletAddresses partyBAddress mempty mempty
      _partyBSigningWitnesses = [partyBSigningWitness]

      -- 1. Start MarloweHeaderSyncClient (request next)
      startDiscoveryClient = runDiscoverySyncClient runtime
          $ MarloweHeaderSyncClient
          $ pure
          $ HeaderSync.SendMsgRequestNext
          -- 2. Expect wait
          $ headerSyncExpectWait do
            -- 3. Create standard contract
            contract@ContractCreated{..} <- createStandardContract runtime partyAWalletAddresses partyAAddress partyBAddress
            blockHeader <- submit runtime partyASigningWitnesses txBody
            let
              expectedContractHeader = ContractHeader
                { contractId
                , rolesCurrency
                , metadata = TransactionMetadata metadata
                , marloweScriptHash
                , marloweScriptAddress
                , payoutScriptHash
                , marloweVersion = SomeMarloweVersion MarloweV1
                , blockHeader
                }
              -- 4. Poll
            HeaderSync.SendMsgPoll
              -- 5. Expect new headers
              <$> headerSyncExpectNewHeaders \actualBlock actualHeaders -> do
                  actualBlock `shouldBe` blockHeader
                  actualHeaders `shouldBe` [expectedContractHeader]
                  continueWithNewHeaders blockHeader contract

      continueWithNewHeaders createBlock contract@ContractCreated{..} = pure
        -- 6. RequestNext (header sync)
        $ HeaderSync.SendMsgRequestNext
        -- 7. Expect Wait
        $ headerSyncExpectWait do
          -- 8. Deposit funds
          inputsApplied <- deposit runtime partyAWalletAddresses contractId partyA partyA ada 100_000_000
          depositBlock <- submit runtime partyASigningWitnesses txBody
          runHistorySyncClient runtime $ marloweSyncClient contract inputsApplied createBlock depositBlock
          fail "TODO implement the rest of the test"

      -- 9. Start MarloweSyncClient (follow contract)
      marloweSyncClient
        :: ContractCreated BabbageEra 'V1
        -> InputsApplied BabbageEra 'V1
        -> BlockHeader
        -> BlockHeader
        -> MarloweSync.MarloweSyncClient IO ()
      marloweSyncClient ContractCreated{..} inputsApplied createBlock depositBlock = MarloweSync.MarloweSyncClient
          $ pure
          $ MarloweSync.SendMsgFollowContract contractId
          -- 10. Expect contract found
          $ marloweSyncExpectContractFound \actualBlock MarloweV1 createStep -> do
            actualBlock `shouldBe` createBlock
            createStep `shouldBe` CreateStep
              { createOutput = TransactionScriptOutput
                  { address = marloweScriptAddress
                  , assets = Assets 2_000_000 mempty
                  , utxo = unContractId contractId
                  , datum
                  }
              , metadata = mempty
              , payoutValidatorHash = payoutScriptHash
              }
            pure
              -- 11. Request next
              $ MarloweSync.SendMsgRequestNext
              -- 12. Expect roll forward with deposit
              $ marloweSyncExpectRollForward \block steps -> do
                  block `shouldBe` depositBlock
                  let
                    InputsApplied{ txBody = body, invalidBefore, invalidHereafter, inputs, output } = inputsApplied
                    expectedStep = ApplyTransaction Transaction
                      { transactionId = fromCardanoTxId $ getTxId body
                      , contractId
                      , metadata = mempty
                      , blockHeader = depositBlock
                      , validityLowerBound = invalidBefore
                      , validityUpperBound = invalidHereafter
                      , inputs
                      , output = TransactionOutput
                          { payouts = mempty
                          , scriptOutput = output
                          }
                      }
                  steps `shouldBe` [expectedStep]
                  fail "TODO implement the rest of the test"

        {-
            13. Request next (marlowe sync)
            14. Expect wait
            15. Make choice as party B
            16. Poll (marlowe sync)
            17. Expect roll forward with choice
            18. Request next (marlowe sync)
            19. Expect wait
            20. Notify
            21. Withdraw as party B
            22. Poll (marlowe sync)
            23. Expect roll forward with notify
            24. Request next (marlowe sync)
            25. Expect roll forward with withdrawal
            26. Request next (marlowe sync)
            27. Expect wait
            28. Cancel
            29. Done
            30. Request next (header sync)
            31. Expect wait
            32. Cancel
            33. Done
            34. Start MarloweSyncClient (follow a tx in the contract)
            35. Expect contract not found
        -}

    startDiscoveryClient
  where
    headerSyncExpectWait
      :: IO (HeaderSync.ClientStWait IO a) -> HeaderSync.ClientStNext IO a
    headerSyncExpectWait action = HeaderSync.ClientStNext
      { recvMsgNewHeaders = \_ _ -> fail "Expected wait, got new headers"
      , recvMsgRollBackward = \_ -> fail "Expected wait, got roll backward"
      , recvMsgWait = action
      }

    headerSyncExpectNewHeaders
      :: (BlockHeader -> [ContractHeader] -> IO (HeaderSync.ClientStIdle IO a))
      -> IO (HeaderSync.ClientStNext IO a)
    headerSyncExpectNewHeaders recvMsgNewHeaders = do
      startTime <- getCurrentTime
      let
        next = HeaderSync.ClientStNext
          { recvMsgNewHeaders
          , recvMsgRollBackward = \_ -> fail "Expected new headers, got roll backward"
          , recvMsgWait = do
              time <- getCurrentTime
              if (time `diffUTCTime` startTime) > timeout
                then fail "Expected new headers, got wait"
                else do
                  threadDelay retryDelayMicroSeconds
                  pure $ HeaderSync.SendMsgPoll next
          }
      pure next

    marloweSyncExpectContractFound
      :: (forall v. BlockHeader -> MarloweVersion v -> CreateStep v -> IO (MarloweSync.ClientStIdle v IO a))
      -> MarloweSync.ClientStFollow IO a
    marloweSyncExpectContractFound recvMsgContractFound = MarloweSync.ClientStFollow
      { recvMsgContractNotFound = fail "Expected contract found, got new contract not found"
      , recvMsgContractFound
      }

    marloweSyncExpectRollForward
      :: (BlockHeader -> [ContractStep v] -> IO (MarloweSync.ClientStIdle v IO a))
      -> MarloweSync.ClientStNext v IO a
    marloweSyncExpectRollForward recvMsgRollForward = MarloweSync.ClientStNext
      { recvMsgRollBackCreation = fail "Expected roll forward, got new roll back creation"
      , recvMsgRollBackward = \_ -> fail "Expected roll forward, got new roll backward"
      , recvMsgWait = fail "Expected roll forward, got wait"
      , recvMsgRollForward
      }

timeout :: NominalDiffTime
timeout = secondsToNominalDiffTime 2

retryDelayMicroSeconds :: Int
retryDelayMicroSeconds = 100_000

getGenesisWallet :: MarloweRuntime -> Int -> IO (Address, ShelleyWitnessSigningKey)
getGenesisWallet MarloweRuntime{..} walletIx = do
  let LocalTestnet{..} = testnet
  let PaymentKeyPair{..} = wallets !! walletIx
  mAddress <- fromBech32 . fromString <$> execCli
    [ "address", "build"
    , "--verification-key-file", paymentVKey
    , "--testnet-magic", "1"
    ]
  address <- expectJust "Failed to decode address" mAddress
  mTextEnvelope <- decodeFileStrict paymentSKey
  textEnvelope <- expectJust "Failed to decode signing key text envelope" mTextEnvelope
  genesisUTxOKey <- expectRight  "failed to decode text envelope"
    $ deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope
  pure (address, WitnessGenesisUTxOKey genesisUTxOKey)

submit
  :: MarloweRuntime
  -> [ShelleyWitnessSigningKey]
  -> TxBody BabbageEra
  -> IO BlockHeader
submit MarloweRuntime{..} signingKeys txBody = do
  let tx = signShelleyTransaction txBody signingKeys
  submitResult <- runTxJobClient $ liftCommandWait $ Submit tx
  expectRight "failed to submit tx" $ first (tx,) submitResult

deposit
  :: MarloweRuntime
  -> WalletAddresses
  -> ContractId
  -> Party
  -> Party
  -> Token
  -> Integer
  -> IO (InputsApplied BabbageEra 'V1)
deposit MarloweRuntime{..} walletAddresses contractId intoAccount fromParty ofToken quantity = do
  result <- runTxJobClient $ liftCommand $ ApplyInputs
    MarloweV1
    walletAddresses
    contractId
    mempty
    Nothing
    Nothing
    [NormalInput $ IDeposit intoAccount fromParty ofToken quantity]
  expectRight "Failed to create deposit transaction" result

createStandardContract
  :: MarloweRuntime
  -> WalletAddresses
  -> Address
  -> Address
  -> IO (ContractCreated BabbageEra 'V1)
createStandardContract
  MarloweRuntime{..}
  walletAddresses
  partyAAddress
  partyBAddress = do
    now <- getCurrentTime
    partyBAddressPlutus <- expectJust "Failed to convert party B address to a plutus address"
      $ toPlutusAddress partyBAddress
    result <- runTxJobClient $ liftCommand $ Create
      Nothing
      MarloweV1
      walletAddresses
      (RoleTokensMint $ mkMint $ pure ("Party A", (partyAAddress, Left 1)))
      mempty
      2_000_000
      (standardContract partyBAddressPlutus now (secondsToNominalDiffTime 100))
    expectRight "failed to create standard contract" result


standardContract
  :: PV2.Address
  -> UTCTime
  -> NominalDiffTime
  -> Contract
standardContract partyBAddress startTime timeoutLength = When
  [ Case (Deposit partyA partyA ada (Constant 100_000_000))
      ( When
          [ Case (Choice (ChoiceId "Gimme the money" partyB) [Bound 0 0])
              ( When
                  [ Case (Notify TrueObs)
                      ( Pay partyA (Party partyB) ada (AvailableMoney partyA ada)
                          ( When
                              [ Case (Deposit partyA partyB ada (Constant 100_000_000)) Close
                              ]
                              (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime))))
                              Close
                          )
                      )
                  ]
                  (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime)))
                  Close
              )
          ]
          (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime))
          Close
      )
  ]
  (toPosixTime $ timeoutLength `addUTCTime` startTime)
  Close
  where
    toPosixTime t = PV2.POSIXTime $ floor $ 1000 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t)
    -- 0x00 = testnet
    partyB = Address (toEnum 0x00) partyBAddress

partyA :: Party
partyA = Role "Party A"

expectJust :: String -> Maybe a -> IO a
expectJust msg = \case
  Nothing -> fail msg
  Just a -> pure a

expectRight :: Show a => String -> Either a b -> IO b
expectRight msg = \case
  Left a -> fail $ msg <> ": " <> show a
  Right b -> pure b
