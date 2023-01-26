{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Integration.BasicSpec
  where

import Cardano.Api (AsType(..), BabbageEra, ShelleyWitnessSigningKey(..), signShelleyTransaction)
import Cardano.Api.Byron (deserialiseFromTextEnvelope)
import Data.Aeson (decodeFileStrict)
import Data.String (fromString)
import Data.Time (NominalDiffTime, UTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock (addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient(..))
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HeaderSync
import Language.Marlowe.Runtime.ChainSync.Api
  (Address, BlockHeader, TransactionMetadata(TransactionMetadata), fromBech32)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), MarloweVersionTag(..), SomeMarloweVersion(..))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), MarloweTxCommand(..), RoleTokensConfig(..), WalletAddresses(WalletAddresses), mkMint)
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
    (partyBAddress, _partyBSigningWitness) <- getGenesisWallet runtime 1
    let
      -- 1. Start MarloweHeaderSyncClient (request next)
      startDiscoveryClient = runDiscoverySyncClient runtime
          $ MarloweHeaderSyncClient
          $ pure
          $ HeaderSync.SendMsgRequestNext
          -- 2. Expect wait
          $ headerSyncExpectWait do
            -- 3. Create standard contract
            let walletAddresses = WalletAddresses partyAAddress mempty mempty
            (blockHeader, contract@ContractCreated{..}) <- createStandardContract
              runtime
              walletAddresses
              [partyASigningWitness]
              partyAAddress
              partyBAddress
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
            pure
              -- 4. Poll
              $ HeaderSync.SendMsgPoll
              -- 5. Expect new headers
              $ headerSyncExpectNewHeaders (`shouldBe` blockHeader) (`shouldBe` [expectedContractHeader])
              $ continueWithNewHeaders contract

      continueWithNewHeaders _ = fail "TODO implement the rest of the test"
        {-
            6. RequestNext (header sync)
            7. Expect Wait
            8. Deposit funds
            9. Start MarloweSyncClient (follow contract)
            10. Expect contract found
            11. Request next (marlowe sync)
            12. Expect roll forward with deposit
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
      :: (BlockHeader -> IO ())
      -> ([ContractHeader] -> IO ())
      -> IO (HeaderSync.ClientStIdle IO a)
      -> HeaderSync.ClientStNext IO a
    headerSyncExpectNewHeaders inspectBlock inspectHeaders action = HeaderSync.ClientStNext
      { recvMsgNewHeaders = \block headers -> do
          inspectBlock block
          inspectHeaders headers
          action
      , recvMsgRollBackward = \_ -> fail "Expected new headers, got roll backward"
      , recvMsgWait = fail "Expected new headers, got wait"
      }

getGenesisWallet :: MarloweRuntime -> Int -> IO (Address, ShelleyWitnessSigningKey)
getGenesisWallet MarloweRuntime{..} walletIx = do
  let LocalTestnet{..} = testnet
  let PaymentKeyPair{..} = wallets !! walletIx
  mAddress <- fromBech32 . fromString <$> execCli
    [ "address", "build"
    , "--verification-key-file", paymentVKey
    , "--testnet-magic", "1"
    ]
  address <- case mAddress of
    Nothing -> fail "Failed to decode address"
    Just a -> pure a
  mTextEnvelope <- decodeFileStrict paymentSKey
  textEnvelope <- case mTextEnvelope of
    Nothing -> fail "Failed to decode signing key text envelope"
    Just a -> pure a
  genesisUTxOKey <- case deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope of
    Left err -> fail $ "failed to decode text envelope " <> show err
    Right a -> pure a
  pure (address, WitnessGenesisUTxOKey genesisUTxOKey)

createStandardContract
  :: MarloweRuntime
  -> WalletAddresses
  -> [ShelleyWitnessSigningKey]
  -> Address
  -> Address
  -> IO (BlockHeader, ContractCreated BabbageEra 'V1)
createStandardContract
  MarloweRuntime{..}
  walletAddresses
  signingKeys
  partyAAddress
  partyBAddress = do
    now <- getCurrentTime
    partyBAddressPlutus <- case toPlutusAddress partyBAddress of
      Nothing -> fail "Failed to convert party B address to a plutus address"
      Just addr -> pure addr
    result <- runTxJobClient $ liftCommand $ Create
      Nothing
      MarloweV1
      walletAddresses
      (RoleTokensMint $ mkMint $ pure ("Party A", (partyAAddress, Left 1)))
      mempty
      2_000_000
      (standardContract partyBAddressPlutus now (secondsToNominalDiffTime 100))
    contract@ContractCreated{..} <- case result of
      Left err -> fail $ "failed to create standard contract: " <> show err
      Right contract -> pure contract
    let tx = signShelleyTransaction txBody signingKeys
    submitResult <- runTxJobClient $ liftCommandWait $ Submit tx
    case submitResult of
      Left err -> fail $ "failed to submit standard contract creation tx: " <> show err
      Right block -> pure (block, contract)


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
    partyA = Role "Party A"
    -- 0x00 = testnet
    partyB = Address (toEnum 0x00) partyBAddress
