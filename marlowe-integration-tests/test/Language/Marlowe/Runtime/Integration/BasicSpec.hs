{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Language.Marlowe.Runtime.Integration.BasicSpec
  where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , CardanoEra(BabbageEra)
  , CtxTx
  , ShelleyWitnessSigningKey(..)
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(TxOut)
  , getTxId
  , signShelleyTransaction
  )
import Cardano.Api.Byron (deserialiseFromTextEnvelope)
import Control.Concurrent (threadDelay)
import Data.Aeson (decodeFileStrict)
import Data.Bifunctor (first)
import Data.Map (Map)
import qualified Data.Map as Map
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
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoTxId, fromCardanoTxOutDatum, fromCardanoTxOutValue)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , AssetId(..)
  , Assets(Assets)
  , BlockHeader
  , TokenName
  , TransactionMetadata(..)
  , TxId
  , TxIx
  , TxOutRef(..)
  , fromBech32
  )
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , SomeMarloweVersion(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  , fromChainPayoutDatum
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.History.Api (ContractStep(..), CreateStep(..), RedeemStep(..))
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), InputsApplied(..), MarloweTxCommand(..), RoleTokensConfig(..), WalletAddresses(..), mkMint)
import Network.Protocol.Job.Client (liftCommand, liftCommandWait)
import qualified Plutus.V2.Ledger.Api as PV2
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local
  (LocalTestnet(..), MarloweRuntime(..), PaymentKeyPair(..), execCli, withLocalMarloweRuntime)

spec :: Spec
spec = describe "Marlowe runtime API" do
  it "Basic e2e scenario" $ withLocalMarloweRuntime \runtime -> do
    (partyAAddress, partyASigningWitness) <- getGenesisWallet runtime 0
    (partyBAddress, partyBSigningWitness) <- getGenesisWallet runtime 1
    partyBAddressPlutus <- expectJust "Failed to convert party B address to a plutus address"
      $ toPlutusAddress partyBAddress
    let
      -- 0x00 = testnet
      partyB = Address (toEnum 0x00) partyBAddressPlutus
      partyAWalletAddresses = WalletAddresses partyAAddress mempty mempty
      partyASigningWitnesses = [partyASigningWitness]
      partyBWalletAddresses = WalletAddresses partyBAddress mempty mempty
      partyBSigningWitnesses = [partyBSigningWitness]

      -- 1. Start MarloweHeaderSyncClient (request next)
      startDiscoveryClient = runDiscoverySyncClient runtime
          $ MarloweHeaderSyncClient
          $ pure
          $ HeaderSync.SendMsgRequestNext
          -- 2. Expect wait
          $ headerSyncExpectWait do
            -- 3. Create standard contract
            contract@ContractCreated{txBody} <- createStandardContract runtime partyAWalletAddresses partyAAddress partyBAddressPlutus
            blockHeader <- submit runtime partyASigningWitnesses txBody
              -- 4. Poll
            HeaderSync.SendMsgPoll
              -- 5. Expect new headers
              <$> headerSyncExpectNewHeaders \actualBlock actualHeaders -> do
                  actualBlock `shouldBe` blockHeader
                  actualHeaders `shouldBe` [contractCreatedToContractHeader blockHeader contract]
                  continueWithNewHeaders blockHeader contract

        -- 6. RequestNext (header sync)
        -- 7. Expect Wait
      continueWithNewHeaders createBlock contract@ContractCreated{contractId} = pure $ HeaderSync.SendMsgRequestNext $ headerSyncExpectWait do
        -- 8. Deposit funds
        inputsApplied@InputsApplied{txBody} <- deposit runtime partyAWalletAddresses contractId partyA partyA ada 100_000_000
        depositBlock <- submit runtime partyASigningWitnesses txBody
        txOutRef <- runHistorySyncClient runtime $ marloweSyncClient contract inputsApplied createBlock depositBlock
        -- 33. Poll
        -- 34. Expect wait
        -- 35. Cancel
        -- 36. Done
        pure $ HeaderSync.SendMsgPoll $ headerSyncExpectWait $ pure $ HeaderSync.SendMsgCancel $ HeaderSync.SendMsgDone txOutRef

      -- 9. Start MarloweSyncClient (follow contract)
      marloweSyncClient
        :: ContractCreated BabbageEra 'V1
        -> InputsApplied BabbageEra 'V1
        -> BlockHeader
        -> BlockHeader
        -> MarloweSync.MarloweSyncClient IO TxOutRef
      marloweSyncClient contractCreated@ContractCreated{contractId, rolesCurrency} inputsApplied createBlock depositBlock = MarloweSync.MarloweSyncClient
          $ pure
          $ MarloweSync.SendMsgFollowContract contractId
          -- 10. Expect contract found
          $ marloweSyncExpectContractFound \actualBlock MarloweV1 createStep -> do
            actualBlock `shouldBe` createBlock
            createStep `shouldBe` contractCreatedToCreateStep contractCreated
            -- 11. Request next
            -- 12. Expect roll forward with deposit
            marloweSyncRequestNextExpectRollForward depositBlock [ApplyTransaction $ inputsAppliedToTransaction depositBlock inputsApplied] do
              -- 13. Request next
              -- 14. Expect wait, poll, expect wait
              pure $ marloweSyncRequestNextExpectWait $ pure $ marloweSyncPollExpectWait do
                -- 15. Make choice as party B
                choiceApplied@InputsApplied{txBody} <- choose runtime partyBWalletAddresses contractId "Gimme the money" partyB 0
                choiceBlock <- submit runtime partyBSigningWitnesses txBody
                -- 16. Poll
                -- 17. Expect roll forward with choice
                marloweSyncPollExpectRollForward choiceBlock [ApplyTransaction $ inputsAppliedToTransaction choiceBlock choiceApplied] do
                  -- 18. Request next
                  -- 19. Expect wait
                  pure $ marloweSyncRequestNextExpectWait do
                    -- 20. Notify
                    notifyApplied@InputsApplied{txBody = notifyTxBody, output = notifyOutput} <- notify runtime partyAWalletAddresses contractId
                    notifyBlock <- submit runtime partyASigningWitnesses notifyTxBody

                    -- 21. Deposit as party B
                    depositApplied@InputsApplied{txBody = depositTxBody} <- deposit runtime partyBWalletAddresses contractId partyA partyB ada 100_000_000
                    depositBlockPartyB <- submit runtime partyBSigningWitnesses depositTxBody

                    -- 22. Withdraw as party A
                    withdrawTxBody <- withdraw runtime partyAWalletAddresses contractId "Party A"
                    withdrawBlock <- submit runtime partyASigningWitnesses withdrawTxBody

                    -- 23. Poll
                    -- 24. Expect roll forward with notify
                    marloweSyncPollExpectRollForward notifyBlock [ApplyTransaction $ inputsAppliedToTransaction notifyBlock notifyApplied] do
                      let depositTransaction@Transaction{output = TransactionOutput{payouts}} = inputsAppliedToTransaction depositBlockPartyB depositApplied
                      -- 25. Request next
                      -- 26. Expect roll forward with deposit
                      marloweSyncRequestNextExpectRollForward depositBlockPartyB [ApplyTransaction depositTransaction] do
                        -- 27. Request next
                        -- 28. Expect roll forward with withdraw
                        payoutTxOutRef <- expectJust "Failed to extract payout from deposit" case Map.toList payouts of
                          [(txOutRef, _)] -> Just txOutRef
                          _ -> Nothing
                        let withdrawTxId = fromCardanoTxId $ getTxId withdrawTxBody
                        marloweSyncRequestNextExpectRollForward withdrawBlock [RedeemPayout $ RedeemStep payoutTxOutRef withdrawTxId $ AssetId rolesCurrency "Party A"] do
                          -- 29. Request next (marlowe sync)
                          -- 30. Expect wait
                          -- 31. Cancel
                          -- 32. Done
                          TransactionScriptOutput{utxo = notifyTxOutRef} <- expectJust "Failed to obtain deposit output" notifyOutput
                          pure $ marloweSyncRequestNextExpectWait $ pure $ MarloweSync.SendMsgCancel $ MarloweSync.SendMsgDone notifyTxOutRef


    txOutRef <- startDiscoveryClient
    -- 37. Start MarloweSyncClient (follow a tx in the contract)
    -- 38. Expect contract not found
    runHistorySyncClient runtime $ MarloweSync.MarloweSyncClient $ pure $ MarloweSync.SendMsgFollowContract (ContractId txOutRef) $ MarloweSync.ClientStFollow
      { recvMsgContractFound = \_ _ _ -> fail "Expected contract not found, got contract found"
      , recvMsgContractNotFound = pure ()
      }

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
      { recvMsgContractNotFound = fail "Expected contract found, got contract not found"
      , recvMsgContractFound
      }

    marloweSyncExpectRollForward
      :: (BlockHeader -> [ContractStep v] -> IO (MarloweSync.ClientStIdle v IO a))
      -> IO (MarloweSync.ClientStNext v IO a)
    marloweSyncExpectRollForward recvMsgRollForward = do
      startTime <- getCurrentTime
      let
        next = MarloweSync.ClientStNext
          { recvMsgRollBackCreation = fail "Expected roll forward, got roll back creation"
          , recvMsgRollBackward = \_ -> fail "Expected roll forward, got roll backward"
          , recvMsgWait = do
              time <- getCurrentTime
              if (time `diffUTCTime` startTime) > timeout
                then fail "Expected roll forward, got wait"
                else do
                  threadDelay retryDelayMicroSeconds
                  pure $ MarloweSync.SendMsgPoll next
          , recvMsgRollForward
          }
      pure next

    marloweSyncPollExpectWait
      :: IO (MarloweSync.ClientStWait v IO a)
      -> MarloweSync.ClientStWait v IO a
    marloweSyncPollExpectWait = MarloweSync.SendMsgPoll . marloweSyncExpectWait

    marloweSyncPollExpectRollForward
      :: (Show (ContractStep v), Eq (ContractStep v))
      => BlockHeader
      -> [ContractStep v]
      -> IO (MarloweSync.ClientStIdle v IO a)
      -> IO (MarloweSync.ClientStWait v IO a)
    marloweSyncPollExpectRollForward expectedBlock expectedSteps next =
      MarloweSync.SendMsgPoll <$> marloweSyncExpectRollForward \actualBlock actualSteps -> do
        actualBlock `shouldBe` expectedBlock
        actualSteps `shouldBe` expectedSteps
        next

    marloweSyncRequestNextExpectWait
      :: IO (MarloweSync.ClientStWait v IO a)
      -> MarloweSync.ClientStIdle v IO a
    marloweSyncRequestNextExpectWait = MarloweSync.SendMsgRequestNext . marloweSyncExpectWait

    marloweSyncRequestNextExpectRollForward
      :: (Show (ContractStep v), Eq (ContractStep v))
      => BlockHeader
      -> [ContractStep v]
      -> IO (MarloweSync.ClientStIdle v IO a)
      -> IO (MarloweSync.ClientStIdle v IO a)
    marloweSyncRequestNextExpectRollForward expectedBlock expectedSteps next =
      MarloweSync.SendMsgRequestNext <$> marloweSyncExpectRollForward \actualBlock actualSteps -> do
        actualBlock `shouldBe` expectedBlock
        actualSteps `shouldBe` expectedSteps
        next

    marloweSyncExpectWait
      :: IO (MarloweSync.ClientStWait v IO a)
      -> MarloweSync.ClientStNext v IO a
    marloweSyncExpectWait recvMsgWait = MarloweSync.ClientStNext
      { recvMsgRollBackCreation = fail "Expected wait, got roll back creation"
      , recvMsgRollBackward = \_ -> fail "Expected wait, got roll backward"
      , recvMsgWait
      , recvMsgRollForward = \_ _ -> fail "Expected wait, got roll forward"
      }

contractCreatedToCreateStep :: ContractCreated BabbageEra v -> CreateStep v
contractCreatedToCreateStep ContractCreated{..} = CreateStep
  { createOutput = TransactionScriptOutput
      { address = marloweScriptAddress
      , assets = Assets 2_000_000 mempty
      , utxo = unContractId contractId
      , datum
      }
  , metadata = mempty
  , payoutValidatorHash = payoutScriptHash
  }

inputsAppliedToTransaction :: BlockHeader -> InputsApplied BabbageEra v -> Transaction v
inputsAppliedToTransaction blockHeader InputsApplied{..} = Transaction
  { transactionId = fromCardanoTxId $ getTxId txBody
  , contractId
  , metadata = mempty
  , blockHeader
  , validityLowerBound = invalidBefore
  , validityUpperBound = invalidHereafter
  , inputs
  , output = TransactionOutput
      { payouts = foldMap (uncurry $ txOutToPayout version $ fromCardanoTxId $ getTxId txBody) case txBody of
          TxBody TxBodyContent{..} -> zip [0..] txOuts
      , scriptOutput = output
      }
  }

txOutToPayout :: MarloweVersion v -> TxId -> TxIx -> TxOut CtxTx BabbageEra -> Map TxOutRef (Payout v)
txOutToPayout version txId txIx (TxOut address value datum _) = case snd $ fromCardanoTxOutDatum datum of
  Just datum' -> case fromChainPayoutDatum version datum' of
    Just payoutDatum -> Map.singleton (TxOutRef txId txIx) Payout
      { address = fromCardanoAddressInEra BabbageEra address
      , assets = fromCardanoTxOutValue value
      , datum = payoutDatum
      }
    Nothing -> mempty
  Nothing -> mempty

contractCreatedToContractHeader :: BlockHeader -> ContractCreated BabbageEra v -> ContractHeader
contractCreatedToContractHeader blockHeader ContractCreated{..} = ContractHeader
  { contractId
  , rolesCurrency
  , metadata = TransactionMetadata metadata
  , marloweScriptHash
  , marloweScriptAddress
  , payoutScriptHash
  , marloweVersion = SomeMarloweVersion version
  , blockHeader
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

choose
  :: MarloweRuntime
  -> WalletAddresses
  -> ContractId
  -> PV2.BuiltinByteString
  -> Party
  -> Integer
  -> IO (InputsApplied BabbageEra 'V1)
choose MarloweRuntime{..} walletAddresses contractId choice party chosenNum = do
  result <- runTxJobClient $ liftCommand $ ApplyInputs
    MarloweV1
    walletAddresses
    contractId
    mempty
    Nothing
    Nothing
    [NormalInput $ IChoice (ChoiceId choice party) chosenNum]
  expectRight "Failed to create choice transaction" result

notify
  :: MarloweRuntime
  -> WalletAddresses
  -> ContractId
  -> IO (InputsApplied BabbageEra 'V1)
notify MarloweRuntime{..} walletAddresses contractId = do
  result <- runTxJobClient $ liftCommand $ ApplyInputs
    MarloweV1
    walletAddresses
    contractId
    mempty
    Nothing
    Nothing
    [NormalInput INotify]
  expectRight "Failed to create notify transaction" result

withdraw
  :: MarloweRuntime
  -> WalletAddresses
  -> ContractId
  -> TokenName
  -> IO (TxBody BabbageEra)
withdraw MarloweRuntime{..} walletAddresses contractId role = do
  result <- runTxJobClient $ liftCommand $ Withdraw MarloweV1 walletAddresses contractId role
  expectRight "Failed to create withdraw transaction" result

createStandardContract
  :: MarloweRuntime
  -> WalletAddresses
  -> Address
  -> PV2.Address
  -> IO (ContractCreated BabbageEra 'V1)
createStandardContract
  MarloweRuntime{..}
  walletAddresses
  partyAAddress
  partyBAddress = do
    now <- getCurrentTime
    result <- runTxJobClient $ liftCommand $ Create
      Nothing
      MarloweV1
      walletAddresses
      (RoleTokensMint $ mkMint $ pure ("Party A", (partyAAddress, Left 1)))
      mempty
      2_000_000
      (standardContract partyBAddress now (secondsToNominalDiffTime 100))
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
