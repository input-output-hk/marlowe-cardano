{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Marlowe.Runtime.Integration.Common
  where

import Cardano.Api
  ( AddressAny(AddressShelley)
  , AsType(..)
  , BabbageEra
  , CardanoEra(..)
  , CtxTx
  , Key(verificationKeyHash)
  , NetworkId(Testnet)
  , NetworkMagic(NetworkMagic)
  , PaymentCredential(PaymentCredentialByKey)
  , ShelleyWitnessSigningKey(..)
  , StakeAddressReference(NoStakeAddress)
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(..)
  , generateSigningKey
  , getTxId
  , getVerificationKey
  , makeShelleyAddress
  , signShelleyTransaction
  )
import qualified Cardano.Api as C
import Cardano.Api.Byron (deserialiseFromTextEnvelope)
import qualified Cardano.Api.Shelley as C
import Control.Concurrent (threadDelay)
import Control.Monad (guard, void, (<=<))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..), ask, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (StateT, runStateT, state)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON(..), Value(..), decodeFileStrict, eitherDecodeStrict)
import Data.Aeson.Types (parseFail)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Traversable (for)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe (ChoiceId(..), Input(..), InputContent(..), Party, Token)
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HeaderSync
import qualified Language.Marlowe.Protocol.Sync.Client as MarloweSync
import Language.Marlowe.Runtime.Cardano.Api
  ( fromCardanoAddressAny
  , fromCardanoAddressInEra
  , fromCardanoTxId
  , fromCardanoTxOutDatum
  , fromCardanoTxOutValue
  , toCardanoAddressAny
  , toCardanoAddressInEra
  , toCardanoTxOut
  )
import Language.Marlowe.Runtime.ChainSync.Api
  ( Assets(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , BlockNo(..)
  , Lovelace
  , SlotNo(..)
  , TokenName
  , TxId
  , TxIx
  , TxOutRef(..)
  , fromBech32
  )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Client
  (MarloweT, applyInputs, runMarloweHeaderSyncClient, runMarloweSyncClient, runMarloweT, runMarloweTxClient)
import qualified Language.Marlowe.Runtime.Client as Client
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout(..)
  , SomeMarloweVersion(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  , emptyMarloweTransactionMetadata
  , fromChainPayoutDatum
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep(..))
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), InputsApplied(..), MarloweTxCommand(..), SubmitError, WalletAddresses(..))
import Network.Protocol.Job.Client (liftCommandWait)
import qualified Plutus.V2.Ledger.Api as PV2
import Servant.Client (ClientError, ClientM)
import System.Exit (ExitCode(..))
import Test.Hspec (shouldBe)
import Test.Integration.Marlowe
  (LocalTestnet(..), MarloweRuntime(MarloweRuntime), PaymentKeyPair(..), SpoNode(..), exec, exec', execCli)
import qualified Test.Integration.Marlowe.Local as MarloweRuntime
import UnliftIO (bracket_)
import UnliftIO.Environment (setEnv, unsetEnv)

type Integration = MarloweT (ReaderT MarloweRuntime IO)

-- Important - the TxId is lazily computed and depends on the resulting Tx. So
-- it should not be used to compute the transaction outputs written.
type TxBuilder = ReaderT TxId (StateT [Chain.TransactionOutput] Integration)

allocateWallet :: [[(Bool, Lovelace)]] -> TxBuilder Wallet
allocateWallet balances = do
  txId <- ask
  (addresses, signingKeys, collateralUtxos) <- unzip3 <$> for balances \utxos -> do
    signingKey <- liftIO $ generateSigningKey AsPaymentKey
    let verificationKey = getVerificationKey signingKey
    let paymentCredential = PaymentCredentialByKey $ verificationKeyHash verificationKey
    networkId' <- lift $ lift networkId
    let address = fromCardanoAddressAny $ AddressShelley $ makeShelleyAddress networkId' paymentCredential NoStakeAddress
    collateralUtxos <- Set.fromList . catMaybes <$> for utxos \(isCollateral, balance) -> state \outputs ->
      ( guard isCollateral $> Chain.TxOutRef txId (fromIntegral $ length outputs)
      , Chain.TransactionOutput address (Assets balance mempty) Nothing Nothing : outputs
      )
    pure (address, WitnessPaymentKey signingKey, collateralUtxos)
  pure Wallet
    { addresses = WalletAddresses (head addresses) (Set.fromList $ tail addresses) (fold collateralUtxos)
    , signingKeys
    }

submitBuilder :: Wallet -> TxBuilder a -> Integration (BlockHeader, a)
submitBuilder wallet builder = mdo
  -- Note - the txId is not evaluated yet - we're referring to it lazily.
  (a, txOuts) <- runStateT (runReaderT builder txId) []
  utxo <- getUTxO wallet
  let
    txBodyContent = TxBodyContent
      { txIns = (, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) . fst
          <$> Map.toList (C.unUTxO utxo)
      , txInsCollateral = C.TxInsCollateralNone
      , txInsReference = C.TxInsReferenceNone
      , txOuts = fromJust . toCardanoTxOut C.MultiAssetInBabbageEra <$> reverse txOuts
      , txTotalCollateral = C.TxTotalCollateralNone
      , txReturnCollateral = C.TxReturnCollateralNone
      , txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra 0
      , txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
      , txMetadata = C.TxMetadataNone
      , txAuxScripts = C.TxAuxScriptsNone
      , txExtraKeyWits = C.TxExtraKeyWitnessesNone
      , txProtocolParams = C.BuildTxWith Nothing
      , txWithdrawals = C.TxWithdrawalsNone
      , txCertificates = C.TxCertificatesNone
      , txUpdateProposal = C.TxUpdateProposalNone
      , txMintValue = C.TxMintNone
      , txScriptValidity = C.TxScriptValidityNone
      }
  txBody <- balanceTx wallet utxo txBodyContent
  let txId = fromCardanoTxId $ getTxId txBody
  (, a) <$> submit wallet txBody

balanceTx :: Wallet -> C.UTxO BabbageEra -> TxBodyContent C.BuildTx BabbageEra -> Integration (TxBody BabbageEra)
balanceTx (Wallet WalletAddresses{..} _) utxo txBodyContent = do
  start <- queryNode 0 C.QuerySystemStart
  history <- queryNode 0 $ C.QueryEraHistory C.CardanoModeIsMultiEra
  protocol <- queryBabbage 0 $ C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage C.QueryProtocolParameters
  changeAddr <- expectJust "Could not convert to Cardano address" $ toCardanoAddressInEra C.BabbageEra changeAddress
  C.BalancedTxBody txBody _ _ <- expectRight "Failed to balance Tx" $ C.makeTransactionBodyAutoBalance
    C.BabbageEraInCardanoMode
    start
    history
    protocol
    mempty
    utxo
    txBodyContent
    changeAddr
    Nothing
  pure txBody

getUTxO :: Wallet -> Integration (C.UTxO BabbageEra)
getUTxO (Wallet WalletAddresses{..} _) = queryBabbage 0
  $ C.QueryInShelleyBasedEra C.ShelleyBasedEraBabbage
  $ C.QueryUTxO
  $ C.QueryUTxOByAddress
  $ Set.insert (fromJust $ toCardanoAddressAny changeAddress)
  $ Set.map (fromJust . toCardanoAddressAny) extraAddresses

queryBabbage :: Int -> C.QueryInEra C.BabbageEra a -> Integration a
queryBabbage nodeNum =
  either (fail . show) pure <=< queryNode nodeNum . C.QueryInEra C.BabbageEraInCardanoMode

queryNode :: Int -> C.QueryInMode C.CardanoMode a -> Integration a
queryNode nodeNum query = do
  connectInfo <- nodeConnectInfo nodeNum
  liftIO $ either (fail . show) pure =<< C.queryNodeLocalState connectInfo Nothing query

nodeConnectInfo :: Int -> Integration (C.LocalNodeConnectInfo C.CardanoMode)
nodeConnectInfo nodeNum = do
  LocalTestnet{..} <- testnet
  let SpoNode{..} = spoNodes !! nodeNum
  C.LocalNodeConnectInfo (C.CardanoModeParams $ C.EpochSlots 21600) <$> networkId <*> pure socket

networkId :: Integration NetworkId
networkId = Testnet . NetworkMagic . fromIntegral . testnetMagic <$> testnet

data Wallet = Wallet
  { addresses :: WalletAddresses
  , signingKeys :: [ShelleyWitnessSigningKey]
  }

instance Semigroup Wallet where
  a <> b = Wallet
    { addresses = WalletAddresses
        { changeAddress = changeAddress $ addresses a
        , extraAddresses = Set.insert (changeAddress (addresses b)) $ on (<>) (extraAddresses . addresses) a b
        , collateralUtxos = on (<>) (collateralUtxos . addresses) a b
        }
    , signingKeys = on (<>) signingKeys a b
    }

runIntegrationTest :: Integration a -> MarloweRuntime -> IO a
runIntegrationTest m runtime@MarloweRuntime.MarloweRuntime{protocolConnector} =
  runReaderT (runMarloweT m protocolConnector) runtime

runWebClient :: ClientM a -> Integration (Either ClientError a)
runWebClient client = lift $ ReaderT \runtime -> MarloweRuntime.runWebClient runtime client

expectJust :: MonadFail m => String -> Maybe a -> m a
expectJust msg = \case
  Nothing -> fail msg
  Just a -> pure a

expectRight :: MonadFail m => Show a => String -> Either a b -> m b
expectRight msg = \case
  Left a -> fail $ msg <> ": " <> show a
  Right b -> pure b

expectLeft :: MonadFail m => Show b => String -> Either a b -> m a
expectLeft msg = \case
  Left a -> pure a
  Right b -> fail $ msg <> ": " <> show b

testnet :: Integration LocalTestnet
testnet = asks MarloweRuntime.testnet

getStakeCredential :: Int -> Integration C.StakeCredential
getStakeCredential nodeNum = do
  LocalTestnet{..} <- testnet
  let SpoNode{..} = spoNodes !! nodeNum
  C.StakeCredentialByKey <$> liftIO do
    mTextEnvelope <- decodeFileStrict stakingRewardVKey
    textEnvelope <- expectJust "Failed to decode staking verification key" mTextEnvelope
    stakeKey <- expectRight "Failed to decode text envelope staking vkey"
      $ deserialiseFromTextEnvelope (AsVerificationKey AsStakeKey) textEnvelope
    pure $ verificationKeyHash stakeKey

getGenesisWallet :: Int -> Integration Wallet
getGenesisWallet walletIx = do
  LocalTestnet{..} <- testnet
  let PaymentKeyPair{..} = wallets !! walletIx
  mAddress <- fromBech32 . fromString <$> execCli
    [ "address", "build"
    , "--verification-key-file", paymentVKey
    , "--testnet-magic", "1"
    ]
  address <- expectJust "Failed to decode address" mAddress
  mTextEnvelope <- liftIO $ decodeFileStrict paymentSKey
  textEnvelope <- expectJust "Failed to decode signing key text envelope" mTextEnvelope
  genesisUTxOKey <- expectRight  "failed to decode text envelope"
    $ deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope
  pure Wallet
    { addresses = WalletAddresses address mempty mempty
    , signingKeys = [WitnessGenesisUTxOKey genesisUTxOKey]
    }

newtype CliHash = CliHash { unCliHash :: ByteString }

instance FromJSON CliHash where
  parseJSON = \case
    String s -> either (parseFail . T.unpack) (pure . CliHash) $ decodeBase16 $ encodeUtf8 s
    _ -> parseFail "Expected a string"

data CliBlockHeader = CliBlockHeader
  { block :: Word64
  , hash :: CliHash
  , slot :: Word64
  }
  deriving (Generic, FromJSON)

getTip :: Integration BlockHeader
getTip = do
  LocalTestnet{..} <- testnet
  let SpoNode{..} = head spoNodes
  output <- bracket_ (setEnv "CARDANO_NODE_SOCKET_PATH" socket) (unsetEnv "CARDANO_NODE_SOCKET_PATH") $ liftIO $ execCli
    [ "query", "tip"
    , "--testnet-magic", show testnetMagic
    ]
  CliBlockHeader{..} <- expectRight "Failed to decode tip" $ eitherDecodeStrict $ T.encodeUtf8 $ T.pack output
  pure $ BlockHeader (SlotNo slot) (BlockHeaderHash $ unCliHash hash) (BlockNo block)

submit
  :: Wallet
  -> TxBody BabbageEra
  -> Integration BlockHeader
submit wallet = expectRight "failed to submit tx" <=< submit' wallet

submit'
  :: Wallet
  -> TxBody BabbageEra
  -> Integration (Either SubmitError BlockHeader)
submit' Wallet{..} txBody = do
  let tx = signShelleyTransaction txBody signingKeys
  runMarloweTxClient $ liftCommandWait $ Submit tx

deposit
  :: Wallet
  -> ContractId
  -> Party
  -> Party
  -> Token
  -> Integer
  -> Integration (InputsApplied BabbageEra 'V1)
deposit Wallet{..} contractId intoAccount fromParty ofToken quantity = do
  result <- applyInputs
    MarloweV1
    addresses
    contractId
    emptyMarloweTransactionMetadata
    [NormalInput $ IDeposit intoAccount fromParty ofToken quantity]
  expectRight "Failed to create deposit transaction" result

choose
  :: Wallet
  -> ContractId
  -> PV2.BuiltinByteString
  -> Party
  -> Integer
  -> Integration (InputsApplied BabbageEra 'V1)
choose Wallet{..} contractId choice party chosenNum = do
  result <- applyInputs
    MarloweV1
    addresses
    contractId
    emptyMarloweTransactionMetadata
    [NormalInput $ IChoice (ChoiceId choice party) chosenNum]
  expectRight "Failed to create choice transaction" result

notify
  :: Wallet
  -> ContractId
  -> Integration (InputsApplied BabbageEra 'V1)
notify Wallet{..} contractId = do
  result <- applyInputs
    MarloweV1
    addresses
    contractId
    emptyMarloweTransactionMetadata
    [NormalInput INotify]
  expectRight "Failed to create notify transaction" result

withdraw
  :: Wallet
  -> ContractId
  -> TokenName
  -> Integration (TxBody BabbageEra)
withdraw Wallet{..} contractId role = do
  result <- Client.withdraw MarloweV1 addresses contractId role
  expectRight "Failed to create withdraw transaction" result

timeout :: NominalDiffTime
timeout = secondsToNominalDiffTime 2

retryDelayMicroSeconds :: Int
retryDelayMicroSeconds = 100_000

contractCreatedToCreateStep :: ContractCreated BabbageEra v -> CreateStep v
contractCreatedToCreateStep ContractCreated{..} = CreateStep
  { createOutput = TransactionScriptOutput
      { address = marloweScriptAddress
      , assets = Assets 2_000_000 mempty
      , utxo = unContractId contractId
      , datum
      }
  , metadata
  , payoutValidatorHash = payoutScriptHash
  }

inputsAppliedToTransaction :: BlockHeader -> InputsApplied BabbageEra v -> Transaction v
inputsAppliedToTransaction blockHeader InputsApplied{..} = Transaction
  { transactionId = fromCardanoTxId $ getTxId txBody
  , contractId
  , metadata
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
  , metadata
  , marloweScriptHash
  , marloweScriptAddress
  , payoutScriptHash
  , marloweVersion = SomeMarloweVersion version
  , blockHeader
  }

headerSyncPollExpectWait
  :: MonadFail m
  => m (HeaderSync.ClientStWait m a)
  -> HeaderSync.ClientStWait m a
headerSyncPollExpectWait = HeaderSync.SendMsgPoll . headerSyncExpectWait

headerSyncRequestNextExpectWait
  :: MonadFail m
  => m (HeaderSync.ClientStWait m a)
  -> HeaderSync.ClientStIdle m a
headerSyncRequestNextExpectWait = HeaderSync.SendMsgRequestNext . headerSyncExpectWait

headerSyncPollExpectNewHeaders
  :: (MonadFail m, MonadIO m)
  => BlockHeader
  -> [ContractHeader]
  -> m (HeaderSync.ClientStIdle m a)
  -> m (HeaderSync.ClientStWait m a)
headerSyncPollExpectNewHeaders block headers next = HeaderSync.SendMsgPoll <$> headerSyncExpectNewHeaders \block' headers' -> do
  liftIO $ block' `shouldBe` block
  liftIO $ headers' `shouldBe` headers
  next

headerSyncRequestNextExpectNewHeaders
  :: (MonadFail m, MonadIO m)
  => BlockHeader
  -> [ContractHeader]
  -> m (HeaderSync.ClientStIdle m a)
  -> m (HeaderSync.ClientStIdle m a)
headerSyncRequestNextExpectNewHeaders block headers next = HeaderSync.SendMsgRequestNext <$> headerSyncExpectNewHeaders \block' headers' -> do
  liftIO $ block' `shouldBe` block
  liftIO $ headers' `shouldBe` headers
  next

headerSyncExpectWait
  :: MonadFail m
  => m (HeaderSync.ClientStWait m a)
  -> HeaderSync.ClientStNext m a
headerSyncExpectWait action = HeaderSync.ClientStNext
  { recvMsgNewHeaders = \_ _ -> fail "Expected wait, got new headers"
  , recvMsgRollBackward = \_ -> fail "Expected wait, got roll backward"
  , recvMsgWait = action
  }

headerSyncExpectNewHeaders
  :: (MonadIO m, MonadFail m)
  => (BlockHeader -> [ContractHeader] -> m (HeaderSync.ClientStIdle m a))
  -> m (HeaderSync.ClientStNext m a)
headerSyncExpectNewHeaders recvMsgNewHeaders = do
  startTime <- liftIO getCurrentTime
  let
    next = HeaderSync.ClientStNext
      { recvMsgNewHeaders
      , recvMsgRollBackward = \_ -> fail "Expected new headers, got roll backward"
      , recvMsgWait = do
          time <- liftIO getCurrentTime
          if (time `diffUTCTime` startTime) > timeout
            then fail "Expected new headers, got wait"
            else do
              liftIO $ threadDelay retryDelayMicroSeconds
              pure $ HeaderSync.SendMsgPoll next
      }
  pure next

marloweSyncExpectContractFound
  :: MonadFail m
  => (forall v. BlockHeader -> MarloweVersion v -> CreateStep v -> m (MarloweSync.ClientStIdle v m a))
  -> MarloweSync.ClientStFollow m a
marloweSyncExpectContractFound recvMsgContractFound = MarloweSync.ClientStFollow
  { recvMsgContractNotFound = fail "Expected contract found, got contract not found"
  , recvMsgContractFound
  }

marloweSyncExpectRollForward
  :: (MonadFail m, MonadIO m)
  => (BlockHeader -> [ContractStep v] -> m (MarloweSync.ClientStIdle v m a))
  -> m (MarloweSync.ClientStNext v m a)
marloweSyncExpectRollForward recvMsgRollForward = do
  startTime <- liftIO getCurrentTime
  let
    next = MarloweSync.ClientStNext
      { recvMsgRollBackCreation = fail "Expected roll forward, got roll back creation"
      , recvMsgRollBackward = \_ -> fail "Expected roll forward, got roll backward"
      , recvMsgWait = do
          time <- liftIO getCurrentTime
          if (time `diffUTCTime` startTime) > timeout
            then fail "Expected roll forward, got wait"
            else do
              liftIO $ threadDelay retryDelayMicroSeconds
              pure $ MarloweSync.SendMsgPoll next
      , recvMsgRollForward
      }
  pure next

headerSyncIntersectExpectNotFound :: [BlockHeader] -> Integration ()
headerSyncIntersectExpectNotFound points = runMarloweHeaderSyncClient
  $ HeaderSync.MarloweHeaderSyncClient
  $ pure
  $ HeaderSync.SendMsgIntersect points HeaderSync.ClientStIntersect
    { recvMsgIntersectNotFound = pure $ HeaderSync.SendMsgDone ()
    , recvMsgIntersectFound = \_ -> fail "Expected intersect not found, got intersect found"
    }

headerSyncIntersectExpectFound
  :: [BlockHeader]
  -> BlockHeader
  -> [BlockHeader]
  -> Integration ()
headerSyncIntersectExpectFound points expectedIntersection remainingPoints = runMarloweHeaderSyncClient
  $ HeaderSync.MarloweHeaderSyncClient
  $ pure
  $ HeaderSync.SendMsgIntersect points HeaderSync.ClientStIntersect
    { recvMsgIntersectNotFound = fail "Expected intersect found, got intersect not found"
    , recvMsgIntersectFound = \actualIntersection -> do
        liftIO $ actualIntersection `shouldBe` expectedIntersection
        expectRemainingPoints remainingPoints
    }
  where
    expectRemainingPoints [] = pure
      $ headerSyncRequestNextExpectWait
      $ pure
      $ HeaderSync.SendMsgCancel
      $ HeaderSync.SendMsgDone ()
    expectRemainingPoints (x : xs) = HeaderSync.SendMsgRequestNext <$> headerSyncExpectNewHeaders \block _ -> do
      liftIO $ block `shouldBe` x
      expectRemainingPoints xs

marloweSyncIntersectExpectNotFound :: ContractId -> [BlockHeader] -> Integration ()
marloweSyncIntersectExpectNotFound contractId points = runMarloweSyncClient
  $ MarloweSync.MarloweSyncClient
  $ pure
  $ MarloweSync.SendMsgIntersect contractId MarloweV1 points MarloweSync.ClientStIntersect
    { recvMsgIntersectNotFound = pure ()
    , recvMsgIntersectFound = \_ -> fail "Expected intersect not found, got intersect found"
    }

marloweSyncIntersectExpectFound
  :: ContractId
  -> [BlockHeader]
  -> BlockHeader
  -> [BlockHeader]
  -> Integration ()
marloweSyncIntersectExpectFound contractId points expectedIntersection remainingPoints = runMarloweSyncClient
  $ MarloweSync.MarloweSyncClient
  $ pure
  $ MarloweSync.SendMsgIntersect contractId MarloweV1 points MarloweSync.ClientStIntersect
    { recvMsgIntersectNotFound = fail "Expected intersect found, got intersect not found"
    , recvMsgIntersectFound = \actualIntersection -> do
        liftIO $ actualIntersection `shouldBe` expectedIntersection
        expectRemainingPoints remainingPoints
    }
  where
    expectRemainingPoints [] = pure
      $ marloweSyncRequestNextExpectWait
      $ pure
      $ MarloweSync.SendMsgCancel
      $ MarloweSync.SendMsgDone ()
    expectRemainingPoints (x : xs) = MarloweSync.SendMsgRequestNext <$> marloweSyncExpectRollForward \block _ -> do
      liftIO $ block `shouldBe` x
      expectRemainingPoints xs

marloweSyncPollExpectWait
  :: MonadFail m => m (MarloweSync.ClientStWait v m a)
  -> MarloweSync.ClientStWait v m a
marloweSyncPollExpectWait = MarloweSync.SendMsgPoll . marloweSyncExpectWait

marloweSyncPollExpectRollForward
  :: (Show (ContractStep v), Eq (ContractStep v), MonadIO m, MonadFail m)
  => BlockHeader
  -> [ContractStep v]
  -> m (MarloweSync.ClientStIdle v m a)
  -> m (MarloweSync.ClientStWait v m a)
marloweSyncPollExpectRollForward expectedBlock expectedSteps next =
  MarloweSync.SendMsgPoll <$> marloweSyncExpectRollForward \actualBlock actualSteps -> do
    liftIO $ actualBlock `shouldBe` expectedBlock
    liftIO $ actualSteps `shouldBe` expectedSteps
    next

marloweSyncRequestNextExpectWait
  :: MonadFail m => m (MarloweSync.ClientStWait v m a)
  -> MarloweSync.ClientStIdle v m a
marloweSyncRequestNextExpectWait = MarloweSync.SendMsgRequestNext . marloweSyncExpectWait

marloweSyncRequestNextExpectRollForward
  :: (Show (ContractStep v), Eq (ContractStep v), MonadIO m, MonadFail m)
  => BlockHeader
  -> [ContractStep v]
  -> m (MarloweSync.ClientStIdle v m a)
  -> m (MarloweSync.ClientStIdle v m a)
marloweSyncRequestNextExpectRollForward expectedBlock expectedSteps next =
  MarloweSync.SendMsgRequestNext <$> marloweSyncExpectRollForward \actualBlock actualSteps -> do
    liftIO $ actualBlock `shouldBe` expectedBlock
    liftIO $ actualSteps `shouldBe` expectedSteps
    next

marloweSyncExpectWait
  :: MonadFail m => m (MarloweSync.ClientStWait v m a)
  -> MarloweSync.ClientStNext v m a
marloweSyncExpectWait recvMsgWait = MarloweSync.ClientStNext
  { recvMsgRollBackCreation = fail "Expected wait, got roll back creation"
  , recvMsgRollBackward = \_ -> fail "Expected wait, got roll backward"
  , recvMsgWait
  , recvMsgRollForward = \_ _ -> fail "Expected wait, got roll forward"
  }

prepareCliArgs :: [String] -> Integration [String]
prepareCliArgs args = do
  MarloweRuntime{proxyPort} <- Reader.ask
  pure $ ["--marlowe-runtime-port", show proxyPort] <> args

execMarlowe :: [String] -> Integration String
execMarlowe = exec "marlowe-runtime-cli" <=< prepareCliArgs

execMarlowe_ :: [String] -> Integration ()
execMarlowe_ = void . execMarlowe

execMarlowe' :: [String] -> Integration (ExitCode, String, String)
execMarlowe' = exec' "marlowe-runtime-cli" <=< prepareCliArgs
