{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}

module Language.Marlowe.Runtime.Integration.Common (
  allocateWallet,
  contractCreatedToContractHeader,
  marloweSyncRequestNextExpectWait,
  contractCreatedToCreateStep,
  expectLeft,
  expectJust,
  expectRight,
  getGenesisWallet,
  getStakeCredential,
  getTip,
  inputsAppliedToTransaction,
  retryDelayMicroSeconds,
  runIntegrationTest,
  submitBuilder,
  submit',
  testnet,
  timeout,
  Wallet (..),
  withCurrentEra,
  withdraw,
  notify,
  buildBurnRoleTokensTx,
  choose,
  deposit,
  submit,
  bulkSyncRequestNextNExpectRollForward,
  bulkSyncRequestNextExpectRollForward,
  bulkSyncPollExpectWait,
  bulkSyncRequestNextExpectWait,
  headerSyncRequestNextExpectWait,
  headerSyncPollExpectNewHeaders,
  headerSyncRequestNextExpectNewHeaders,
  headerSyncPollExpectWait,
  headerSyncExpectWait,
  Integration,
  bulkSyncPollExpectRollForward,
  marloweSyncExpectContractFound,
  marloweSyncExpectRollForward,
  headerSyncIntersectExpectNotFound,
  headerSyncIntersectExpectFound,
  marloweSyncIntersectExpectNotFound,
  marloweSyncIntersectExpectFound,
  marloweSyncPollExpectRollForward,
  marloweSyncPollExpectWait,
  marloweSyncRequestNextExpectRollForward,
  prepareCliArgs,
  execMarlowe,
  execMarlowe_,
  execMarlowe',
  runWebClient,
  runWebClient',
  expectJustM,
  waitTillWalletHasFunds,
  mkEmptyWallet,
  prettyJSON,
) where

import Cardano.Api (
  AddressAny (AddressShelley),
  AnyCardanoEra (..),
  AsType (..),
  BabbageEraOnwards,
  File (..),
  Key (verificationKeyHash),
  NetworkId (Testnet),
  NetworkMagic (NetworkMagic),
  PaymentCredential (PaymentCredentialByKey),
  ShelleyWitnessSigningKey (..),
  StakeAddressReference (NoStakeAddress),
  TxBody (..),
  TxBodyContent (..),
  generateSigningKey,
  getTxId,
  getVerificationKey,
  makeShelleyAddress,
  signShelleyTransaction,
  toLedgerEpochInfo,
 )
import qualified Cardano.Api as C
import Cardano.Api.Byron (deserialiseFromTextEnvelope)
import qualified Cardano.Api.Shelley as C
import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData)
import Control.Monad (guard, void, when, (<=<))
import Control.Monad.Event.Class (Inject (inject), NoopEventT (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..), ask, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.Reader.Class (asks)
import Control.Monad.State (StateT, runStateT, state)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Marlowe (MarloweT, runMarloweT)
import Control.Monad.Trans.Marlowe.Class (
  applyInputs,
  runMarloweHeaderSyncClient,
  runMarloweSyncClient,
  runMarloweTxClient,
 )
import Data.Aeson (FromJSON (..), Value (..), decodeFileStrict, eitherDecodeStrict)
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A
import Data.Aeson.Types (parseFail)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 (decodeBase16)
import qualified Data.ByteString.Lazy as LBS
import Data.Either (isRight)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor (($>))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Traversable (for)
import Data.Word (Word64, Word8)
import GHC.Generics (Generic)
import Language.Marlowe (ChoiceId (..), Input (..), InputContent (..), Party, Token)
import qualified Language.Marlowe.Protocol.BulkSync.Client as BulkSync
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HeaderSync
import qualified Language.Marlowe.Protocol.Sync.Client as MarloweSync
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressAny,
  fromCardanoTxId,
  toCardanoAddressAny,
  toCardanoAddressInEra,
  toCardanoTxOut,
 )
import Language.Marlowe.Runtime.Cardano.Feature (withShelleyBasedEra)
import Language.Marlowe.Runtime.ChainSync.Api (
  Assets (..),
  BlockHeader (..),
  BlockHeaderHash (..),
  BlockNo (..),
  SlotNo (..),
  TxId,
  TxOutRef,
  fromBech32,
  mkTxOutAssets,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import qualified Language.Marlowe.Runtime.Client as Client
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  SomeMarloweVersion (..),
  Transaction (..),
  TransactionScriptOutput (..),
  emptyMarloweTransactionMetadata,
 )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader (..))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep (..), MarloweBlock)
import Language.Marlowe.Runtime.Transaction.Api (
  BurnRoleTokensTx,
  ContractCreated (..),
  ContractCreatedInEra (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  MarloweTxCommand (..),
  RoleTokenFilter,
  SubmitError,
  WalletAddresses (..),
  WithdrawTx (..),
 )
import Language.Marlowe.Runtime.Transaction.Constraints (WalletContext (availableUtxos))
import qualified Language.Marlowe.Runtime.Transaction.Query as Transaction
import Network.Protocol.Connection (runConnector)
import Network.Protocol.Driver.Trace (HasSpanContext (context, wrapContext))
import Network.Protocol.Job.Client (liftCommandWait)
import Network.Protocol.Peer.Trace (defaultSpanContext)
import Network.Protocol.Query.Client (request)
import Observe.Event.Explicit (injectSelector)
import Ouroboros.Network.Protocol.LocalStateQuery.Type (Target (VolatileTip))
import qualified PlutusLedgerApi.V2 as PV2
import Servant.Client (ClientError)
import Servant.Client.Streaming (ClientM)
import System.Exit (ExitCode (..))
import Test.Hspec (shouldBe)
import Test.Integration.Marlowe.Local (
  Attempts (..),
  LocalTestnet (..),
  MarloweRuntime (MarloweRuntime),
  PaymentKeyPair (..),
  SpoNode (..),
  exec,
  exec',
  execCli,
  retryTillTrue,
 )
import qualified Test.Integration.Marlowe.Local as MarloweRuntime
import UnliftIO (bracket_)
import UnliftIO.Environment (setEnv, unsetEnv)

data RuntimeRef = RuntimeRef

instance Semigroup RuntimeRef where
  (<>) = const

instance Monoid RuntimeRef where
  mempty = RuntimeRef

instance HasSpanContext RuntimeRef where
  context _ = pure defaultSpanContext
  wrapContext _ = RuntimeRef

data RuntimeSelector f where
  AnyEvent :: s f -> RuntimeSelector f

instance Inject s RuntimeSelector where
  inject = injectSelector AnyEvent

type Integration = ReaderT MarloweRuntime (MarloweT IO)

mkEmptyWallet :: Integration Wallet
mkEmptyWallet = do
  signingKey <- liftIO $ generateSigningKey AsPaymentKey
  networkId' <- networkId
  let verificationKey = getVerificationKey signingKey
  let paymentCredential = PaymentCredentialByKey $ verificationKeyHash verificationKey
  let address = fromCardanoAddressAny $ AddressShelley $ makeShelleyAddress networkId' paymentCredential NoStakeAddress
  pure $
    Wallet
      { addresses = WalletAddresses address mempty mempty
      , signingKeys = [WitnessPaymentKey signingKey]
      }

-- Important - the TxId is lazily computed and depends on the resulting Tx. So
-- it should not be used to compute the transaction outputs written.
type TxBuilder = ReaderT TxId (StateT [Chain.TransactionOutput] Integration)

allocateWallet :: [[(Bool, Integer)]] -> TxBuilder Wallet
allocateWallet balances = do
  txId <- ask
  (addresses, signingKeys, collateralUtxos) <-
    unzip3 <$> for balances \utxos -> do
      signingKey <- liftIO $ generateSigningKey AsPaymentKey
      let verificationKey = getVerificationKey signingKey
      let paymentCredential = PaymentCredentialByKey $ verificationKeyHash verificationKey
      networkId' <- lift $ lift networkId
      let address = fromCardanoAddressAny $ AddressShelley $ makeShelleyAddress networkId' paymentCredential NoStakeAddress
      collateralUtxos <-
        Set.fromList . catMaybes <$> for utxos \(isCollateral, balance) -> state \outputs ->
          ( guard isCollateral $> Chain.TxOutRef txId (Chain.TxIx $ fromIntegral $ length outputs)
          , do
              let assets = fold $ mkTxOutAssets $ Assets (Chain.Lovelace balance) mempty
              Chain.TransactionOutput address assets Nothing Nothing : outputs
          )
      pure (address, WitnessPaymentKey signingKey, collateralUtxos)
  pure
    Wallet
      { addresses = WalletAddresses (head addresses) (Set.fromList $ tail addresses) (fold collateralUtxos)
      , signingKeys
      }

submitBuilder :: forall a. Wallet -> TxBuilder a -> Integration (BlockHeader, a)
submitBuilder wallet builder = withCurrentEra go
  where
    go :: forall era. C.ShelleyBasedEra era -> Integration (BlockHeader, a)
    go era = mdo
      -- Note - the txId is not evaluated yet - we're referring to it lazily.
      (a, txOuts) <- runStateT (runReaderT builder txId) []
      utxo <- getCardanoNodeUTxO era wallet
      multiAsset <- C.inEonForShelleyBasedEra (fail $ "Era not Mary or later" <> show era) pure era
      referenceScripts <- C.inEonForShelleyBasedEra (fail $ "Era not Babbage or later" <> show era) pure era
      let txBodyContent =
            (C.defaultTxBodyContent era)
              { txIns =
                  (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) . fst
                    <$> Map.toList (C.unUTxO utxo)
              , txOuts = fromJust . toCardanoTxOut multiAsset <$> reverse txOuts
              }
      txBody <- balanceTx era wallet utxo txBodyContent
      let txId = fromCardanoTxId $ getTxId txBody
      (,a) <$> submit wallet referenceScripts txBody

withCurrentEra :: (forall era. C.ShelleyBasedEra era -> Integration a) -> Integration a
withCurrentEra f = do
  AnyCardanoEra era <- queryNode 0 C.QueryCurrentEra
  C.inEonForEra (fail "Still in byron") f era

balanceTx :: C.ShelleyBasedEra era -> Wallet -> C.UTxO era -> TxBodyContent C.BuildTx era -> Integration (TxBody era)
balanceTx era (Wallet WalletAddresses{..} _) utxo txBodyContent = do
  start <- queryNode 0 C.QuerySystemStart
  history <- queryNode 0 C.QueryEraHistory
  protocol <- queryShelley 0 $ C.QueryInShelleyBasedEra era C.QueryProtocolParameters
  changeAddr <-
    expectJust "Could not convert to Cardano address" $ toCardanoAddressInEra (C.shelleyBasedToCardanoEra era) changeAddress
  C.BalancedTxBody _ txBody _ _ <-
    withShelleyBasedEra era $
      expectRight "Failed to balance Tx" $
        C.makeTransactionBodyAutoBalance
          era
          start
          (toLedgerEpochInfo history)
          (C.LedgerProtocolParameters protocol)
          mempty
          mempty
          mempty
          utxo
          txBodyContent
          changeAddr
          Nothing
  pure txBody

getCardanoNodeUTxO :: C.ShelleyBasedEra era -> Wallet -> Integration (C.UTxO era)
getCardanoNodeUTxO era (Wallet WalletAddresses{..} _) =
  queryShelley 0 $
    C.QueryInShelleyBasedEra era $
      C.QueryUTxO $
        C.QueryUTxOByAddress $
          Set.insert (fromJust $ toCardanoAddressAny changeAddress) $
            Set.map (fromJust . toCardanoAddressAny) extraAddresses

queryShelley :: Int -> C.QueryInEra era a -> Integration a
queryShelley nodeNum = either (fail . show) pure <=< queryNode nodeNum . C.QueryInEra

queryNode :: Int -> C.QueryInMode a -> Integration a
queryNode nodeNum query = do
  connectInfo <- nodeConnectInfo nodeNum
  liftIO $ either (fail . show) pure =<< C.queryNodeLocalState connectInfo VolatileTip query

nodeConnectInfo :: Int -> Integration C.LocalNodeConnectInfo
nodeConnectInfo nodeNum = do
  LocalTestnet{..} <- testnet
  let SpoNode{..} = spoNodes !! nodeNum
  C.LocalNodeConnectInfo (C.CardanoModeParams $ C.EpochSlots 21600) <$> networkId <*> pure (File socket)

networkId :: Integration NetworkId
networkId = Testnet . NetworkMagic . fromIntegral . testnetMagic <$> testnet

data Wallet = Wallet
  { addresses :: WalletAddresses
  , signingKeys :: [ShelleyWitnessSigningKey]
  }

instance Semigroup Wallet where
  a <> b =
    Wallet
      { addresses =
          WalletAddresses
            { changeAddress = changeAddress $ addresses a
            , extraAddresses = Set.insert (changeAddress (addresses b)) $ on (<>) (extraAddresses . addresses) a b
            , collateralUtxos = on (<>) (collateralUtxos . addresses) a b
            }
      , signingKeys = on (<>) signingKeys a b
      }

runIntegrationTest :: Integration a -> MarloweRuntime -> IO a
runIntegrationTest m runtime@MarloweRuntime.MarloweRuntime{protocolConnector} =
  runMarloweT (runReaderT m runtime) protocolConnector

runWebClient :: (NFData a) => ClientM a -> Integration (Either ClientError a)
runWebClient client = ReaderT \runtime -> liftIO $ MarloweRuntime.runWebClient runtime client

runWebClient' :: (NFData a) => ClientM a -> Integration a
runWebClient' clientM = do
  res <- runWebClient clientM
  case res of
    Left err -> fail $ show err
    Right a -> pure a

expectJust :: (MonadFail m) => String -> Maybe a -> m a
expectJust msg = \case
  Nothing -> fail msg
  Just a -> pure a

expectJustM :: (MonadFail m) => String -> m (Maybe a) -> m a
expectJustM msg = (expectJust msg =<<)

expectRight :: (MonadFail m) => (Show a) => String -> Either a b -> m b
expectRight msg = \case
  Left a -> fail $ msg <> ": " <> show a
  Right b -> pure b

expectLeft :: (MonadFail m) => (Show b) => String -> Either a b -> m a
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
    stakeKey <-
      expectRight "Failed to decode text envelope staking vkey" $
        deserialiseFromTextEnvelope (AsVerificationKey AsStakeKey) textEnvelope
    pure $ verificationKeyHash stakeKey

getGenesisWallet :: Int -> Integration Wallet
getGenesisWallet walletIx = do
  LocalTestnet{..} <- testnet
  when (walletIx >= length wallets) do
    fail $ "Genesis wallet index out of bounds. Maximal index: " <> show (length wallets - 1)
  let PaymentKeyPair{..} = wallets !! walletIx
  mAddress <-
    fromBech32 . fromString
      <$> execCli
        [ "address"
        , "build"
        , "--verification-key-file"
        , paymentVKey
        , "--testnet-magic"
        , "1"
        ]
  address <- expectJust "Failed to decode address" mAddress
  mTextEnvelope <- liftIO $ decodeFileStrict paymentSKey
  textEnvelope <- expectJust "Failed to decode signing key text envelope" mTextEnvelope
  genesisUTxOKey <-
    expectRight "failed to decode text envelope" $
      deserialiseFromTextEnvelope (AsSigningKey AsGenesisUTxOKey) textEnvelope
  pure
    Wallet
      { addresses = WalletAddresses address mempty mempty
      , signingKeys = [WitnessGenesisUTxOKey genesisUTxOKey]
      }

newtype CliHash = CliHash {unCliHash :: ByteString}

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
  output <-
    bracket_ (setEnv "CARDANO_NODE_SOCKET_PATH" socket) (unsetEnv "CARDANO_NODE_SOCKET_PATH") $
      liftIO $
        execCli
          [ "query"
          , "tip"
          , "--testnet-magic"
          , show testnetMagic
          ]
  CliBlockHeader{..} <- expectRight "Failed to decode tip" $ eitherDecodeStrict $ T.encodeUtf8 $ T.pack output
  pure $ BlockHeader (SlotNo slot) (BlockHeaderHash $ unCliHash hash) (BlockNo block)

submit
  :: Wallet
  -> BabbageEraOnwards era
  -> TxBody era
  -> Integration BlockHeader
submit era wallet = expectRight "failed to submit tx" <=< submit' era wallet

submit'
  :: Wallet
  -> BabbageEraOnwards era
  -> TxBody era
  -> Integration (Either SubmitError BlockHeader)
submit' Wallet{..} era txBody = do
  let tx = signShelleyTransaction (C.babbageEraOnwardsToShelleyBasedEra era) txBody signingKeys
  res <- runMarloweTxClient $ liftCommandWait $ Submit era tx
  let cTxId = C.getTxId txBody
      txId = fromCardanoTxId cTxId
  when (isRight res) do
    waitForTx txId
  pure res

deposit
  :: Wallet
  -> ContractId
  -> Party
  -> Party
  -> Token
  -> Integer
  -> Integration (InputsApplied 'V1)
deposit Wallet{..} contractId intoAccount fromParty ofToken quantity = do
  result <-
    applyInputs
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
  -> Integration (InputsApplied 'V1)
choose Wallet{..} contractId choice party chosenNum = do
  result <-
    applyInputs
      MarloweV1
      addresses
      contractId
      emptyMarloweTransactionMetadata
      [NormalInput $ IChoice (ChoiceId choice party) chosenNum]
  expectRight "Failed to create choice transaction" result

notify
  :: Wallet
  -> ContractId
  -> Integration (InputsApplied 'V1)
notify Wallet{..} contractId = do
  result <-
    applyInputs
      MarloweV1
      addresses
      contractId
      emptyMarloweTransactionMetadata
      [NormalInput INotify]
  expectRight "Failed to create notify transaction" result

withdraw
  :: Wallet
  -> Set TxOutRef
  -> Integration (WithdrawTx 'V1)
withdraw Wallet{..} payouts = do
  result <- Client.withdraw MarloweV1 addresses payouts
  expectRight "Failed to create withdraw transaction" result

buildBurnRoleTokensTx
  :: Wallet
  -> RoleTokenFilter
  -> Integration (BurnRoleTokensTx 'V1)
buildBurnRoleTokensTx Wallet{..} tokenFilter = do
  result <- Client.buildBurnRoleTokensTx MarloweV1 addresses tokenFilter
  expectRight "Failed to create burn Role Tokens transaction" result

timeout :: NominalDiffTime
timeout = secondsToNominalDiffTime 2

retryDelayMicroSeconds :: Int
retryDelayMicroSeconds = 100_000

contractCreatedToCreateStep :: ContractCreated v -> CreateStep v
contractCreatedToCreateStep (ContractCreated _ ContractCreatedInEra{..}) =
  CreateStep
    { createOutput =
        TransactionScriptOutput
          { address = marloweScriptAddress
          , assets
          , utxo = unContractId contractId
          , datum
          }
    , metadata
    , payoutValidatorHash = payoutScriptHash
    }

inputsAppliedToTransaction :: BlockHeader -> InputsApplied v -> Transaction v
inputsAppliedToTransaction blockHeader (InputsApplied _ InputsAppliedInEra{..}) =
  Transaction
    { transactionId = fromCardanoTxId $ getTxId txBody
    , contractId
    , metadata
    , blockHeader
    , validityLowerBound = invalidBefore
    , validityUpperBound = invalidHereafter
    , inputs
    , output
    }

contractCreatedToContractHeader :: BlockHeader -> ContractCreated v -> ContractHeader
contractCreatedToContractHeader blockHeader (ContractCreated _ ContractCreatedInEra{..}) =
  ContractHeader
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
  :: (MonadFail m)
  => m (HeaderSync.ClientStWait m a)
  -> HeaderSync.ClientStWait m a
headerSyncPollExpectWait = HeaderSync.SendMsgPoll . headerSyncExpectWait

headerSyncRequestNextExpectWait
  :: (MonadFail m)
  => m (HeaderSync.ClientStWait m a)
  -> HeaderSync.ClientStIdle m a
headerSyncRequestNextExpectWait = HeaderSync.SendMsgRequestNext . headerSyncExpectWait

bulkSyncRequestNextExpectWait
  :: (MonadFail m)
  => m (BulkSync.ClientStPoll m a)
  -> BulkSync.ClientStIdle m a
bulkSyncRequestNextExpectWait = BulkSync.SendMsgRequestNext 0 . bulkSyncExpectWait

bulkSyncPollExpectWait
  :: (MonadFail m)
  => m (BulkSync.ClientStPoll m a)
  -> BulkSync.ClientStPoll m a
bulkSyncPollExpectWait = BulkSync.SendMsgPoll . bulkSyncExpectWait

bulkSyncRequestNextExpectRollForward
  :: (MonadFail m, MonadIO m)
  => [MarloweBlock]
  -> m (BulkSync.ClientStIdle m a)
  -> m (BulkSync.ClientStIdle m a)
bulkSyncRequestNextExpectRollForward = bulkSyncRequestNextNExpectRollForward 0

bulkSyncRequestNextNExpectRollForward
  :: (MonadFail m, MonadIO m)
  => Word8
  -> [MarloweBlock]
  -> m (BulkSync.ClientStIdle m a)
  -> m (BulkSync.ClientStIdle m a)
bulkSyncRequestNextNExpectRollForward extraBlockCount blocks next =
  BulkSync.SendMsgRequestNext extraBlockCount <$> bulkSyncExpectRollForward \blocks' _ -> do
    liftIO $ blocks' `shouldBe` blocks
    next

headerSyncPollExpectNewHeaders
  :: (MonadFail m, MonadIO m)
  => BlockHeader
  -> [ContractHeader]
  -> m (HeaderSync.ClientStIdle m a)
  -> m (HeaderSync.ClientStWait m a)
headerSyncPollExpectNewHeaders block headers next =
  HeaderSync.SendMsgPoll <$> headerSyncExpectNewHeaders \block' headers' -> do
    liftIO $ block' `shouldBe` block
    liftIO $ headers' `shouldBe` headers
    next

headerSyncRequestNextExpectNewHeaders
  :: (MonadFail m, MonadIO m)
  => BlockHeader
  -> [ContractHeader]
  -> m (HeaderSync.ClientStIdle m a)
  -> m (HeaderSync.ClientStIdle m a)
headerSyncRequestNextExpectNewHeaders block headers next =
  HeaderSync.SendMsgRequestNext <$> headerSyncExpectNewHeaders \block' headers' -> do
    liftIO $ block' `shouldBe` block
    liftIO $ headers' `shouldBe` headers
    next

bulkSyncPollExpectRollForward
  :: (MonadFail m, MonadIO m)
  => [MarloweBlock]
  -> m (BulkSync.ClientStIdle m a)
  -> m (BulkSync.ClientStPoll m a)
bulkSyncPollExpectRollForward blocks next =
  BulkSync.SendMsgPoll <$> bulkSyncExpectRollForward \blocks' _ -> do
    liftIO $ blocks' `shouldBe` blocks
    next

headerSyncExpectWait
  :: (MonadFail m)
  => m (HeaderSync.ClientStWait m a)
  -> HeaderSync.ClientStNext m a
headerSyncExpectWait action =
  HeaderSync.ClientStNext
    { recvMsgNewHeaders = \_ _ -> fail "Expected wait, got new headers"
    , recvMsgRollBackward = \_ -> fail "Expected wait, got roll backward"
    , recvMsgWait = action
    }

bulkSyncExpectWait
  :: (MonadFail m)
  => m (BulkSync.ClientStPoll m a)
  -> BulkSync.ClientStNext m a
bulkSyncExpectWait action =
  BulkSync.ClientStNext
    { recvMsgRollForward = \_ _ -> fail "Expected wait, got roll forward"
    , recvMsgRollBackward = \_ _ -> fail "Expected wait, got roll backward"
    , recvMsgWait = action
    }

headerSyncExpectNewHeaders
  :: (MonadIO m, MonadFail m)
  => (BlockHeader -> [ContractHeader] -> m (HeaderSync.ClientStIdle m a))
  -> m (HeaderSync.ClientStNext m a)
headerSyncExpectNewHeaders recvMsgNewHeaders = do
  startTime <- liftIO getCurrentTime
  let next =
        HeaderSync.ClientStNext
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

bulkSyncExpectRollForward
  :: (MonadIO m, MonadFail m)
  => ([MarloweBlock] -> Chain.BlockHeader -> m (BulkSync.ClientStIdle m a))
  -> m (BulkSync.ClientStNext m a)
bulkSyncExpectRollForward recvMsgRollForward = do
  startTime <- liftIO getCurrentTime
  let next =
        BulkSync.ClientStNext
          { recvMsgRollForward
          , recvMsgRollBackward = \_ _ -> fail "Expected new headers, got roll backward"
          , recvMsgWait = do
              time <- liftIO getCurrentTime
              if (time `diffUTCTime` startTime) > timeout
                then fail "Expected new headers, got wait"
                else do
                  liftIO $ threadDelay retryDelayMicroSeconds
                  pure $ BulkSync.SendMsgPoll next
          }
  pure next

loadWalletContext :: WalletAddresses -> Integration WalletContext
loadWalletContext walletAddresses = do
  chainSyncQueryConnector <- asks MarloweRuntime.chainSyncQueryConnector
  let runQuery :: Chain.GetUTxOsQuery -> NoopEventT RuntimeRef RuntimeSelector IO Chain.UTxOs
      runQuery = NoopEventT . runConnector chainSyncQueryConnector . request . ChainSync.GetUTxOs
  liftIO $ runNoopEventT $ Transaction.loadWalletContext runQuery walletAddresses

-- Wait till chain sync catches up and the wallet has funds.
waitTillWalletHasFunds :: Wallet -> Integration ()
waitTillWalletHasFunds Wallet{..} = do
  retryTillTrue (Attempts 60) do
    walletContext <- loadWalletContext addresses
    pure $ not . null . Map.toList . Chain.unUTxOs . availableUtxos $ walletContext

queryUTxOs :: Set Chain.TxOutRef -> Integration Chain.UTxOs
queryUTxOs txOutRefs = do
  chainSyncQueryConnector <- asks MarloweRuntime.chainSyncQueryConnector
  let runQuery = runConnector chainSyncQueryConnector . request . ChainSync.GetUTxOs . ChainSync.GetUTxOsForTxOutRefs
  lift $ lift $ runQuery txOutRefs

waitForUTxOs :: Set Chain.TxOutRef -> Integration ()
waitForUTxOs txOutRefs = do
  retryTillTrue (Attempts 60) do
    Chain.UTxOs utxos <- queryUTxOs txOutRefs
    -- check if all the txOutRefs are present in the UTxOs
    pure $ txOutRefs `Set.isSubsetOf` Map.keysSet utxos

waitForTx :: Chain.TxId -> Integration ()
waitForTx txId = do
  let -- TxOut corresponding to the transaction
      txOuts = Set.singleton (Chain.TxOutRef txId (Chain.TxIx 0))
  waitForUTxOs txOuts

marloweSyncExpectContractFound
  :: (MonadFail m)
  => (forall v. BlockHeader -> MarloweVersion v -> CreateStep v -> m (MarloweSync.ClientStIdle v m a))
  -> MarloweSync.ClientStFollow m a
marloweSyncExpectContractFound recvMsgContractFound =
  MarloweSync.ClientStFollow
    { recvMsgContractNotFound = fail "Expected contract found, got contract not found"
    , recvMsgContractFound
    }

marloweSyncExpectRollForward
  :: (MonadFail m, MonadIO m)
  => (BlockHeader -> [ContractStep v] -> m (MarloweSync.ClientStIdle v m a))
  -> m (MarloweSync.ClientStNext v m a)
marloweSyncExpectRollForward recvMsgRollForward = do
  startTime <- liftIO getCurrentTime
  let next =
        MarloweSync.ClientStNext
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
headerSyncIntersectExpectNotFound points =
  runMarloweHeaderSyncClient $
    HeaderSync.MarloweHeaderSyncClient $
      pure $
        HeaderSync.SendMsgIntersect
          points
          HeaderSync.ClientStIntersect
            { recvMsgIntersectNotFound = pure $ HeaderSync.SendMsgDone ()
            , recvMsgIntersectFound = \_ -> fail "Expected intersect not found, got intersect found"
            }

headerSyncIntersectExpectFound
  :: [BlockHeader]
  -> BlockHeader
  -> [BlockHeader]
  -> Integration ()
headerSyncIntersectExpectFound points expectedIntersection remainingPoints =
  runMarloweHeaderSyncClient $
    HeaderSync.MarloweHeaderSyncClient $
      pure $
        HeaderSync.SendMsgIntersect
          points
          HeaderSync.ClientStIntersect
            { recvMsgIntersectNotFound = fail "Expected intersect found, got intersect not found"
            , recvMsgIntersectFound = \actualIntersection -> do
                liftIO $ actualIntersection `shouldBe` expectedIntersection
                expectRemainingPoints remainingPoints
            }
  where
    expectRemainingPoints [] =
      pure $
        headerSyncRequestNextExpectWait $
          pure $
            HeaderSync.SendMsgCancel $
              HeaderSync.SendMsgDone ()
    expectRemainingPoints (x : xs) =
      HeaderSync.SendMsgRequestNext <$> headerSyncExpectNewHeaders \block _ -> do
        liftIO $ block `shouldBe` x
        expectRemainingPoints xs

marloweSyncIntersectExpectNotFound :: ContractId -> [BlockHeader] -> Integration ()
marloweSyncIntersectExpectNotFound contractId points =
  runMarloweSyncClient $
    MarloweSync.MarloweSyncClient $
      pure $
        MarloweSync.SendMsgIntersect
          contractId
          MarloweV1
          points
          MarloweSync.ClientStIntersect
            { recvMsgIntersectNotFound = pure ()
            , recvMsgIntersectFound = \_ -> fail "Expected intersect not found, got intersect found"
            }

marloweSyncIntersectExpectFound
  :: ContractId
  -> [BlockHeader]
  -> BlockHeader
  -> [BlockHeader]
  -> Integration ()
marloweSyncIntersectExpectFound contractId points expectedIntersection remainingPoints =
  runMarloweSyncClient $
    MarloweSync.MarloweSyncClient $
      pure $
        MarloweSync.SendMsgIntersect
          contractId
          MarloweV1
          points
          MarloweSync.ClientStIntersect
            { recvMsgIntersectNotFound = fail "Expected intersect found, got intersect not found"
            , recvMsgIntersectFound = \actualIntersection -> do
                liftIO $ actualIntersection `shouldBe` expectedIntersection
                expectRemainingPoints remainingPoints
            }
  where
    expectRemainingPoints [] =
      pure $
        marloweSyncRequestNextExpectWait $
          pure $
            MarloweSync.SendMsgCancel $
              MarloweSync.SendMsgDone ()
    expectRemainingPoints (x : xs) =
      MarloweSync.SendMsgRequestNext <$> marloweSyncExpectRollForward \block _ -> do
        liftIO $ block `shouldBe` x
        expectRemainingPoints xs

marloweSyncPollExpectWait
  :: (MonadFail m)
  => m (MarloweSync.ClientStWait v m a)
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
  :: (MonadFail m)
  => m (MarloweSync.ClientStWait v m a)
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
  :: (MonadFail m)
  => m (MarloweSync.ClientStWait v m a)
  -> MarloweSync.ClientStNext v m a
marloweSyncExpectWait recvMsgWait =
  MarloweSync.ClientStNext
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

prettyJSON :: A.Value -> T.Text
prettyJSON = T.decodeUtf8 . LBS.toStrict . A.encodePretty
