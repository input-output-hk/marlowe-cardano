{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Benchmark the basic contract lifecycle.
module Language.Marlowe.Runtime.Benchmark.Lifecycle (
  -- * Benchmarking
  runScenario,
  measure,
) where

import Control.Monad (forM_, replicateM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Control.Monad.Trans.Marlowe.Class (MonadMarlowe, applyInputs, createContract, submitAndWait)
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.Bits ((.&.), (.|.))
import Data.Maybe (fromJust, mapMaybe)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input (NormalInput), Party (Address, Role))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Runtime.Benchmark.Lifecycle.Scenario (Scenario (..), randomScenario)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId, toCardanoAddressAny, toCardanoAddressInEra)
import Language.Marlowe.Runtime.ChainSync.Api (Address (unAddress), Quantity (Quantity), TokenName (TokenName), TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion (..), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Transaction.Api (
  ContractCreated (..),
  ContractCreatedInEra (..),
  Destination (ToAddress),
  InputsApplied (..),
  InputsAppliedInEra (..),
  RoleTokensConfig (..),
  WalletAddresses (WalletAddresses),
  mkMint,
 )
import PlutusLedgerApi.V2 (fromBuiltin, unTokenName)
import System.IO (hPutStrLn, stderr)
import Test.QuickCheck (generate, infiniteListOf)
import Test.QuickCheck.Hedgehog (hedgehog)
import UnliftIO (catchIO, forConcurrently)

import Cardano.Api (TxBodyContent (..))
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE (fromList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (Address (Address))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import qualified Test.Gen.Cardano.Api.Typed as C (genShelleyWitnessSigningKey)

data Benchmark = Benchmark
  { metric :: String
  , start :: UTCTime
  , finish :: UTCTime
  , contractsPerSecond :: Double
  , creationsPerSecond :: Double
  , appliesPerSecond :: Double
  , withdrawsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

-- | Measure the performance of the basic contract lifecycle.
measure
  :: (C.IsShelleyBasedEra era)
  => C.SocketPath
  -- ^ The path to the Cardano node socket.
  -> C.BabbageEraOnwards era
  -- ^ The Cardano era.
  -> C.NetworkId
  -- ^ The Cardano network.
  -> Int
  -- ^ The number of clients to run in parallel.
  -> Address
  -- ^ The faucet address.
  -> C.SigningKey C.PaymentExtendedKey
  -- ^ The faucet signing key.
  -> Int
  -> MarloweT IO [Benchmark]
measure node era network parallelism faucetAddress faucetKey count =
  do
    let lovelacePerClient = C.lovelaceToValue . fromIntegral $ 10_000_000 + 1_000_000 * count
        fundAddress' = fundAddress node era network
    keys <- liftIO . replicateM parallelism $ genKey faucetAddress
    fundAddress' faucetAddress faucetKey faucetAddress $
      second (const lovelacePerClient) <$> keys
    result <-
      forConcurrently keys (uncurry $ run "Lifecycle" count)
        `catchIO` (\e -> (liftIO . hPutStrLn stderr $ "Cleaning up after error: " <> show e) >> pure mempty)
    forM_ keys $
      \(address, key) -> fundAddress' address key faucetAddress mempty
    pure result

fundAddress
  :: (C.IsShelleyBasedEra era)
  => C.SocketPath
  -- ^ The path to the Cardano node socket.
  -> C.BabbageEraOnwards era
  -- ^ The Cardano era.
  -> C.NetworkId
  -- ^ The Cardano network.
  -> Address
  -- ^ The faucet address.
  -> C.SigningKey C.PaymentExtendedKey
  -- ^ The faucet signing key.
  -> Address
  -- ^ The change address.
  -> [(Address, C.Value)]
  -- ^ The destination addresses and the value they receive.
  -> MarloweT IO ()
  -- ^ Action for running the transactions.
fundAddress node era network srcAddress srcKey changeAddress dstAddressAmount =
  do
    let local = C.LocalNodeConnectInfo (C.CardanoModeParams $ C.EpochSlots 432_000) network node
    Right systemStart <- liftIO $ C.queryNodeLocalState local Net.Query.VolatileTip C.QuerySystemStart
    Right ledgerEpochInfo <-
      liftIO $
        fmap (fmap C.toLedgerEpochInfo) $
          C.queryNodeLocalState
            local
            Net.Query.VolatileTip
            C.QueryEraHistory
    Right protocol <-
      liftIO $
        C.executeQueryCardanoMode node network $
          C.QueryInEra $
            C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters
    Right utxos <-
      liftIO $
        C.executeQueryCardanoMode node network $
          C.QueryInEra $
            C.QueryInShelleyBasedEra C.shelleyBasedEra $
              C.QueryUTxO $
                C.QueryUTxOByAddress . S.singleton . fromJust $
                  toCardanoAddressAny srcAddress
    let txBodyContent =
          (C.defaultTxBodyContent C.shelleyBasedEra)
            { txIns = second (const . C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) <$> M.toList (C.unUTxO utxos)
            , txOuts =
                [ C.TxOut
                  (fromJust $ toCardanoAddressInEra C.cardanoEra address)
                  ( case era of
                      C.BabbageEraOnwardsBabbage ->
                        C.TxOutValueShelleyBased C.ShelleyBasedEraBabbage $ C.toLedgerValue C.MaryEraOnwardsBabbage amount
                      C.BabbageEraOnwardsConway ->
                        C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toLedgerValue C.MaryEraOnwardsConway amount
                  )
                  C.TxOutDatumNone
                  C.ReferenceScriptNone
                | (address, amount) <- dstAddressAmount
                ]
            , txFee = C.TxFeeExplicit C.shelleyBasedEra 0
            }
        (&) = flip ($)
        txBody =
          C.makeTransactionBodyAutoBalance
            C.shelleyBasedEra
            systemStart
            ledgerEpochInfo
            (C.LedgerProtocolParameters protocol)
            mempty
            mempty
            mempty
            utxos
            txBodyContent
            (fromJust $ toCardanoAddressInEra C.cardanoEra changeAddress)
            Nothing
            & \case
              Right (C.BalancedTxBody _ txBody' _ _) -> txBody'
              Left e -> error $ show e
    void $ signSubmit era srcKey txBody

-- | Generate a new address and its signing key.
genKey
  :: Address
  -- ^ Any address on  the network.
  -> IO (Address, C.SigningKey C.PaymentExtendedKey)
  -- ^ Action to create the address and signing key.
genKey faucetAddress =
  do
    let isPaymentExtendedKey (C.WitnessPaymentExtendedKey key) = Just key
        isPaymentExtendedKey _ = Nothing
    skey <-
      fmap (head . mapMaybe isPaymentExtendedKey)
        . generate
        . infiniteListOf
        $ hedgehog C.genShelleyWitnessSigningKey
    let address =
          Chain.Address $
            (BS.singleton . (0x60 .|.) . (.&. 0x0f) . BS.head . unAddress) faucetAddress
              <> (C.serialiseToRawBytes . C.verificationKeyHash . C.getVerificationKey) skey
    liftIO
      . hPutStrLn stderr
      $ "Generated address " <> show address <> " and signing key " <> show skey <> "."
    pure (address, skey)

-- | Run multiple scenarios.
run
  :: String
  -- ^ The label for the metric.
  -> Int
  -- ^ The number of contracts to run.
  -> Address
  -- ^ The address.
  -> C.SigningKey C.PaymentExtendedKey
  -- ^ The signing key.
  -> MarloweT IO Benchmark
  -- ^ Action to run the benchmark.
run metric count address key =
  do
    let address' = uncurry Address . fromJust . deserialiseAddress $ unAddress address
    start <- liftIO getCurrentTime
    start' <- liftIO getPOSIXTime
    scenarios <- liftIO . replicateM count $ randomScenario 1200 [address']
    mapM_ (runScenario $ M.singleton address' (address, key)) scenarios
    finish <- liftIO getCurrentTime
    finish' <- liftIO getPOSIXTime
    let seconds = realToFrac $ finish' - start'
        contractsPerSecond = realToFrac (length scenarios) / seconds
        creationsPerSecond = realToFrac (length scenarios) / seconds
        appliesPerSecond = realToFrac (sum $ length . actions <$> scenarios) / seconds
        withdrawsPerSecond = 0
    pure Benchmark{..}

-- | Run a single scenario.
runScenario
  :: (MonadIO m)
  => (MonadMarlowe m)
  => M.Map Party (Address, C.SigningKey C.PaymentExtendedKey)
  -- ^ The parties and their addresses and signing keys.
  -> Scenario
  -- ^ The scenario.
  -> m ()
  -- ^ Action to run the scenario.
runScenario partyCredentials Scenario{..} =
  let act contractId party action =
        let (address, key) = partyCredentials M.! party
         in apply address key contractId [NormalInput action]
   in forM_ actions . uncurry . act
        =<< create partyCredentials contract

-- | Create a contract.
create
  :: (MonadIO m)
  => (MonadMarlowe m)
  => M.Map Party (Address, C.SigningKey C.PaymentExtendedKey)
  -> Contract
  -> m ContractId
create partyCredentials contract =
  do
    let (_, (change, key)) = M.findMin partyCredentials
        distributeRole (Role token) (address, _) = Just (TokenName . fromBuiltin $ unTokenName token, Nothing, ToAddress address, Quantity 1)
        distributeRole _ _ = Nothing
        distributeRoles = mapMaybe (uncurry distributeRole) $ M.toList partyCredentials
        roles =
          if null distributeRoles
            then RoleTokensNone
            else RoleTokensMint . mkMint $ NE.fromList distributeRoles
    ContractCreated support ContractCreatedInEra{contractId, txBody} <-
      createContract
        Nothing
        MarloweV1
        (WalletAddresses change mempty mempty)
        Nothing
        roles
        emptyMarloweTransactionMetadata
        Nothing
        mempty
        (Left contract)
        >>= either (error . show) pure
    void $ signSubmit support key txBody
    pure contractId

-- | Apply input to a contract.
apply
  :: (MonadIO m)
  => (MonadMarlowe m)
  => Address
  -> C.SigningKey C.PaymentExtendedKey
  -> ContractId
  -> [Input]
  -> m TxId
apply change key contractId input =
  do
    InputsApplied support InputsAppliedInEra{txBody} <-
      applyInputs
        MarloweV1
        (WalletAddresses change mempty mempty)
        contractId
        emptyMarloweTransactionMetadata
        input
        >>= either (error . show) pure
    signSubmit support key txBody

-- TODO: Here we should determine any payouts and withdraw them.

-- | Sign and submit a transaction.
signSubmit
  :: (MonadIO m)
  => (MonadMarlowe m)
  => C.BabbageEraOnwards era
  -> C.SigningKey C.PaymentExtendedKey
  -> C.TxBody era
  -> m TxId
signSubmit support key txBody =
  case support of
    C.BabbageEraOnwardsBabbage -> signSubmit' support key txBody
    C.BabbageEraOnwardsConway -> signSubmit' support key txBody

-- | Sign and submit a transaction.
signSubmit'
  :: (C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadMarlowe m)
  => C.BabbageEraOnwards era
  -> C.SigningKey C.PaymentExtendedKey
  -> C.TxBody era
  -> m TxId
signSubmit' support key txBody =
  do
    let tx = C.signShelleyTransaction C.shelleyBasedEra txBody [C.WitnessPaymentExtendedKey key]
    submitAndWait support tx
      >>= \case
        Right _ -> pure . fromCardanoTxId $ C.getTxId txBody
        Left e -> error $ show e
