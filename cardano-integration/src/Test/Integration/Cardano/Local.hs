{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Test.Integration.Cardano.Local
  ( Delegator(..)
  , LocalTestnet(..)
  , LocalTestnetOptions(..)
  , Network(..)
  , PaymentKeyPair(..)
  , SpoNode(..)
  , StakingKeyPair(..)
  , TestnetException(..)
  , defaultOptions
  , withLocalTestnet
  , withLocalTestnet'
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently_, race_)
import Control.Exception (Exception(displayException), throw)
import Control.Exception.Lifted (SomeException, catch)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource, MonadThrow(throwM), MonadUnliftIO)
import Data.Aeson (FromJSON, Key, Result(..), ToJSON, fromJSON, object, toJSON, (.=))
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Value)
import Data.Functor (void, (<&>))
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time (UTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Yaml.Aeson as YAML
import GHC.Generics (Generic)
import System.FilePath ((</>))
import System.IO (Handle, IOMode(..), hClose, openFile)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..), cleanupProcess, createProcess, proc)
import System.Random (randomRIO)
import Test.HUnit.Lang (HUnitFailure(..))
import Test.Integration.Cardano.Process (execCli_)
import Test.Integration.Workspace
  ( Workspace(..)
  , copyToWorkspace
  , createWorkspace
  , createWorkspaceDir
  , moveToWorkspace
  , resolveWorkspacePath
  , rewriteJSONFile
  , rewriteYAMLFile
  , writeWorkspaceFileJSON
  )
import qualified Test.Integration.Workspace as W
import Text.Printf (printf)
import UnliftIO.Async (concurrently)
import UnliftIO.Exception (catchAny)
import UnliftIO.Resource (allocate, runResourceT, unprotect)

data LocalTestnetOptions = LocalTestnetOptions
  { slotDuration :: Int
  , securityParameter :: Int
  , numSpoNodes :: Int
  , numDelegators :: Int
  , numWallets :: Int
  , totalBalance :: Int
  } deriving (Show, Eq, Ord)

data LocalTestnet = LocalTestnet
  { delegators :: [Delegator]
  , network :: Network
  , spoNodes :: [SpoNode]
  , testnetMagic :: Int
  , wallets :: [PaymentKeyPair]
  , workspace :: Workspace
  , securityParameter :: Int
  }

data PaymentKeyPair = PaymentKeyPair
  { paymentSKey :: FilePath
  , paymentVKey :: FilePath
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON PaymentKeyPair

data StakingKeyPair = StakingKeyPair
  { stakingSKey :: FilePath
  , stakingVKey :: FilePath
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON StakingKeyPair

data Delegator = Delegator
  { paymentKeyPair :: PaymentKeyPair
  , stakingKeyPair :: StakingKeyPair
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Delegator

data SpoNode = SpoNode
  { byronDelegateKey :: FilePath
  , byronDelegationCert :: FilePath
  , coldSKey :: FilePath
  , coldVKey :: FilePath
  , db :: FilePath
  , kesSKey :: FilePath
  , kesVKey :: FilePath
  , nodeName :: String
  , opcert :: FilePath
  , port :: Int
  , processHandle :: ProcessHandle
  , socket :: FilePath
  , stakingRewardSKey :: FilePath
  , stakingRewardVKey :: FilePath
  , stderrLogs :: FilePath
  , stdin :: Handle
  , stdoutLogs :: FilePath
  , topology :: FilePath
  , vrfSKey :: FilePath
  , vrfVKey :: FilePath
  }

data Network = Network
  { configurationYaml :: FilePath
  , byronGenesisJson :: FilePath
  , shelleyGenesisJson :: FilePath
  , alonzoGenesisJson :: FilePath
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON Network

defaultOptions :: LocalTestnetOptions
defaultOptions = LocalTestnetOptions
  { slotDuration = 100
  , securityParameter = 10
  , numSpoNodes = 1
  , numDelegators = 3
  , numWallets = 3
  , totalBalance = 10020000000
  }

-- | Run a test script in the context of a local testnet using default options.
withLocalTestnet :: (MonadUnliftIO m, MonadBaseControl IO m, MonadThrow m) => (LocalTestnet -> m a) -> m a
withLocalTestnet = withLocalTestnet' defaultOptions

-- | A version of @@withLocalTestnet@@ that accepts custom options.
withLocalTestnet'
  :: (MonadUnliftIO m, MonadBaseControl IO m, MonadThrow m)
  => LocalTestnetOptions
  -> (LocalTestnet -> m a)
  -> m a
withLocalTestnet' options test = runResourceT do
  testnet <- startLocalTestnet options
  result <- lift $ (Right <$> test testnet)
    `catch` (\ex@HUnitFailure{} -> pure $ Left ex)
    `catch` rethrowAsTestnetException testnet
  either throw pure result

data TestnetException = TestnetException
  { workspace :: FilePath
  , exception :: String
  } deriving (Generic)

instance ToJSON TestnetException

instance Show TestnetException where
  show = T.unpack . T.decodeUtf8 . YAML.encode . object . pure . ("TestnetException" .=)

instance Exception TestnetException where

-- | Whenever an exception occurs, this function will decorate it with
-- information about the local testnet.
rethrowAsTestnetException :: (MonadBase IO m, MonadIO m, MonadThrow m) => LocalTestnet -> SomeException -> m a
rethrowAsTestnetException LocalTestnet{..} ex = do
  -- prevent workspace from being cleaned up for diagnosing errors.
  void $ unprotect $ W.releaseKey workspace
  let exception = displayException ex
  throwM $ TestnetException (workspaceDir workspace) exception

startLocalTestnet :: (MonadResource m, MonadUnliftIO m) => LocalTestnetOptions -> m LocalTestnet
startLocalTestnet options@LocalTestnetOptions{..} = do
  workspace <- createWorkspace "local-testnet"
  catchAny
    do
      testnetMagic <- randomRIO (1000, 2000)
      socketDir <- createWorkspaceDir workspace "socket"
      logsDir <- createWorkspaceDir workspace "logs"

      currentTime <- round . utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
      -- Add time to execute the CLI commands to set everything up
      let startTime = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromInteger currentTime + 1

      (byronGenesisDir, shelleyGenesisDir) <- concurrently
        (createByronGenesis workspace startTime testnetMagic options)
        (createShelleyGenesisStaked workspace startTime testnetMagic options)

      let
        wallets = [1..numWallets] <&> \n -> PaymentKeyPair
          { paymentSKey = shelleyGenesisDir </> "utxo-keys" </> "utxo" <> show n <> ".skey"
          , paymentVKey = shelleyGenesisDir </> "utxo-keys" </> "utxo" <> show n <> ".vkey"
          }

        delegators = [1..numDelegators] <&> \n -> Delegator
          { paymentKeyPair = PaymentKeyPair
            { paymentSKey = shelleyGenesisDir </> "stake-delegator-keys" </> "payment" <> show n <> ".skey"
            , paymentVKey = shelleyGenesisDir </> "stake-delegator-keys" </> "payment" <> show n <> ".vkey"
            }
          , stakingKeyPair = StakingKeyPair
            { stakingSKey = shelleyGenesisDir </> "stake-delegator-keys" </> "staking" <> show n <> ".skey"
            , stakingVKey = shelleyGenesisDir </> "stake-delegator-keys" </> "staking" <> show n <> ".vkey"
            }
          }

      network <- setupNetwork workspace options byronGenesisDir shelleyGenesisDir
      spoNodes <- traverse (setupSpoNode workspace options network logsDir socketDir byronGenesisDir shelleyGenesisDir) [1..numSpoNodes]

      liftIO $ mapConcurrently_ assertChainExtended spoNodes

      pure LocalTestnet{..}
    \e -> do
      -- prevent workspace from being cleaned up for diagnosing errors.
      _ <- unprotect $ W.releaseKey workspace
      throw e

createByronGenesis :: MonadIO m => Workspace -> UTCTime -> Int -> LocalTestnetOptions -> m FilePath
createByronGenesis workspace startTime testnetMagic LocalTestnetOptions{..} = do
  byronGenesisSpecFile <- writeWorkspaceFileJSON workspace "byron.genesis.spec.json" $ object
    [ "heavyDelThd"       .= ("300000000000" :: String)
    , "maxBlockSize"      .= ("2000000" :: String)
    , "maxTxSize"         .= ("4096" :: String)
    , "maxHeaderSize"     .= ("2000000" :: String)
    , "maxProposalSize"   .= ("700" :: String)
    , "mpcThd"            .= ("20000000000000" :: String)
    , "scriptVersion"     .= (0 :: Int)
    , "slotDuration"      .= show slotDuration
    , "unlockStakeEpoch"  .= ("18446744073709551615" :: String)
    , "updateImplicit"    .= ("10000" :: String)
    , "updateProposalThd" .= ("100000000000000" :: String)
    , "updateVoteThd"     .= ("1000000000000" :: String)
    , "softforkRule"      .= object
      [ "initThd"           .= ("900000000000000" :: String)
      , "minThd"            .= ("600000000000000" :: String)
      , "thdDecrement"      .= ("50000000000000" :: String)
      ]
    , "txFeePolicy"       .= object
      [ "multiplier"        .= ("43946000000" :: String)
      , "summand"           .= ("155381000000000" :: String)
      ]
    ]
  let byronGenesisDir = resolveWorkspacePath workspace "byron-genesis"
  execCli_
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show testnetMagic
    , "--start-time", show @Int $ floor $ nominalDiffTimeToSeconds $ utcTimeToPOSIXSeconds startTime
    , "--k", show securityParameter
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show numSpoNodes
    , "--total-balance", show totalBalance
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", byronGenesisSpecFile
    , "--genesis-output-dir", byronGenesisDir
    ]
  pure byronGenesisDir

createShelleyGenesisStaked :: MonadIO m => Workspace -> UTCTime -> Int -> LocalTestnetOptions -> m FilePath
createShelleyGenesisStaked workspace startTime testnetMagic LocalTestnetOptions{..} = do
  let shelleyGenesisDir = "shelley-genesis"
  let shelleyGenesisDirInWorkspace = resolveWorkspacePath workspace shelleyGenesisDir

  _ <- copyToWorkspace
    workspace
    "./configuration/alonzo-babbage-test-genesis.json"
    (shelleyGenesisDir </> "genesis.alonzo.spec.json")

  configFile <- copyToWorkspace
    workspace
    "./configuration/byron-mainnet/configuration.yaml"
    (shelleyGenesisDir </> "configuration.yaml")

  rewriteYAMLFile configFile
    ( KM.delete "GenesisFile"
    . KM.insert "Protocol" (toJSON @String "Cardano")
    . KM.insert "PBftSignatureThreshold" (toJSON @Double 0.6)
    . KM.insert "minSeverity" (toJSON @String "Debug")
    . KM.insert "ByronGenesisFile" (toJSON @String "genesis/byron/genesis.json")
    . KM.insert "ShelleyGenesisFile" (toJSON @String "genesis/shelley/genesis.json")
    . KM.insert "AlonzoGenesisFile" (toJSON @String "genesis/shelley/genesis.alonzo.json")
    . KM.insert "RequiresNetworkMagic" (toJSON @String "RequiresMagic")
    . KM.insert "LastKnownBlockVersion-Major" (toJSON @Int 6)
    . KM.insert "LastKnownBlockVersion-Minor" (toJSON @Int 0)
    . KM.insert "TestShelleyHardForkAtEpoch" (toJSON @Int 0)
    . KM.insert "TestAllegraHardForkAtEpoch" (toJSON @Int 0)
    . KM.insert "TestMaryHardForkAtEpoch" (toJSON @Int 0)
    . KM.insert "TestAlonzoHardForkAtEpoch" (toJSON @Int 0)
    . KM.insert "TestBabbageHardForkAtEpoch" (toJSON @Int 0)
    . KM.insert "TestEnableDevelopmentHardForkEras" (toJSON True)
    )

  execCli_
    [ "genesis", "create-staked"
    , "--genesis-dir", shelleyGenesisDirInWorkspace
    , "--testnet-magic", show testnetMagic
    , "--gen-pools", show numSpoNodes
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--start-time", iso8601Show startTime
    , "--gen-stake-delegs", show numDelegators
    , "--gen-utxo-keys", show numWallets
    ]

  pure shelleyGenesisDirInWorkspace

setupNetwork :: MonadIO m => Workspace -> LocalTestnetOptions -> FilePath -> FilePath -> m Network
setupNetwork workspace LocalTestnetOptions{..} byronGenesisDir shelleyGenesisDir = do
  configurationYaml <- moveToWorkspace workspace (shelleyGenesisDir </> "configuration.yaml") "configuration.yaml"
  byronGenesisJson <- moveToWorkspace workspace (byronGenesisDir </> "genesis.json") "genesis/byron/genesis.json"
  shelleyGenesisJson <- moveToWorkspace workspace (shelleyGenesisDir </> "genesis.json") "genesis/shelley/genesis.json"
  alonzoGenesisJson <- moveToWorkspace workspace (shelleyGenesisDir </> "genesis.alonzo.json") "genesis/shelley/genesis.alonzo.json"

  rewriteJSONFile shelleyGenesisJson
    ( KM.insert "slotLength"             (toJSON @Double (fromIntegral slotDuration / 1000))
    . KM.insert "activeSlotsCoeff"       (toJSON @Double 0.2)
    . KM.insert "securityParam"          (toJSON @Int securityParameter)
    . KM.insert "epochLength"            (toJSON @Int 500)
    . KM.insert "maxLovelaceSupply"      (toJSON @Int 1000000000000)
    . KM.insert "updateQuorum"           (toJSON @Int 2)
    . updateKeyMap "protocolParams"
      ( KM.insert "minFeeA"                (toJSON @Int 44)
      . KM.insert "minFeeB"                (toJSON @Int 155381)
      . KM.insert "minUTxOValue"           (toJSON @Int 1000000)
      . KM.insert "decentralisationParam"  (toJSON @Double 0.7)
      . KM.insert "rho"                    (toJSON @Double 0.1)
      . KM.insert "tau"                    (toJSON @Double 0.1)
      . updateKeyMap "protocolVersion"     (KM.insert "major" (toJSON @Int 7))
      )
    )

  pure Network{..}

updateKeyMap :: (FromJSON a, ToJSON b) => Key -> (a -> b) -> KeyMap Value -> KeyMap Value
updateKeyMap key f km = case KM.lookup key km of
  Nothing -> km
  Just v -> case fromJSON v of
    Success a -> KM.insert key (toJSON $ f a) km
    _ -> km

setupSpoNode
  :: MonadResource m
  => Workspace
  -> LocalTestnetOptions
  -> Network
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Int
  -> m SpoNode
setupSpoNode workspace LocalTestnetOptions{..} Network{..} logsDir socketDir byronGenesisDir shelleyGenesisDir n = do
  let nodeName = "node-spo" <> show n
  let
    movePoolFile name extension =
      moveToWorkspace workspace (shelleyGenesisDir </> "pools" </> name <> show n <> "." <> extension) (nodeName </> name <> "." <> extension)
  let stdoutLogs = resolveWorkspacePath workspace $ logsDir </> nodeName <> ".stdout.log"
  let stderrLogs = resolveWorkspacePath workspace $ logsDir </> nodeName <> ".stderr.log"
  let socket = resolveWorkspacePath workspace $ socketDir </> nodeName <> ".socket"
  let port = 3000 + n

  vrfSKey <- movePoolFile "vrf" "skey"
  vrfVKey <- movePoolFile "vrf" "vkey"
  coldSKey <- movePoolFile "cold" "skey"
  coldVKey <- movePoolFile "cold" "vkey"
  stakingRewardSKey <- movePoolFile "staking-reward" "skey"
  stakingRewardVKey <- movePoolFile "staking-reward" "vkey"
  opcert <- movePoolFile "opcert" "cert"
  kesSKey <- movePoolFile "kes" "skey"
  kesVKey <- movePoolFile "kes" "vkey"

  byronDelegateKey <- moveToWorkspace workspace (byronGenesisDir </> "delegate-keys." <> printf "%03d" (n - 1) <> ".key") (nodeName </> "byron-delegate.key")
  byronDelegationCert <- moveToWorkspace workspace (byronGenesisDir </> "delegation-cert." <> printf "%03d" (n - 1) <> ".json") (nodeName </> "byron-delegation.cert")

  db <- createWorkspaceDir workspace $ nodeName </> "db"

  let
    mkProducer i = object
      [ "addr" .= ("127.0.0.1" :: String)
      , "port" .= (3000 + i :: Int)
      , "valency" .= (1 :: Int)
      ]

  topology <- writeWorkspaceFileJSON workspace (nodeName </> "topology.json") $ object
    [ "Producers" .= (mkProducer <$> filter (/= n) [1..numSpoNodes])
    ]

  (_, nodeStdout) <- allocate (openFile stdoutLogs WriteMode) hClose
  (_, nodeStderr) <- allocate (openFile stderrLogs WriteMode) hClose

  (_, (mStdin, _, _, processHandle)) <- allocate
    ( createProcess
        ( proc "cardano-node"
            [ "run"
            , "--config", configurationYaml
            , "--topology", topology
            , "--database-path", db
            , "--socket-path", socket
            , "--shelley-kes-key", kesSKey
            , "--shelley-vrf-key", vrfSKey
            , "--byron-delegation-certificate", byronDelegationCert
            , "--byron-signing-key", byronDelegateKey
            , "--shelley-operational-certificate", opcert
            , "--port", show port
            ]
        )
        { std_in = CreatePipe
        , std_out = UseHandle nodeStdout
        , std_err = UseHandle nodeStderr
        , cwd = Just $ workspaceDir workspace
        }
    )
    cleanupProcess

  let stdin = fromJust mStdin

  pure SpoNode{..}

assertChainExtended :: SpoNode -> IO ()
assertChainExtended SpoNode{..} = race_ waitForChainExtendedMessage do
  threadDelay 90_000_000
  fail $ "SPO Node " <> show nodeName <> " failed to start"
  where
    waitForChainExtendedMessage = do
      logs <- readFile stdoutLogs
      if "Chain extended, new tip:" `isInfixOf` logs
        then pure ()
        else threadDelay 1000 *> waitForChainExtendedMessage
