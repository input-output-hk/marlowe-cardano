
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Test (
  runTests
) where


import Cardano.Api (ConsensusModeParams (CardanoModeParams), EpochSlots (..), LocalNodeConnectInfo (..), NetworkId (..))
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT, throwError)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.IO (decodeFileStrict, readSigningKey)
import Language.Marlowe.CLI.Test.PAB (pabTest)
import Language.Marlowe.CLI.Test.Script (scriptTest)
import Language.Marlowe.CLI.Test.Types (MarloweTests (..), PabAccess (..))
import Language.Marlowe.CLI.Types (CliError (..))
import Ledger.TimeSlot (SlotConfig (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Network.WebSockets (runClient)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (pabClient)
import Plutus.V1.Ledger.Api (POSIXTime (..))
import Servant.Client (BaseUrl (..), mkClientEnv, runClientM)


runTests :: MonadError CliError m
         => MonadIO m
         => MarloweTests FilePath
         -> m ()
runTests ScriptTests{..} =
  do
    let
      network' = fromMaybe Mainnet network
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network'
        , localNodeSocketPath      = socketPath
        }
      slotConfig = SlotConfig slotLength (POSIXTime slotZeroOffset)
    tests' <- mapM decodeFileStrict tests
    mapM_ (scriptTest network' connection slotConfig) tests'
runTests PabTests{..} =
  do
    manager <- liftIO $ newManager defaultManagerSettings
    let
      network' = fromMaybe Mainnet network
      localConnection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network'
        , localNodeSocketPath      = socketPath
        }
      BaseUrl{..} = pabUrl
      client = pabClient
      runHttp url f =
        do
          result <- liftIO $ runClientM f $ mkClientEnv manager url
          case result of
            Right result' -> pure result'
            Left  e       -> throwError . CliError $ show e
      runWallet = runHttp walletUrl
      runApi = runHttp pabUrl
      runWs ContractInstanceId{..} f =
        do
          result <-
            withSocketsDo
              . runClient baseUrlHost baseUrlPort ("/ws/" <> show unContractInstanceId)
              $ runExceptT . f
          case result of
            Right () -> pure ()
            Left  e  -> print e
    tests' <- mapM decodeFileStrict tests
    faucetKey <- readSigningKey faucetFile
    mapM_ (pabTest PabAccess{..} faucetKey faucetAddress burnAddress passphrase) tests'
