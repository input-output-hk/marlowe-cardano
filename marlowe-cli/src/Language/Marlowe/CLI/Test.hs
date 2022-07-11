-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Test (
-- * Testing
  runTests
) where


import Cardano.Api (ConsensusModeParams (CardanoModeParams), EpochSlots (..), LocalNodeConnectInfo (..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT)
import Data.Bifunctor (first)
import Language.Marlowe.CLI.IO (decodeFileStrict, readSigningKey)
import Language.Marlowe.CLI.Test.PAB (pabTest)
import Control.Monad.Except (MonadError, MonadIO)
import Language.Marlowe.CLI.IO (decodeFileStrict)
import Language.Marlowe.CLI.Test.Script (scriptTest)
import Language.Marlowe.CLI.Test.Types (MarloweTests (..))
import Language.Marlowe.CLI.Transaction (querySlotConfig)
import Language.Marlowe.CLI.Types (CliError (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Network.WebSockets (runClient)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (pabClient)
import Plutus.V1.Ledger.Api (defaultCostModelParams)
import Servant.Client (BaseUrl (..), mkClientEnv, runClientM)


-- | Run tests of a Marlowe contract.
runTests :: MonadError CliError m
         => MonadIO m
         => MarloweTests FilePath  -- ^ The tests.
         -> m ()                   -- ^ Action for running the tests.
runTests ScriptTests{..} =
  do
    costModel <-
      maybe
        (throwError $ CliError "Missing default cost model.")
        pure
        defaultCostModelParams
    let
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network
        , localNodeSocketPath      = socketPath
        }
    slotConfig <- querySlotConfig connection
    tests' <- mapM decodeFileStrict tests
    mapM_ (scriptTest costModel network connection slotConfig) tests'
