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
import Control.Monad.Except (MonadError, MonadIO)
import Language.Marlowe.CLI.IO (decodeFileStrict)
import Language.Marlowe.CLI.Test.Script (scriptTest)
import Language.Marlowe.CLI.Test.Types (MarloweTests (..))
import Language.Marlowe.CLI.Transaction (querySlotConfig)
import Language.Marlowe.CLI.Types (CliError (..))
import PlutusCore (defaultCostModelParams)


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
