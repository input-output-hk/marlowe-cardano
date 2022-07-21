-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts without the PAB.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}


module Language.Marlowe.CLI.Test.Script (
-- * Testing
  scriptTest
) where



import Cardano.Api (CardanoMode, LocalNodeConnectInfo (..), NetworkId (..))
import Control.Monad.Except (MonadError, throwError)
import Language.Marlowe.CLI.Test.Types
import Language.Marlowe.CLI.Types (CliError (..))
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))


-- | Test a Marlowe contract.
scriptTest  :: MonadError CliError m
            => NetworkId                         -- ^ The network magic.
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest _network _connection _slotConfig ScriptTest{} =
  throwError $ CliError "Script tests are not implemented yet."
