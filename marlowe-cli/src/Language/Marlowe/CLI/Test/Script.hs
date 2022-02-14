
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Test.Script (
  scriptTest
) where



import Cardano.Api (CardanoMode, LocalNodeConnectInfo (..), NetworkId (..))
import Control.Monad.Except (MonadError, throwError)
import Language.Marlowe.CLI.Test.Types
import Language.Marlowe.CLI.Types (CliError (..))
import Ledger.TimeSlot (SlotConfig (..))


scriptTest  :: MonadError CliError m
            => NetworkId
            -> LocalNodeConnectInfo CardanoMode
            -> SlotConfig
            -> ScriptTest
            -> m ()
scriptTest _network _connection _slotConfig ScriptTest{} =
  throwError $ CliError "Script tests are not implemented yet."
