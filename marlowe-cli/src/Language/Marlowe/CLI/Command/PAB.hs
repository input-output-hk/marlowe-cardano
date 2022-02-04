-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | PAB-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Command.PAB (
-- * Marlowe CLI Commands
  PabCommand(..)
, parsePabCommand
, runPabCommand
) where

import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT, throwError)
import Language.Marlowe.CLI.Command.Parse (parseUrl, parseWalletId)
import Language.Marlowe.CLI.PAB (runCompanion)
import Language.Marlowe.CLI.Types (CliError (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Network.WebSockets (runClient)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (pabClient)
import Servant.Client (BaseUrl (..), mkClientEnv, runClientM)
import Wallet.Emulator.Wallet (WalletId)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for running contracts.
data PabCommand =
    -- | Run the companion contract.
    Companion
    {
      pabUrl   :: BaseUrl
    , walletId :: WalletId
    , loop     :: Bool
    }


-- | Run a contract-related command.
runPabCommand :: MonadError CliError m
              => MonadIO m
              => PabCommand  -- ^ The command.
              -> m ()        -- ^ Action for running the command.
runPabCommand command =
  do
    manager <- liftIO $ newManager defaultManagerSettings
    let
      BaseUrl{..} = pabUrl command
      client = pabClient
      apiClientEnv = mkClientEnv manager $ pabUrl command
      runApi f =
        do
          result <- liftIO $ runClientM f apiClientEnv
          case result of
            Right result' -> pure result'
            Left  e       -> throwError . CliError $ show e
      runWs ContractInstanceId{..} f =
        do
          result <-
            liftIO
              . withSocketsDo
              . runClient baseUrlHost baseUrlPort ("/ws/" <> show unContractInstanceId)
              $ runExceptT . f
          case result of
            Right result' -> pure result'
            Left  e       -> throwError e
    case command of
      Companion{..} -> runCompanion
                         client runApi runWs
                         walletId
                         loop


-- | Parser for PAB commands.
parsePabCommand :: O.Parser PabCommand
parsePabCommand =
  O.hsubparser
    $ O.commandGroup "Commands for running contracts on the PAB:"
    <> companionCommand


-- | Parser for the "companion" command.
companionCommand :: O.Mod O.CommandFields PabCommand
companionCommand =
  O.command "companion"
    . O.info companionOptions
    $ O.progDesc "Start the Marlowe companion contract."


-- | Parser for the "companion" options.
companionOptions :: O.Parser PabCommand
companionOptions =
  Companion
    <$> O.option parseUrl      (O.long "pab-url" <> O.metavar "URL"       <> O.help "URL for the Marlowe PAB."                  )
    <*> O.option parseWalletId (O.long "wallet"  <> O.metavar "WALLET_ID" <> O.help "Wallet ID for the contract."               )
    <*> O.switch               (O.long "loop"                             <> O.help "Whether to listen to PAB messages forever.")
