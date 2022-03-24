-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Transaction queries in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}


module Language.Marlowe.CLI.Command.Query (
-- * Marlowe CLI Commands
  QueryCommand(..)
, parseQueryCommand
, runQueryCommand
) where


import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Language.Marlowe.CLI.ChainIndex (queryApp, queryPayout)
import Language.Marlowe.CLI.Command.Parse (parseCurrencySymbol, parseUrl)
import Language.Marlowe.CLI.Types (CliError (..))
import Language.Marlowe.Client (defaultMarloweParams, marloweParams)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Plutus.V1.Ledger.Api (CurrencySymbol)
import Servant.Client (BaseUrl (..), mkClientEnv, runClientM)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options.
data QueryCommand =
    -- | Query the application UTxOs.
    App
    {
      indexUrl      :: BaseUrl               -- ^ The URL for the chain index.
    , rolesCurrency :: Maybe CurrencySymbol  -- ^ The role currency symbols, if any.
    , spent         :: Bool                  -- ^ Whether to also report spent transactions.
    , outputFile    :: Maybe FilePath        -- ^ The output JSON file for Marlowe data.
    }
    -- | Query the payout UTxOs.
  | Payout
    {
      indexUrl      :: BaseUrl               -- ^ The URL for the chain index.
    , rolesCurrency :: Maybe CurrencySymbol  -- ^ The role currency symbols, if any.
    , spent         :: Bool                  -- ^ Whether to also report spent transactions.
    , outputFile    :: Maybe FilePath        -- ^ The output JSON file for Marlowe data.
    }


-- | Run a miscellaneous command.
runQueryCommand :: MonadError CliError m
               => MonadIO m
               => QueryCommand  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runQueryCommand command =
  do
    manager <- liftIO $ newManager defaultManagerSettings
    let
      marloweParams' = maybe defaultMarloweParams marloweParams $ rolesCurrency command
      apiClientEnv = mkClientEnv manager $ indexUrl command
      runApi f =
        do
          result <- liftIO $ runClientM f apiClientEnv
          case result of
            Right result' -> pure result'
            Left  e       -> throwError . CliError $ show e
    case command of
      App{..}    -> queryApp    runApi marloweParams' spent outputFile
      Payout{..} -> queryPayout runApi marloweParams' spent outputFile


-- | Parser for query commands.
parseQueryCommand :: O.Parser QueryCommand
parseQueryCommand =
  O.hsubparser
    $ O.commandGroup "Query commands:"
    <> appCommand
    <> payoutCommand


-- | Parser for the "app" command.
appCommand :: O.Mod O.CommandFields QueryCommand
appCommand =
  O.command "app"
    $ O.info appOptions
    $ O.progDesc "Query the state of the Marlowe application."


-- | Parser for the "app" options.
appOptions :: O.Parser QueryCommand
appOptions =
  App
    <$> O.option parseUrl                           (O.long "index-url"      <> O.metavar "URL"             <> O.help "URL for the Plutus chain index."           )
    <*> (O.optional . O.option parseCurrencySymbol) (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."    )
    <*> O.switch                                    (O.long "spent"                                         <> O.help "Whether to also report spent transactions.")
    <*> (O.optional . O.strOption)                  (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for Marlowe data."        )


-- | Parser for the "payout" command.
payoutCommand :: O.Mod O.CommandFields QueryCommand
payoutCommand =
  O.command "payout"
    $ O.info payoutOptions
    $ O.progDesc "Query the state of the Marlowe payouts."


-- | Parser for the "payout" options.
payoutOptions :: O.Parser QueryCommand
payoutOptions =
  App
    <$> O.option parseUrl                           (O.long "index-url"      <> O.metavar "URL"             <> O.help "URL for the Plutus chain index."           )
    <*> (O.optional . O.option parseCurrencySymbol) (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."    )
    <*> O.switch                                    (O.long "spent"                                         <> O.help "Whether to also report spent transactions.")
    <*> (O.optional . O.strOption)                  (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for payout data."         )
