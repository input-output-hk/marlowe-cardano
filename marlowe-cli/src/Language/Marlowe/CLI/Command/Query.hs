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


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Command.Query (
-- * Marlowe CLI Commands
  QueryCommand(..)
, parseQueryCommand
, runQueryCommand
) where


import Cardano.Api (AddressAny, TxId)
import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Language.Marlowe.CLI.ChainIndex (queryAddress, queryApp, queryHistory, queryOutput, queryPayout,
                                        queryTransaction)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseCurrencySymbol, parseOutputQuery, parseTxId, parseUrl)
import Language.Marlowe.CLI.Types (CliError (..), OutputQuery)
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
    -- | Query the contract histories.
  | Histories
    {
      indexUrl      :: BaseUrl               -- ^ The URL for the chain index.
    , rolesCurrency :: Maybe CurrencySymbol  -- ^ The role currency symbols, if any.
    , outputFile    :: Maybe FilePath        -- ^ The output JSON file for Marlowe histories.
    }
    -- | Query transaction details.
  | Transaction
    {
      indexUrl   :: BaseUrl         -- ^ The URL for the chain index.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file for Marlowe histories.
    , txIds      :: [TxId]          -- ^ The transaction IDs.
    }
    -- | Query UTxO details.
  | Address
    {
      indexUrl   :: BaseUrl         -- ^ The URL for the chain index.
    , spent      :: Bool            -- ^ Whether to also report spent transactions.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file for Marlowe histories.
    , addresses  :: [AddressAny]    -- ^ The addresses.
    }
    -- | Query UTxO details.
  | Output
    {
      indexUrl   :: BaseUrl         -- ^ The URL for the chain index.
    , query      :: OutputQuery     -- ^ Filter the query results.
    , spent      :: Bool            -- ^ Whether to also report spent transactions.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file for Marlowe histories.
    , addresses  :: [AddressAny]    -- ^ The addresses.
    }


-- | Run a query command.
runQueryCommand :: MonadError CliError m
               => MonadIO m
               => QueryCommand  -- ^ The command.
               -> m ()          -- ^ Action for running the command.
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
      App{..}         -> queryApp         runApi marloweParams'  spent outputFile
      Payout{..}      -> queryPayout      runApi marloweParams'  spent outputFile
      Histories{..}   -> queryHistory     runApi marloweParams'        outputFile
      Transaction{..} -> queryTransaction runApi txIds                 outputFile
      Address{..}     -> queryAddress     runApi addresses       spent outputFile
      Output{..}      -> queryOutput      runApi addresses query spent outputFile


-- | Parser for query commands.
parseQueryCommand :: O.Parser QueryCommand
parseQueryCommand =
  O.hsubparser
    $ O.commandGroup "Query commands:"
    <> addressCommand
    <> appCommand
    <> historyCommand
    <> outputCommand
    <> payoutCommand
    <> transactionCommand


-- | Parser for the "app" command.
appCommand :: O.Mod O.CommandFields QueryCommand
appCommand =
  O.command "app"
    $ O.info appOptions
    $ O.progDesc "Query the state of the Marlowe application script."


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
    $ O.progDesc "Query the state of the Marlowe payout script."


-- | Parser for the "payout" options.
payoutOptions :: O.Parser QueryCommand
payoutOptions =
  App
    <$> O.option parseUrl                           (O.long "index-url"      <> O.metavar "URL"             <> O.help "URL for the Plutus chain index."           )
    <*> (O.optional . O.option parseCurrencySymbol) (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."    )
    <*> O.switch                                    (O.long "spent"                                         <> O.help "Whether to also report spent transactions.")
    <*> (O.optional . O.strOption)                  (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for payout data."         )


-- | Parser for the "history" command.
historyCommand :: O.Mod O.CommandFields QueryCommand
historyCommand =
  O.command "history"
    $ O.info historyOptions
    $ O.progDesc "Query for the Marlowe contract histories."


-- | Parser for the "history" options.
historyOptions :: O.Parser QueryCommand
historyOptions =
  Histories
    <$> O.option parseUrl                           (O.long "index-url"      <> O.metavar "URL"             <> O.help "URL for the Plutus chain index."           )
    <*> (O.optional . O.option parseCurrencySymbol) (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."    )
    <*> (O.optional . O.strOption)                  (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for history data."        )


-- | Parser for the "address" command.
addressCommand :: O.Mod O.CommandFields QueryCommand
addressCommand =
  O.command "address"
    $ O.info addressOptions
    $ O.progDesc "Query transactions at an address."


-- | Parser for the "address" options.
addressOptions :: O.Parser QueryCommand
addressOptions =
  Address
    <$> O.option parseUrl                  (O.long "index-url" <> O.metavar "URL"         <> O.help "URL for the Plutus chain index."           )
    <*> O.switch                           (O.long "spent"                                <> O.help "Whether to also report spent transactions.")
    <*> (O.optional . O.strOption)         (O.long "out-file"  <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for address data."        )
    <*> O.some (O.argument parseAddressAny $                      O.metavar "ADDRESS"     <> O.help "The address."                              )


-- | Parser for the "output" command.
outputCommand :: O.Mod O.CommandFields QueryCommand
outputCommand =
  O.command "output"
    $ O.info outputOptions
    $ O.progDesc "Query output details."


-- | Parser for the "output" options.
outputOptions :: O.Parser QueryCommand
outputOptions =
  Output
    <$> O.option parseUrl                  (O.long "index-url" <> O.metavar "URL"         <> O.help "URL for the Plutus chain index."           )
    <*> parseOutputQuery
    <*> O.switch                           (O.long "spent"                                <> O.help "Whether to also report spent transactions.")
    <*> (O.optional . O.strOption)         (O.long "out-file"  <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for address data."        )
    <*> O.some (O.argument parseAddressAny $                      O.metavar "ADDRESS"     <> O.help "The address."                              )


-- | Parser for the "transaction" command.
transactionCommand :: O.Mod O.CommandFields QueryCommand
transactionCommand =
  O.command "transaction"
    $ O.info transactionOptions
    $ O.progDesc "Query transaction details."


-- | Parser for the "transaction" options.
transactionOptions :: O.Parser QueryCommand
transactionOptions =
  Transaction
    <$> O.option parseUrl            (O.long "index-url" <> O.metavar "URL"         <> O.help "URL for the Plutus chain index."       )
    <*> (O.optional . O.strOption)   (O.long "out-file"  <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for transaction data.")
    <*> O.some (O.argument parseTxId $                      O.metavar "TXID"        <> O.help "The transaction ID."                   )
