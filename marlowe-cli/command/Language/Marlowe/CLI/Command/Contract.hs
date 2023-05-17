-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Export-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Command.Contract
  ( -- * Marlowe CLI Commands
    ContractCommand(..)
  , parseContractCommand
  , runContractCommand
  ) where


import Cardano.Api (NetworkId(..), StakeAddressReference(..))
import Control.Monad.Except (MonadError, MonadIO)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse
  (parseCurrencySymbol, parseNetworkId, parseStakeAddressReference, protocolVersionOpt)
import Language.Marlowe.CLI.Export
  (exportDatum, exportMarlowe, exportMarloweAddress, exportMarloweValidator, exportRedeemer)
import Language.Marlowe.CLI.Plutus.Script.Utils (printPir, printUplc)
import Language.Marlowe.CLI.Types (CliEnv, CliError)
import Language.Marlowe.Client (defaultMarloweParams, marloweParams)
import Plutus.V1.Ledger.Api (CurrencySymbol, ProtocolVersion)

import Control.Monad.Reader.Class (MonadReader)
import Language.Marlowe.CLI.IO (getDefaultCostModel)
import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for exporting data.
data ContractCommand =
    -- | Export comprehensive Marlowe contract and transaction information.
    Export
    {
      network         :: NetworkId                    -- ^ The network ID, if any.
    , stake           :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency   :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    , protocolVersion :: ProtocolVersion              -- ^ The protocol version.
    , contractFile    :: FilePath                     -- ^ The JSON file containing the contract.
    , stateFile       :: FilePath                     -- ^ The JSON file containing the contract's state.
    , inputFiles      :: [FilePath]                   -- ^ The JSON files containing the contract's input.
    , outputFile      :: Maybe FilePath               -- ^ The output JSON file for Marlowe contract information.
    , printStats      :: Bool                         -- ^ Whether to print statistics about the contract and transaction.
    }
    -- | Export the address for a Marlowe contract.
  | ExportAddress
    {
      network       :: NetworkId                    -- ^ The network ID, if any.
    , stake         :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    }
    -- | Export the validator for a Marlowe contract.
  | ExportValidator
    {
      network         :: NetworkId                    -- ^ The network ID, if any.
    , stake           :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , protocolVersion :: ProtocolVersion              -- ^ The protocol version.
    , outputFile      :: Maybe FilePath               -- ^ The output JSON file for the validator information.
    , printHash       :: Bool                         -- ^ Whether to print the validator hash.
    , printStats      :: Bool                         -- ^ Whether to print statistics about the contract.
    }
    -- | Export the datum for a Marlowe contract transaction.
  | ExportDatum
    {
      rolesCurrency :: Maybe CurrencySymbol  -- ^ The role currency symbols, if any.
    , contractFile  :: FilePath              -- ^ The JSON file containing the contract.
    , stateFile     :: FilePath              -- ^ The JSON file containing the contract's state.
    , outputFile    :: Maybe FilePath        -- ^ The output JSON file for the datum.
    , printStats    :: Bool                  -- ^ Whether to print statistics about the datum.
    }
    -- | Export the redeemer for a Marlowe contract transaction.
  | ExportRedeemer
    {
      inputFiles :: [FilePath]      -- ^ The JSON files containing the contract's input.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file for the redeemer.
    , printStats :: Bool            -- ^ Whether to print statistics about the redeemer.
    }
    -- | Print the PIR for the Marlowe validator.
  | PrintPir
    {
      outputFile :: Maybe FilePath  -- ^ The output file for the PIR.
    }
    -- | Print the UPLC for the Marlowe validator.
  | PrintUplc
    {
      outputFile :: Maybe FilePath  -- ^ The output file for the UPLC.
    }


-- | Run an export-related command.
runContractCommand :: MonadError CliError m
                   => MonadIO m
                   => MonadReader (CliEnv era) m
                   => ContractCommand  -- ^ The command.
                   -> m ()             -- ^ Action for running the command.
runContractCommand command =
  do
    costModel <- getDefaultCostModel
    let
      network' = network command
      marloweParams' = maybe defaultMarloweParams marloweParams $ rolesCurrency command
      stake'         = fromMaybe NoStakeAddress $ stake command
    case command of
      Export{..}          -> exportMarlowe
                               marloweParams'
                               protocolVersion
                               costModel
                               network'
                               stake'
                               contractFile
                               stateFile
                               inputFiles
                               outputFile
                               printStats
      ExportAddress{}     -> exportMarloweAddress  network' stake'
      ExportValidator{..} -> exportMarloweValidator protocolVersion costModel network' stake' outputFile printHash printStats
      ExportDatum{..}     -> exportDatum
                               marloweParams'
                               contractFile
                               stateFile
                               outputFile
                               printStats
      ExportRedeemer{..}  -> exportRedeemer
                               inputFiles
                               outputFile
                               printStats
      PrintPir{..}        -> printPir outputFile
      PrintUplc{..}       -> printUplc outputFile


-- | Parser for export-related commands.
parseContractCommand :: O.Mod O.OptionFields NetworkId
                     -> O.Parser ContractCommand
parseContractCommand network =
  O.hsubparser
    $ O.commandGroup "Low-level commands for exporting Marlowe contract information:"
    <> exportAddressCommand network
    <> exportDatumCommand
    <> exportMarloweCommand network
    <> exportRedeemerCommand
    <> exportValidatorCommand network
    <> printPirCommand
    <> printUplcCommand


-- | Parser for the "marlowe" command.
exportMarloweCommand :: O.Mod O.OptionFields NetworkId
                     -> O.Mod O.CommandFields ContractCommand
exportMarloweCommand network =
  O.command "marlowe"
    $ O.info (exportMarloweOptions network)
    $ O.progDesc "Export a Marlowe contract to a JSON file."


-- | Parser for the "marlowe" options.
exportMarloweOptions :: O.Mod O.OptionFields NetworkId
                     -> O.Parser ContractCommand
exportMarloweOptions network =
  Export
    <$> parseNetworkId network
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                    <> O.help "Stake address, if any."                                                            )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL"            <> O.help "The currency symbol for roles, if any."                                            )
    <*> protocolVersionOpt
    <*> O.strOption                                        (O.long "contract-file"  <> O.metavar "CONTRACT_FILE"              <> O.help "JSON input file for the contract."                                                 )
    <*> O.strOption                                        (O.long "state-file"     <> O.metavar "STATE_FILE"                 <> O.help "JSON input file for the contract state."                                           )
    <*> (O.many . O.strOption)                             (O.long "input-file"     <> O.metavar "INPUT_FILE"                 <> O.help "JSON input file for redeemer inputs."                                              )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"                <> O.help "JSON output file for contract."                                                    )
    <*> O.switch                                           (O.long "print-stats"                                              <> O.help "Print statistics."                                                                 )


-- | Parser for the "address" command.
exportAddressCommand :: O.Mod O.OptionFields NetworkId
                     -> O.Mod O.CommandFields ContractCommand
exportAddressCommand network =
  O.command "address"
    . O.info (exportAddressOptions network)
    $ O.progDesc "Print a contract validator address."


-- | Parser for the "address" options.
exportAddressOptions :: O.Mod O.OptionFields NetworkId
                     -> O.Parser ContractCommand
exportAddressOptions network =
  ExportAddress
    <$> parseNetworkId network
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"            <> O.help "Stake address, if any."                                                            )


-- | Parser for the "validator" command.
exportValidatorCommand :: O.Mod O.OptionFields NetworkId
                       -> O.Mod O.CommandFields ContractCommand
exportValidatorCommand network =
  O.command "validator"
    . O.info (exportValidatorOptions network)
    $ O.progDesc "Export a contract validator to a JSON file."


-- | Parser for the "validator" options.
exportValidatorOptions :: O.Mod O.OptionFields NetworkId
                       -> O.Parser ContractCommand
exportValidatorOptions network =
  ExportValidator
    <$> parseNetworkId network
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                <> O.help "Stake address, if any."                                                            )
    <*> protocolVersionOpt
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"            <> O.help "JSON output file for validator."                                                   )
    <*> O.switch                                           (O.long "print-hash"                                           <> O.help "Print validator hash."                                                             )
    <*> O.switch                                           (O.long "print-stats"                                          <> O.help "Print statistics."                                                                 )


-- | Parser for the "datum" command.
exportDatumCommand :: O.Mod O.CommandFields ContractCommand
exportDatumCommand =
  O.command "datum"
    . O.info exportDatumOptions
    $ O.progDesc "Export a contract datum to a JSON file."


-- | Parser for the "datum" options.
exportDatumOptions :: O.Parser ContractCommand
exportDatumOptions =
  ExportDatum
    <$> (O.optional . O.option parseCurrencySymbol) (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any." )
    <*> O.strOption                                 (O.long "contract-file"  <> O.metavar "CONTRACT_FILE"   <> O.help "JSON input file for the contract."      )
    <*> O.strOption                                 (O.long "state-file"     <> O.metavar "STATE_FILE"      <> O.help "JSON input file for the contract state.")
    <*> (O.optional . O.strOption)                  (O.long "out-file"       <> O.metavar "DATUM_FILE"      <> O.help "JSON output file for datum."            )
    <*> O.switch                                    (O.long "print-stats"                                   <> O.help "Print statistics."                      )


-- | Parser for the "redeemer" command.
exportRedeemerCommand :: O.Mod O.CommandFields ContractCommand
exportRedeemerCommand =
  O.command "redeemer"
    . O.info exportRedeemerOptions
    $ O.progDesc "Export a contract redeemer to a JSON file."


-- | Parser for the "redeemer" options.
exportRedeemerOptions :: O.Parser ContractCommand
exportRedeemerOptions =
  ExportRedeemer
    <$> (O.many . O.strOption)     (O.long "input-file"  <> O.metavar "INPUT_FILE"  <> O.help "JSON input file for redeemer inputs.")
    <*> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer."      )
    <*> O.switch                   (O.long "print-stats"                            <> O.help "Print statistics."                   )


-- | Parser for the "pir" command.
printPirCommand :: O.Mod O.CommandFields ContractCommand
printPirCommand =
  O.command "pir"
    . O.info (
      PrintPir
        <$> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "The output file for the PIR.")
    )
    $ O.progDesc "Print the Plutus Intermediate Representation (PIR) for the Marlowe validator."


-- | Parser for the "uplc" command.
printUplcCommand :: O.Mod O.CommandFields ContractCommand
printUplcCommand =
  O.command "uplc"
    . O.info (
      PrintUplc
        <$> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "The output file for the UPLC.")
    )
    $ O.progDesc "Print the Untyped Plutus Core (UPLC) for the Marlowe validator."
