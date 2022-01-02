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


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Command.Contract (
-- * Marlowe CLI Commands
  ContractCommand(..)
, parseContractCommand
, runContractCommand
) where


import           Cardano.Api                        (NetworkId (..), StakeAddressReference (..))
import           Control.Monad.Except               (MonadError, MonadIO, throwError)
import           Data.Maybe                         (fromMaybe)
import           Language.Marlowe.CLI.Command.Parse (parseCurrencySymbol, parseNetworkId, parseStakeAddressReference)
import           Language.Marlowe.CLI.Export        (exportAddress, exportDatum, exportMarlowe, exportRedeemer,
                                                     exportValidator)
import           Language.Marlowe.CLI.Types         (CliError)
import           Language.Marlowe.Client            (defaultMarloweParams, marloweParams)
import           Plutus.V1.Ledger.Api               (CurrencySymbol, POSIXTime (..), defaultCostModelParams)

import qualified Options.Applicative                as O


-- | Marlowe CLI commands and options for exporting data.
data ContractCommand =
    -- | Export comprehensive Marlowe contrac and transactiont information.
    Export
    {
      slotLength     :: Integer                      -- ^ The slot length, in milliseconds.
    , slotZeroOffset :: Integer                      -- ^ The effective POSIX time of slot zero, in milliseconds.
    , network        :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake          :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency  :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    , contractFile   :: FilePath                     -- ^ The JSON file containing the contract.
    , stateFile      :: FilePath                     -- ^ The JSON file containing the contract's state.
    , inputFiles     :: [FilePath]                   -- ^ The JSON files containing the contract's input.
    , outputFile     :: Maybe FilePath               -- ^ The output JSON file for Marlowe contract information.
    , printStats     :: Bool                         -- ^ Whether to print statistics about the contract and transaction.
    }
    -- | Export the address for a Marlowe contract.
  | ExportAddress
    {
      network       :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake         :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    }
    -- | Export the validator for a Marlowe contract.
  | ExportValidator
    {
      network       :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake         :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    , outputFile    :: Maybe FilePath               -- ^ The output JSON file for the validator information.
    , printHash     :: Bool                         -- ^ Whether to print the validator hash.
    , printStats    :: Bool                         -- ^ Whether to print statistics about the contract.
    }
    -- | Export the datum for a Marlowe contract transaction.
  | ExportDatum
    {
      slotLength     :: Integer         -- ^ The slot length, in milliseconds.
    , slotZeroOffset :: Integer         -- ^ The effective POSIX time of slot zero, in milliseconds.
    , contractFile   :: FilePath        -- ^ The JSON file containing the contract.
    , stateFile      :: FilePath        -- ^ The JSON file containing the contract's state.
    , outputFile     :: Maybe FilePath  -- ^ The output JSON file for the datum.
    , printStats     :: Bool            -- ^ Whether to print statistics about the datum.
    }
    -- | Export the redeemer for a Marlowe contract transaction.
  | ExportRedeemer
    {
      inputFiles :: [FilePath]      -- ^ The JSON files containing the contract's input.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file for the redeemer.
    , printStats :: Bool            -- ^ Whether to print statistics about the redeemer.
    }


-- | Run an export-related command.
runContractCommand :: MonadError CliError m
                 => MonadIO m
                 => ContractCommand  -- ^ The command.
                 -> m ()           -- ^ Action for running the command.
runContractCommand command =
  do
    costModel <-
      maybe
        (throwError "Missing default cost model.")
        pure
        defaultCostModelParams
    let
      network' = fromMaybe Mainnet $ network command
      marloweParams' = maybe defaultMarloweParams marloweParams $ rolesCurrency command
      stake'         = fromMaybe NoStakeAddress $ stake command
    case command of
      Export{..}          -> exportMarlowe
                               (slotLength, POSIXTime slotZeroOffset)
                               marloweParams' costModel network' stake'
                               contractFile stateFile
                               inputFiles
                               outputFile
                               printStats
      ExportAddress{}     -> exportAddress
                               marloweParams' network' stake'
      ExportValidator{..} -> exportValidator
                               marloweParams' costModel network' stake'
                               outputFile
                               printHash printStats
      ExportDatum{..}     -> exportDatum
                               (slotLength, POSIXTime slotZeroOffset)
                               contractFile stateFile
                               outputFile
                               printStats
      ExportRedeemer{..}  -> exportRedeemer
                               inputFiles
                               outputFile
                               printStats


-- | Parser for export-related commands.
parseContractCommand :: O.Parser ContractCommand
parseContractCommand =
  O.hsubparser
    $ O.commandGroup "Low-level commands for exporting Marlowe contract information:"
    <> exportAddressCommand
    <> exportDatumCommand
    <> exportMarloweCommand
    <> exportRedeemerCommand
    <> exportValidatorCommand


-- | Parser for the "marlowe" command.
exportMarloweCommand :: O.Mod O.CommandFields ContractCommand
exportMarloweCommand =
  O.command "marlowe"
    $ O.info exportMarloweOptions
    $ O.progDesc "Export a Marlowe contract to a JSON file."


-- | Parser for the "marlowe" options.
exportMarloweOptions :: O.Parser ContractCommand
exportMarloweOptions =
  Export
    <$> O.option O.auto                                    (O.long "slot-length"    <> O.metavar "INTEGER"         <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto                                    (O.long "slot-offset"    <> O.metavar "INTEGER"         <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"                                  <> O.help "Network magic, or omit for mainnet."                    )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                                  <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL"                          <> O.help "The currency symbol for roles, if any."                 )
    <*> O.strOption                                        (O.long "contract-file"  <> O.metavar "CONTRACT_FILE"                            <> O.help "JSON input file for the contract."                      )
    <*> O.strOption                                        (O.long "state-file"     <> O.metavar "STATE_FILE"                               <> O.help "JSON input file for the contract state."                )
    <*> (O.many . O.strOption)                             (O.long "input-file"     <> O.metavar "INPUT_FILE"                               <> O.help "JSON input file for redeemer inputs."                   )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"                              <> O.help "JSON output file for contract."                         )
    <*> O.switch                                           (O.long "print-stats"                                                            <> O.help "Print statistics."                                      )


-- | Parser for the "address" command.
exportAddressCommand :: O.Mod O.CommandFields ContractCommand
exportAddressCommand =
  O.command "address"
    . O.info exportAddressOptions
    $ O.progDesc "Print a contract validator address."


-- | Parser for the "address" options.
exportAddressOptions :: O.Parser ContractCommand
exportAddressOptions =
  ExportAddress
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."                    )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."                 )


-- | Parser for the "validator" command.
exportValidatorCommand :: O.Mod O.CommandFields ContractCommand
exportValidatorCommand =
  O.command "validator"
    . O.info exportValidatorOptions
    $ O.progDesc "Export a contract validator to a JSON file."


-- | Parser for the "validator" options.
exportValidatorOptions :: O.Parser ContractCommand
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."                    )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."                 )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for validator."                        )
    <*> O.switch                                           (O.long "print-hash"                                    <> O.help "Print validator hash."                                  )
    <*> O.switch                                           (O.long "print-stats"                                   <> O.help "Print statistics."                                      )


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
    <$> O.option O.auto            (O.long "slot-length"   <> O.metavar "INTEGER"       <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto            (O.long "slot-offset"   <> O.metavar "INTEGER"       <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> O.strOption                (O.long "contract-file" <> O.metavar "CONTRACT_FILE"                          <> O.help "JSON input file for the contract."                      )
    <*> O.strOption                (O.long "state-file"    <> O.metavar "STATE_FILE"                             <> O.help "JSON input file for the contract state."                )
    <*> (O.optional . O.strOption) (O.long "out-file"      <> O.metavar "DATUM_FILE"                             <> O.help "JSON output file for datum."                            )
    <*> O.switch                   (O.long "print-stats"                                                         <> O.help "Print statistics."                                      )


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
