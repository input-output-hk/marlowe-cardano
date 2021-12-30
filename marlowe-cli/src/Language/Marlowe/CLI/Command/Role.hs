-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Role-related commands of the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Command.Role (
-- * Marlowe CLI Commands
  RoleCommand(..)
, parseRoleCommand
, runRoleCommand
) where


import           Cardano.Api                        (NetworkId (..), StakeAddressReference (..))
import           Control.Monad.Except               (MonadError, MonadIO, throwError)
import           Data.Maybe                         (fromMaybe)
import           Language.Marlowe.CLI.Command.Parse (parseCurrencySymbol, parseNetworkId, parseStakeAddressReference,
                                                     parseTokenName)
import           Language.Marlowe.CLI.Export        (exportRoleAddress, exportRoleDatum, exportRoleRedeemer,
                                                     exportRoleValidator)
import           Language.Marlowe.CLI.Types         (CliError)
import           Plutus.V1.Ledger.Api               (CurrencySymbol, TokenName, defaultCostModelParams)

import qualified Options.Applicative                as O


-- | Marlowe CLI commands and options for exporting role information.
data RoleCommand =
    -- | Export the role address for a Marlowe contract.
    ExportAddress
    {
      network        :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake          :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency' :: CurrencySymbol               -- ^ The role currency symbols, if any.
    }
    -- | Export the role validator for a Marlowe contract.
  | ExportValidator
    {
      network        :: Maybe NetworkId              -- ^ The network ID, if any.
    , stake          :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency' :: CurrencySymbol               -- ^ The role currency symbols, if any.
    , outputFile     :: Maybe FilePath               -- ^ The output JSON file for the validator information.
    , printHash      :: Bool                         -- ^ Whether to print the validator hash.
    , printStats     :: Bool                         -- ^ Whether to print statistics about the contract.
    }
    -- | Export the role datum for a Marlowe contract transaction.
  | ExportDatum
    {
      roleName   :: TokenName       -- ^ The role name.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file for the datum.
    , printStats :: Bool            -- ^ Whether to print statistics about the datum.
    }
    -- | Export the role redeemer for a Marlowe contract transaction.
  | ExportRedeemer
    {
      outputFile :: Maybe FilePath  -- ^ The output JSON file for the redeemer.
    , printStats :: Bool            -- ^ Whether to print statistics about the redeemer.
    }


-- | Run a related command.
runRoleCommand :: MonadError CliError m
               => MonadIO m
               => RoleCommand  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runRoleCommand command =
  do
    costModel <-
      maybe
        (throwError "Missing default cost model.")
        pure
        defaultCostModelParams
    let
      network' = fromMaybe Mainnet $ network command
      stake'         = fromMaybe NoStakeAddress $ stake command
    case command of
      ExportAddress{..}   -> exportRoleAddress
                               rolesCurrency' network' stake'
      ExportValidator{..} -> exportRoleValidator
                               rolesCurrency' costModel network' stake'
                               outputFile
                               printHash printStats
      ExportDatum{..}     -> exportRoleDatum
                               roleName
                               outputFile
                               printStats
      ExportRedeemer{..}  -> exportRoleRedeemer
                               outputFile
                               printStats


-- | Parser for related commands.
parseRoleCommand :: O.Parser RoleCommand
parseRoleCommand =
  O.hsubparser
    $ O.commandGroup "Low-level commands for exporting Marlowe role information:"
    <> exportAddressCommand
    <> exportDatumCommand
    <> exportRedeemerCommand
    <> exportValidatorCommand


-- | Parser for the "address" command.
exportAddressCommand :: O.Mod O.CommandFields RoleCommand
exportAddressCommand =
  O.command "address"
    . O.info exportAddressOptions
    $ O.progDesc "Print a role validator address."


-- | Parser for the "address" options.
exportAddressOptions :: O.Parser RoleCommand
exportAddressOptions =
  ExportAddress
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet.")
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."             )
    <*> O.option parseCurrencySymbol                       (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles."     )


-- | Parser for the "validator" command.
exportValidatorCommand :: O.Mod O.CommandFields RoleCommand
exportValidatorCommand =
  O.command "validator"
    . O.info exportValidatorOptions
    $ O.progDesc "Export a role validator to a JSON file."


-- | Parser for the "validator" options.
exportValidatorOptions :: O.Parser RoleCommand
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet.")
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."             )
    <*> O.option parseCurrencySymbol                       (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles."     )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for validator."    )
    <*> O.switch                                           (O.long "print-hash"                                    <> O.help "Print validator hash."              )
    <*> O.switch                                           (O.long "print-stats"                                   <> O.help "Print statistics."                  )


-- | Parser for the "datum" command.
exportDatumCommand :: O.Mod O.CommandFields RoleCommand
exportDatumCommand =
  O.command "datum"
    . O.info exportDatumOptions
    $ O.progDesc "Export a role datum to a JSON file."


-- | Parser for the "datum" options.
exportDatumOptions :: O.Parser RoleCommand
exportDatumOptions =
  ExportDatum
    <$> O.option parseTokenName    (O.long "name"        <> O.metavar "TOKEN_NAME" <> O.help "JSON input file for the contract state.")
    <*> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "DATUM_FILE" <> O.help "JSON output file for datum."            )
    <*> O.switch                   (O.long "print-stats"                           <> O.help "Print statistics."                      )


-- | Parser for the "redeemer" command.
exportRedeemerCommand :: O.Mod O.CommandFields RoleCommand
exportRedeemerCommand =
  O.command "redeemer"
    . O.info exportRedeemerOptions
    $ O.progDesc "Export a role redeemer to a JSON file."


-- | Parser for the "redeemer" options.
exportRedeemerOptions :: O.Parser RoleCommand
exportRedeemerOptions =
  ExportRedeemer
    <$> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer."      )
    <*> O.switch                   (O.long "print-stats"                            <> O.help "Print statistics."                   )
