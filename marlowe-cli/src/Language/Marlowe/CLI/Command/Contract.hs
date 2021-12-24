-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Contract-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Command.Contract (
-- * Marlowe CLI Commands
  ContractCommand(..)
, parseContractCommand
, runContractCommand
) where


import           Cardano.Api                (SlotNo)
import           Control.Monad.Except       (MonadError, MonadIO)
import           Language.Marlowe.CLI.Parse (parseSlotNo)
import           Language.Marlowe.CLI.Run   (computeMarlowe)
import           Language.Marlowe.CLI.Types (CliError)

import qualified Options.Applicative        as O


-- | Marlowe CLI commands and options for running contracts.
data ContractCommand =
    -- | Compute the next step in a contract.
    Compute
    {
      contractFile :: FilePath        -- ^ The JSON file containing the contract.
    , stateFile    :: FilePath        -- ^ The JSON file containing the contract's state.
    , inputFiles   :: [FilePath]      -- ^ The JSON files containing the contract's inputs.
    , minimumSlot  :: SlotNo          -- ^ The first valid slot for the transaction.
    , maximumSlot  :: SlotNo          -- ^ The last valid slot for the transaction.
    , outputFile   :: Maybe FilePath  -- ^ The output JSON file with the results of the computation.
    , printStats   :: Bool            -- ^ Whether to print statistics about the redeemer.
    }


-- | Run a contract-related command.
runContractCommand :: MonadError CliError m
                   => MonadIO m
                   => ContractCommand  -- ^ The command.
                   -> m ()             -- ^ Action for running the command.
runContractCommand Compute{..} = computeMarlowe
                                   contractFile stateFile
                                   inputFiles minimumSlot maximumSlot
                                   outputFile
                                   printStats


-- | Parser for contract commands.
parseContractCommand :: O.Parser ContractCommand
parseContractCommand =
  O.hsubparser
    $ O.commandGroup "Commands for running contracts:"
    <> computeCommand


-- | Parser for the "compute" command.
computeCommand :: O.Mod O.CommandFields ContractCommand
computeCommand =
  O.command "compute"
    $ O.info computeOptions
    $ O.progDesc "Compute the next step of a Marlowe contract and write the output to a JSON file."


-- | Parser for the "compute" options.
computeOptions :: O.Parser ContractCommand
computeOptions =
  Compute
    <$> O.strOption                (O.long "contract-file"     <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract."      )
    <*> O.strOption                (O.long "state-file"        <> O.metavar "STATE_FILE"    <> O.help "JSON input file for the contract state.")
    <*> (O.many . O.strOption)     (O.long "input-file"        <> O.metavar "INPUT_FILE"    <> O.help "JSON input file for redeemer inputs."   )
    <*> O.option parseSlotNo       (O.long "invalid-before"    <> O.metavar "SLOT"          <> O.help "Minimum slot for the redemption."       )
    <*> O.option parseSlotNo       (O.long "invalid-hereafter" <> O.metavar "SLOT"          <> O.help "Maximum slot for the redemption."       )
    <*> (O.optional . O.strOption) (O.long "out-file"          <> O.metavar "OUTPUT_FILE"   <> O.help "JSON output file for contract."         )
    <*> O.switch                   (O.long "print-stats"                                    <> O.help "Print statistics."                      )
