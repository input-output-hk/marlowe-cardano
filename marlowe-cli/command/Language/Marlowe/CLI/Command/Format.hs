{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------

-- | Format Marlowe contracts in the CLI tool.
module Language.Marlowe.CLI.Command.Format (
  -- * Marlowe CLI Commands
  FormatCommand (..),
  runFormatCommand,
  parseFormatCommand,
) where

import Control.Monad.Except (MonadError, MonadIO (..))
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Format (maybeWriteJson, maybeWritePretty, readContractJson, readContractPretty)
import Language.Marlowe.CLI.Types (CliError (..))
import Options.Applicative qualified as O

-- | Marlowe CLI commands and options for convert templates.
data FormatCommand
  = -- | Format a contract.
    Format
    { inputFile :: Maybe FilePath
    -- ^ The Marlowe file containing the contract to be converted.
    , outputFile :: Maybe FilePath
    -- ^ The output file for the converted Marlowe contract.
    , fromPretty :: Bool
    -- ^ Flag to indicate whether the input format is pretty printed Marlowe.
    , toPretty :: Bool
    -- ^ Flag to indicate whether the output format should be pretty printed Marlowe.
    }
  deriving stock (Eq, Generic, Show)

-- | Create a contract from a template.
runFormatCommand
  :: (MonadError CliError m)
  => (MonadIO m)
  => FormatCommand
  -- ^ The command.
  -> m ()
  -- ^ Action for runninng the command.
runFormatCommand
  Format
    { inputFile
    , outputFile
    , fromPretty = False
    , toPretty = False
    } = readContractJson inputFile >>= maybeWriteJson outputFile
runFormatCommand
  Format
    { inputFile
    , outputFile
    , fromPretty = True
    , toPretty = False
    } = readContractPretty inputFile >>= maybeWriteJson outputFile
runFormatCommand
  Format
    { inputFile
    , outputFile
    , fromPretty = False
    , toPretty = True
    } = readContractJson inputFile >>= maybeWritePretty outputFile
runFormatCommand
  Format
    { inputFile
    , outputFile
    , fromPretty = True
    , toPretty = True
    } = readContractPretty inputFile >>= maybeWritePretty outputFile

-- | Parser for template commands.
parseFormatCommand :: O.Parser FormatCommand
parseFormatCommand =
  Format
    <$> inFile
    <*> outFile
    <*> readPrettyFlag
    <*> writePrettyFlag
  where
    inFile =
      O.optional . O.strOption $
        mconcat
          [ O.long "in-file"
          , O.metavar "MARLOWE_FILE"
          , O.help "The Marlowe file containing the contract. Stdin if omitted."
          ]
    outFile =
      O.optional . O.strOption $
        mconcat
          [ O.long "out-file"
          , O.metavar "MARLOWE_FILE"
          , O.help "The Marlowe file containing the output contract. Stdout if omitted."
          ]
    readPrettyFlag =
      O.flag False True $
        mconcat
          [ O.long "read-pretty"
          , O.help "Whether to read a pretty printed Marlowe Contract instead of JSON."
          ]
    writePrettyFlag =
      O.flag False True $
        mconcat
          [ O.long "write-pretty"
          , O.help "Whether to write a pretty printed Marlowe Contract instead of JSON."
          ]
