{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
--
-- Module      :  Language.Marlowe.CLI.Command.Format
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
import Data.Char (toUpper)
import Language.Marlowe.CLI.Format (
  maybeWriteJson,
  maybeWritePretty,
  maybeWriteYaml,
  readContractJson,
  readContractPretty,
 )
import Language.Marlowe.CLI.Types (CliError (..))
import Options.Applicative qualified as O

-- | Marlowe CLI options for formatting contracts.
data FormatCommand
  = -- | Format a contract.
    Format
    { inputFile :: Maybe FilePath
    -- ^ The Marlowe file containing the contract to be formatted.
    , outputFile :: Maybe FilePath
    -- ^ The output file for the formatted Marlowe contract.
    , inFormat :: Maybe Format
    -- ^ Format of the input Marlowe contract.
    , outFormat :: Maybe Format
    -- ^ Format of the output Marlowe contract.
    }
  deriving stock (Eq, Show)

-- | Format a Marlowe contract.
runFormatCommand
  :: (MonadError CliError m)
  => (MonadIO m)
  => FormatCommand
  -- ^ The command.
  -> m ()
  -- ^ Action for runninng the command.
runFormatCommand Format{..} =
  do
    contract <- case inFormat of
      Just Json -> readContractJson inputFile
      Just Yaml -> readContractJson inputFile
      Just Pretty -> readContractPretty inputFile
      Nothing -> readContractJson inputFile
    case outFormat of
      Just Json -> maybeWriteJson outputFile contract
      Just Yaml -> maybeWriteYaml outputFile contract
      Just Pretty -> maybeWritePretty outputFile contract
      Nothing -> maybeWriteJson outputFile contract

-- | Parser for format commands.
parseFormatCommand :: O.Parser FormatCommand
parseFormatCommand =
  Format
    <$> inFile
    <*> outFile
    <*> inFormatParser
    <*> outFormatParser
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
    inFormatParser =
      O.optional . O.option formatReader $
        mconcat
          [ O.long "in"
          , O.metavar "FORMAT"
          , O.help "The format of the input Marlowe contract. Known formats are: Json, Yaml, Marlowe"
          ]
    outFormatParser =
      O.optional . O.option formatReader $
        mconcat
          [ O.long "out"
          , O.metavar "FORMAT"
          , O.help "The format of the output Marlowe contract. Known formats are: Json, Yaml, Marlowe"
          ]

data Format = Json | Yaml | Pretty
  deriving stock (Eq, Read, Show)

formatReader :: O.ReadM Format
formatReader = readFormat =<< O.str
  where
    readFormat arg =
      case map toUpper arg of
        "JSON" -> return Json
        "YAML" -> return Yaml
        "MARLOWE" -> return Pretty
        _ -> O.readerError $ "cannot parse argument '" <> arg <> "'. Valid are: json, yaml, marlowe. Default: json"
