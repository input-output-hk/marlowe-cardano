-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Input-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Command.Input
  ( -- * Marlowe CLI Commands
    InputCommand(..)
  , parseInputCommand
  , runInputCommand
  ) where


import Control.Monad.Except (MonadIO)
import Language.Marlowe.CLI.Command.Parse (parseParty, parseToken)
import Language.Marlowe.CLI.Run (makeChoice, makeDeposit, makeNotification)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, ChoiceName, ChosenNum, Party, Token)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for inputing data.
data InputCommand =
    -- | Input a deposit to a contract.
    InputDeposit
    {
      account    :: AccountId       -- ^ The account for the deposit.
    , party      :: Party           -- ^ The party making the deposit.
    , token      :: Maybe Token     -- ^ The token being deposited, if not Ada.
    , amount     :: Integer         -- ^ The amount of the token deposited.
    , outputFile :: Maybe FilePath  -- ^ The output JSON file representing the input.
    }
    -- | Input a choice to a contract.
  | InputChoice
    {
      choiceName  :: ChoiceName      -- ^ The name of the choice made.
    , choiceParty :: Party           -- ^ The party making the choice.
    , chosen      :: ChosenNum       -- ^ The number chosen.
    , outputFile  :: Maybe FilePath  -- ^ The output JSON file representing the input.
    }
    -- | Input a notification to a contract.
  | InputNotify
    {
      outputFile :: Maybe FilePath  -- ^ The output JSON file representing the input.
    }


-- | Run an input-related command.
runInputCommand :: MonadIO m
                => InputCommand  -- ^ The command.
                -> m ()          -- ^ Action for running the command.
runInputCommand InputDeposit{..} = makeDeposit
                                     account party token amount
                                     outputFile
runInputCommand InputChoice{..}  = makeChoice
                                     choiceName choiceParty chosen
                                     outputFile
runInputCommand InputNotify{..}  = makeNotification
                                     outputFile


-- | Parser for input-related commands.
parseInputCommand :: O.Parser InputCommand
parseInputCommand =
  O.hsubparser
    $ O.commandGroup "Low-level commands for creating inputs to a contract:"
    <> inputChoiceCommand
    <> inputDepositCommand
    <> inputNotifyCommand


-- | Parser for the "deposit" command.
inputDepositCommand :: O.Mod O.CommandFields InputCommand
inputDepositCommand =
  O.command "deposit"
    $ O.info inputDepositOptions
    $ O.progDesc "Create Marlowe input for a deposit."


-- | Parser for the "deposit" options.
inputDepositOptions :: O.Parser InputCommand
inputDepositOptions =
  InputDeposit
    <$> O.option parseParty                (O.long "deposit-account" <> O.metavar "PARTY"       <> O.help "The account for the deposit."          )
    <*> O.option parseParty                (O.long "deposit-party"   <> O.metavar "PARTY"       <> O.help "The party making the deposit."         )
    <*> (O.optional . O.option parseToken) (O.long "deposit-token"   <> O.metavar "TOKEN"       <> O.help "The token being deposited, if not Ada.")
    <*> O.option O.auto                    (O.long "deposit-amount"  <> O.metavar "INTEGER"     <> O.help "The amount of token being deposited."  )
    <*> (O.optional . O.strOption)         (O.long "out-file"        <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input."  )


-- | Parser for the "choose" command.
inputChoiceCommand :: O.Mod O.CommandFields InputCommand
inputChoiceCommand =
  O.command "choose"
    $ O.info inputChoiceOptions
    $ O.progDesc "Create Marlowe input for a choice."


-- | Parser for the "choose" options.
inputChoiceOptions :: O.Parser InputCommand
inputChoiceOptions =
  InputChoice
    <$> O.strOption                (O.long "choice-name"   <> O.metavar "NAME"        <> O.help "The name of the choice made."        )
    <*> O.option parseParty        (O.long "choice-party"  <> O.metavar "PARTY"       <> O.help "The party making the choice."        )
    <*> O.option O.auto            (O.long "choice-number" <> O.metavar "INTEGER"     <> O.help "The number chosen."                  )
    <*> (O.optional . O.strOption) (O.long "out-file"      <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input.")


-- | Parser for the "notify" command.
inputNotifyCommand :: O.Mod O.CommandFields InputCommand
inputNotifyCommand =
  O.command "notify"
    $ O.info inputNotifyOptions
    $ O.progDesc "Create Marlowe input for a notification."


-- | Parser for the "notify" options.
inputNotifyOptions :: O.Parser InputCommand
inputNotifyOptions =
  InputNotify
    <$> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input.")
