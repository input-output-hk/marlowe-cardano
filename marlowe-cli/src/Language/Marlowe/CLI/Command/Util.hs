-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Miscellaneous utilities in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


module Language.Marlowe.CLI.Command.Util (
-- * Marlowe CLI Commands
  UtilCommand(..)
, parseUtilCommand
, runUtilCommand
) where


import qualified Options.Applicative as O


-- | Marlowe CLI commands and options.
data UtilCommand = Util


-- | Run a miscellaneous command.
runUtilCommand :: Monad m
               => UtilCommand  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runUtilCommand = const $ pure ()


-- | Parser for miscellaneous commands.
parseUtilCommand :: O.Parser UtilCommand
parseUtilCommand =
  O.hsubparser
    $ O.commandGroup "Miscellaneous low-level commands:"
