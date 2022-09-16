-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}


module Main
  ( -- * Entry Point
    main
  ) where


import Cardano.Config.Git.RevFromGit (gitRevFromGit)
import Data.Maybe (fromMaybe)
import Data.Text (pack, strip, unpack)
import Data.Version (showVersion)
import Language.Marlowe.CLI.Command (runCLI)
import Paths_marlowe_cli (version)


-- | Run the Marlow CLI tool.
main :: IO () -- ^ Action to run the tool.
main =
  runCLI
    $ showVersion version <> fromMaybe mempty fromGit


fromGit :: Maybe String
#if defined(arm_HOST_ARCH)
  -- cross compiling to arm fails; due to a linker bug
fromGit = Nothing
#else
fromGit = Just ((\v -> if null v then mempty else " @ " <> v) . unpack . strip $ pack $(gitRevFromGit))
#endif
