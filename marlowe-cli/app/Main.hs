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


module Main (
-- * Entry Point
  main
) where


import Cardano.Config.Git.Rev (gitRev)
import Data.Text (unpack)
import Data.Version (showVersion)
import Language.Marlowe.CLI.Command (runCLI)
import Paths_marlowe_cli (version)


-- | Run the Marlow CLI tool.
main :: IO () -- ^ Action to run the tool.
main =
  runCLI
    $ showVersion version <> " @ " <> unpack gitRev
