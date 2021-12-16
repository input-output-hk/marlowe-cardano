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


import           Language.Marlowe.CLI (mainCLI)
import           Paths_marlowe_cli    (version)


-- | Run the Marlow CLI tool.
main :: IO () -- ^ Action to run the tool.
main = mainCLI version
