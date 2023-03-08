-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Utility to generate all valid transactions for contracts in JSON files.
--
-----------------------------------------------------------------------------


{-# LANGUAGE LambdaCase #-}


module Main
  ( -- * Testing
    main
  ) where


import Control.Monad.Except (runExceptT)
import Data.List (isSuffixOf)
import Spec.Marlowe.Reference (processContract)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath ((<.>), (</>))


-- | Entry point.
main :: IO ()
main =
  do
    folders <- getArgs
    let
      findContracts folder =
        fmap (\x -> folder </> take (length x - 9) x)
          . filter (".contract" `isSuffixOf`)
          <$> listDirectory folder
    prefixes <- concat <$> mapM findContracts folders
    sequence_
      [
        runExceptT (processContract (prefix <.> "contract") (prefix <.> "paths"))
          >>= \case
            Right () -> putStrLn $ "Successfully processed " <> show prefix <> "."
            Left msg -> putStrLn $ "Failed processing " <> show prefix <> ": " <> msg <> "."
      |
        prefix <- prefixes
      ]
