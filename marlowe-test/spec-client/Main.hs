-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Client for `marlowe-test-spec` from the `marlowe` repository.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}


module Main
  ( -- * Testing
    main
  ) where


import Data.List.Split (splitOn)
import Spec.Marlowe.Service (handle)
import Spec.Marlowe.Service.Types (Response(ResponseFailure))
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import qualified Data.Aeson as A (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, putStrLn)


-- | Entry point.
main :: IO ()
main =
  do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    stream <- getContents
    sequence_
      [
        LBS8.putStrLn . A.encode
          =<< case A.eitherDecode $ LBS8.pack chunk of
            Right request -> handle request
            Left message  -> pure $ ResponseFailure message
      |
        chunk <- parseChunks ticksOnSeparateLine stream
      ]


-- | Select whether the triple-ticks need to be a separate line.
ticksOnSeparateLine :: Bool
ticksOnSeparateLine = False


-- | Parse lines of the `marlowe-test-spec` communication protocol.
parseChunks :: Bool -> String -> [String]
parseChunks False = filter (not . all null . lines) . splitOn "```"
parseChunks True =
  let
    separate :: Bool -> [String] -> [String] -> [[String]]
    separate False _ [] = mempty
    separate False [] ("```" : remainder) = separate True mempty remainder
    separate False previous remainder = separate True previous remainder
    separate True previous [] = [previous]
    separate True previous ("```" : remainder) = previous : separate False mempty remainder
    separate True previous (next : remainer) = separate True (previous <> [next]) remainer
  in
    fmap unlines . separate False mempty . lines
