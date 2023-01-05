

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Spec.Marlowe.Service (handle)
import System.Exit (die)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdin, stdout)

import qualified Data.Aeson as A (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, putStrLn)


main :: IO ()
main =
  do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    stream <- lines <$> getContents
    sequence_
      [
        case A.eitherDecode $ LBS8.pack chunk of
          Right request -> LBS8.putStrLn . A.encode =<< handle request
          Left message  -> die message
      |
        chunk <- parseChunks stream
      ]


parseChunks :: [String] -> [String]
parseChunks stream =
  let
    separate :: Bool -> [String] -> [String] -> [[String]]
    separate False _ [] = mempty
    separate False [] ("```" : remainder) = separate True mempty remainder
    separate True previous ("```" : remainder) = previous : separate False mempty remainder
    separate True previous (next : remainer) = separate True (previous <> [next]) remainer
    separate _ _ _ = error "Invalid stream."
  in
    unlines <$> separate False mempty stream
