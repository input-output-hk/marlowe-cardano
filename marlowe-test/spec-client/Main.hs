

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( handle
  , main
  ) where


import Marlowe.Spec.Service.Random (generateValue)
import Marlowe.Spec.Service.Serialization (roundtripSerialization)
import Marlowe.Spec.Service.Types (Request(..), Response(..))
import System.Exit (die)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdin, stdout)

import qualified Data.Aeson as A (eitherDecode, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as LBS8 (pack, putStrLn)
import qualified Language.Marlowe.Core.V1.Semantics as Marlowe (computeTransaction, playTrace)


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


handle :: Request -> IO Response
handle TestRoundtripSerialization{..} =
  pure
    . RequestResponse . A.toJSON
    $ roundtripSerialization typeSerialized valueSerialized
handle GenerateRandomValue{..} =
  generateValue typeSerialized
    >>= \case
      Right value -> pure . RequestResponse . A.object . pure $ "value" A..= value
      Left failureResponse -> pure $ ResponseFailure{..}
handle ComputeTransaction{..} =
  let
    valueResponse = A.toJSON $ Marlowe.computeTransaction transactionInput state contract
  in
    pure RequestResponse{..}
handle PlayTrace{..} =
  let
    valueResponse = A.toJSON $ Marlowe.playTrace initialTime contract transactionInputs
  in
    pure RequestResponse{..}
