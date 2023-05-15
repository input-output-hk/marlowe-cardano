
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Run benchmarks for Marlowe validators.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}


module Main
  ( -- * Entry point
    main
  ) where


import Benchmark.Marlowe (tabulateResults)
import Cardano.Binary (serialize')
import Data.List (intercalate)
import Plutus.V2.Ledger.Api (SerializedScript, ValidatorHash)

import qualified Benchmark.Marlowe.RolePayout as RolePayout (benchmarks, validatorBytes, validatorHash)
import qualified Benchmark.Marlowe.Semantics as Semantics (benchmarks, validatorBytes, validatorHash)
import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.Base16 as B16 (encode)


-- | Run the benchmarks and export information about the validators and the benchmarking results.
main :: IO ()
main =
  do

    benchmarks <- either error id <$> Semantics.benchmarks
    writeFile "marlowe-semantics.tsv"
      . unlines . fmap (intercalate "\t")
      $ tabulateResults "Semantics" Semantics.validatorHash Semantics.validatorBytes benchmarks

    printValidator
      "Semantics"
      "marlowe-semantics"
      Semantics.validatorHash
      Semantics.validatorBytes

    benchmarks' <- either error id <$> RolePayout.benchmarks
    writeFile "marlowe-rolepayout.tsv"
      . unlines . fmap (intercalate "\t")
      $ tabulateResults "Role Payout" RolePayout.validatorHash RolePayout.validatorBytes benchmarks'

    printValidator
      "Role payout"
      "marlowe-rolepayout"
      RolePayout.validatorHash
      RolePayout.validatorBytes


-- | Print information about a validator.
printValidator
  :: String  -- ^ The name of the validator.
  -> FilePath  -- ^ The base file path for exported files.
  -> ValidatorHash  -- ^ The hash of the validator script.
  -> SerializedScript  -- ^ The serialised validator.
  -> IO ()  -- ^ Action to print the information about the benchmarking, and write the files.
printValidator name file hash validator =
  do
    putStrLn $ name <> ":"
    putStrLn $ "  Validator hash: " <> show hash
    putStrLn $ "  Validator file: " <> file <> ".plutus"
    putStrLn $ "  Measurements file: " <> file <> ".tsv"
    BS.writeFile (file <> ".plutus")
      $ "{\"type\": \"PlutusScriptV2\", \"description\": \"\", \"cborHex\": \""
      <> B16.encode (serialize' validator) <> "\"}"
    putStrLn ""
