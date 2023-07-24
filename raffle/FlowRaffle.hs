#!/usr/bin/env cabal
{- cabal:
build-depends: base, shh, aeson, PyF, shh, bytestring,MissingH
-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString.Lazy.Char8 as C
import Shh
import PyF
import System.Environment (getEnv,getArgs)
import GHC.Generics
import Data.List 
import Data.List.Utils (replace)
import Control.Concurrent
import Data.Maybe
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS 

load SearchPath ["sleep","jq","date","curl","grep","pwd", "cat", "marlowe-cli", "marlowe-runtime-cli", "cardano-cli" ,"echo"]

main :: IO ()
main = do
  exe "./MintPrizeTokens.hs" "./inputs/configuration.json" "1stPrice" "./inputs/prizes.json" 
  contractId1 <- last . (fmap C.unpack ) <$> (exe "./InitializeRaffle.hs" "./inputs/configuration.json" "./inputs/parties1.json" "./inputs/prizes.json" |> captureLines)
  exe "./ExecuteRaffle.hs" "./inputs/configuration.json" "./inputs/parties1.json" "./inputs/prizes.json" contractId1 
  echo contractId1

  exe "./MintPrizeTokens.hs" "./inputs/configuration.json" "2ndPrice" "./inputs/prizes.json" 
  contractId2 <- last . (fmap C.unpack ) <$> (exe "./InitializeRaffle.hs" "./inputs/configuration.json" "./inputs/parties2.json" "./inputs/prizes.json" |> captureLines)
  -- exe "./ExecuteRaffle.hs" "./inputs/configuration.json" "./inputs/parties.json" "./inputs/prizes.json" contractId 
  echo contractId2

  exe "./MintPrizeTokens.hs" "./inputs/configuration.json" "3rdPrice" "./inputs/prizes.json" 
  contractId3 <- last . (fmap C.unpack ) <$> (exe "./InitializeRaffle.hs" "./inputs/configuration.json" "./inputs/parties3.json" "./inputs/prizes.json" |> captureLines)
  echo contractId3
  -- exe "./ExecuteRaffle.hs" "./inputs/configuration.json" "./inputs/parties.json" "./inputs/prizes.json" contractId 
  