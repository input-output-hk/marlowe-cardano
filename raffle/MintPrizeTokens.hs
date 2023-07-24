#!/usr/bin/env runhaskell

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

type PolicyId = String
type TokenName = String
type ContractId = String
type RandomNumber = String
type AddressBech32 = String
data RaffleConfiguration 
    = RaffleConfiguration 
    { runtimeURI :: RuntimeURI
    , contract :: FilePath -- where to save the raffle contract json generated
    , state :: FilePath -- where to save the raffle contract state generated
    , chunkSize :: Integer -- parameter for generatiing the contract
    , tmpTxToSign :: FilePath -- tmp file for tx created to sign
    , tmpTxToSubmit :: FilePath -- tmp file for signed tx to submit
    , sponsorAddressFilePath :: FilePath
    , sponsorCollateralFilePath :: FilePath
    , sponsorPrivateKeyFilePath :: FilePath
    , deadlines :: Deadlines
    } deriving (Show,Generic,A.FromJSON,A.ToJSON)
data Sponsor = Sponsor {s_address :: String, s_collateral :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)
data Oracle = Oracle {o_address :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)
data RuntimeURI = RuntimeURI {host :: String, proxy_port :: Integer, web_port :: Integer} deriving (Show,Generic,A.FromJSON,A.ToJSON)
data Deadlines = Deadlines {deposit :: String, selectWinner :: String, payout :: String} deriving (Show,Generic,A.FromJSON,A.ToJSON)



main :: IO ()
main = do
  args <- getArgs  
  raffleConfiguration <- (fromJust . A.decode @RaffleConfiguration) <$> (LBS.readFile $ args !! 0) 
  let prizeName = args !! 1
  let prizeFilePath = args !! 2
  s_address <- C.unpack <$> (cat (sponsorAddressFilePath raffleConfiguration) |> captureTrim)
  s_collateral <- C.unpack <$> (cat (sponsorCollateralFilePath raffleConfiguration) |> captureTrim)
   
  let sponsor = Sponsor{ s_address = s_address, s_collateral = s_collateral}

  prizes <- mintPrizeTokens raffleConfiguration sponsor [prizeName]
  echo $ "Prizes Minted " ++ show prizes


mintPrizeTokens :: RaffleConfiguration -> Sponsor -> [TokenName] ->IO [(PolicyId,TokenName)]
mintPrizeTokens  raffleConfiguration sponsor tokenNames = do
  nodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH"
  policyID <- (C.unpack) <$> ( marlowe_cli 
      "util" 
      "mint"
      "--testnet-magic" 1 --preprod
      "--socket-path" nodeSocketPath
      "--issuer" ((s_address sponsor) ++ ":" ++ (sponsorPrivateKeyFilePath raffleConfiguration))
      "--count" 1
      "--out-file" (tmpTxToSign raffleConfiguration)
      (asArg $ (\tokenName -> tokenName ++ ":" ++ (s_address sponsor)) <$> tokenNames) |> captureTrim)
  
  submit sponsor raffleConfiguration
  let mintedTokens = (policyID,) <$> tokenNames
  echo $ ">> minted NFT Tokens : " ++  show mintedTokens
  return mintedTokens

submit :: Sponsor -> RaffleConfiguration ->  IO ()
submit sponsor raffleConfiguration = do
  cardano_cli 
    "transaction" 
    "sign"
    "--signing-key-file" (sponsorPrivateKeyFilePath raffleConfiguration)
    "--tx-body-file" (tmpTxToSign raffleConfiguration)
    "--out-file" (tmpTxToSubmit raffleConfiguration)
  echo $ " >> tx signed"
  marlowe_runtime_cli
    "--marlowe-runtime-host" (host . runtimeURI $ raffleConfiguration)
    "--marlowe-runtime-port" (proxy_port .  runtimeURI $ raffleConfiguration)
    "submit" 
    (tmpTxToSubmit raffleConfiguration)
  echo $ " >> tx submitted"
