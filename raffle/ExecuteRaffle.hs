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

-- cabal new-install --lib shh
-- cabal new-install --lib shh-extras
-- cabal new-install --lib PyF
-- cabal new-install --lib MissingH
-- cabal new-install --lib aeson

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
  parties <- (fromJust . A.decode @[AddressBech32]) <$> (LBS.readFile $ args !! 1) 
  prizes <- (fromJust . A.decode @[(PolicyId,TokenName)]) <$> (LBS.readFile $ args !! 2)
  let contractId = args !! 3   
 
  s_address <- C.unpack <$> (cat (sponsorAddressFilePath raffleConfiguration) |> captureTrim)
  s_collateral <- C.unpack <$> (cat (sponsorCollateralFilePath raffleConfiguration) |> captureTrim)
   
  let sponsor = Sponsor{ s_address = s_address, s_collateral = s_collateral}
      oracle = Oracle{ o_address = s_address}

  runRaffleStateMachine
    raffleConfiguration
    sponsor
    oracle
    parties
    prizes 
    contractId 

data ContractState 
  = WaitingForPrizeDeposit 
  | WaitingForOracle
  | WaitingForNotify
  | Close
  | Unknown String
  deriving (Show,Eq)

getState :: RuntimeURI -> ContractId -> IO ContractState
getState runtime contractId = do
  validityStart <- C.unpack <$> (date  "+\"%Y-%m-%dT%H:%M:%SZ\"" |> captureTrim)
  validityEnd <-   C.unpack <$> (date "-d" "+10 minutes" "+\"%Y-%m-%dT%H:%M:%SZ\"" |> captureTrim)
  let contractIdEncoded = replace "#" "%23" contractId
      nextQuery = curl
                  "-s"  
                  "-H" 
                  "GET" 
                  [fmt|http://{(host $ runtime)}:{(show . web_port $ runtime)}/contracts/{contractIdEncoded}/next?validityStart={replace "\"" "" validityStart}&validityEnd={replace "\"" "" validityEnd}|]
  
  isClose <- ((== "\"contractClosed\""). C.unpack <$>) $ nextQuery |> jq ".errorCode" |> captureTrim
  if isClose then return Close
    else do 
      isWaitingForOracle <- ((/= "null"). C.unpack <$>) $ (nextQuery |> jq ".applicable_inputs.choices[0]" |> captureTrim)
      if isWaitingForOracle then return WaitingForOracle
      else do 
        isWaitingForPrizeDeposit <- ((/= "null"). C.unpack <$>) $ (nextQuery |> jq ".applicable_inputs.deposits[0]" |> captureTrim)
        if isWaitingForPrizeDeposit then return WaitingForPrizeDeposit
        else do 
          isWaitingForNotify <- ((/= "null"). C.unpack <$>) $ (nextQuery |> jq ".applicable_inputs.notify" |> captureTrim)
          if isWaitingForNotify then return WaitingForNotify
          else do
            unknown <- (C.unpack <$>) $ (nextQuery  |> captureTrim)
            return $ Unknown unknown 



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

runRaffleStateMachine :: RaffleConfiguration -> Sponsor -> Oracle -> [String] -> [(PolicyId,TokenName)]  -> ContractId -> IO()
runRaffleStateMachine raffleConfiguration sponsor oracle parties prizes contractId
  = runRaffleStateMachine' False raffleConfiguration sponsor oracle  parties prizes contractId

runRaffleStateMachine' :: Bool -> RaffleConfiguration -> Sponsor -> Oracle  -> [String] -> [(PolicyId,TokenName)]  -> ContractId ->  IO()
runRaffleStateMachine' True _ _ _ _ _ _ = return ()
runRaffleStateMachine' False raffleConfiguration sponsor oracle parties prizes contractId = do
  sleep "5s" 
  state <- getState (runtimeURI raffleConfiguration) contractId
  case state of 
    WaitingForPrizeDeposit -> do
      echo "#########################"
      echo "WaitingForPrizeDeposit"
      sequence $ (\(a,b) -> depositNFT contractId a b) <$> prizes
      echo ">>>> Prize NFTs Deposited"
      echo "#########################"
      runRaffleStateMachine' False raffleConfiguration sponsor oracle parties prizes contractId
      where 
            depositNFT :: ContractId -> PolicyId -> TokenName -> IO()
            depositNFT contractId policyId tokenName = do
              echo $ " >> Depositing " ++ tokenName
              marlowe_runtime_cli
                "--marlowe-runtime-host" (host (runtimeURI raffleConfiguration))
                "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
                "deposit"
                "--change-address"  (s_address sponsor)
                "--collateral-utxo" (s_collateral sponsor)
                "--manual-sign"     (tmpTxToSign raffleConfiguration)
                "--contract" contractId
                "--to-party" (s_address sponsor)
                "--from-party" (s_address sponsor) 
                "--currency" policyId
                "--token-name" tokenName
                "--quantity" 1
              submit sponsor raffleConfiguration
              echo $ " >> " ++ tokenName ++ " deposited" 
    WaitingForOracle -> do
      echo "#########################"
      echo "WaitingForOracle"
      echo "#########################" 
      choiceMade <- provideRandomChoiceNumber
      echo $ ">>>> Oracle has answered with " ++ choiceMade
      echo "#########################" 
      runRaffleStateMachine' False raffleConfiguration sponsor oracle parties prizes contractId 
       where 
        provideRandomChoiceNumber :: IO RandomNumber
        provideRandomChoiceNumber = do
          choiceMade <- C.unpack <$>
            ( curl
                "-s"  
                "-X" 
                "GET"
                [fmt|https://www.random.org/integers/?format=plain&col=1&rnd=new&base=10&num=1&min={0}&max={show $ (length parties)-1}|]
              |> captureTrim)
            
          marlowe_runtime_cli
            "--marlowe-runtime-host" (host (runtimeURI raffleConfiguration))
            "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
            "choose"
            "--change-address"  (s_address sponsor)
            "--collateral-utxo" (s_collateral sponsor)
            "--manual-sign"     (tmpTxToSign raffleConfiguration)
            "--contract" contractId
            "--choice" "RANDOM"
            "--party" (s_address sponsor)
            "--value" choiceMade 
          submit sponsor raffleConfiguration
          return choiceMade
    WaitingForNotify -> do
      echo "#########################"
      echo "WaitingForNotify"
      echo "#########################"
      notify
      echo $ " >> Notified"
      echo "#########################"   
      runRaffleStateMachine' False raffleConfiguration sponsor oracle parties prizes contractId
        where 
          notify  = do 
            marlowe_runtime_cli
              "--marlowe-runtime-host" (host (runtimeURI raffleConfiguration))
              "--marlowe-runtime-port" (proxy_port (runtimeURI raffleConfiguration))
              "notify"
              "--change-address"  (s_address sponsor)
              "--collateral-utxo" (s_collateral sponsor)
              "--manual-sign"     (tmpTxToSign raffleConfiguration)
              "--contract" contractId
            submit sponsor raffleConfiguration
             
    Close -> do 
      echo "#########################"
      echo "Raffle Closed"
      echo "#########################"  
      return () 
    Unknown payload -> do
      echo "#########################"
      echo "Unknown State : " 
      echo payload
      echo "#########################"  
      runRaffleStateMachine' False raffleConfiguration sponsor oracle parties prizes contractId 
