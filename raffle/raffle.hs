#!/usr/bin/env runhaskell

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.ByteString.Lazy.Char8 as C
import Shh
import PyF
import System.Environment (getEnv)
import Data.List 
import Data.List.Utils (replace)
import Control.Concurrent

-- cabal new-install --lib shh
-- cabal new-install --lib shh-extras
-- cabal new-install --lib PyF
-- cabal new-install --lib MissingH

load SearchPath ["jq","date","curl","grep","pwd", "cat", "marlowe-cli", "marlowe-runtime-cli", "cardano-cli" ,"echo"]

data Sponsor = Sponsor {s_address :: String, s_collateral :: String, s_privateKeyFilePath :: String}
data Oracle = Oracle {o_address :: String}
data RaffleConfiguration = RaffleConfiguration {contract :: FilePath, state :: FilePath, tmpTxToSign :: FilePath, tmpTxToSubmit :: FilePath, chunkSize :: Integer}
data RuntimeURI = RuntimeURI {host :: String, proxy_port :: Integer, web_port :: Integer} deriving Show
data Deadlines = Deadlines {deposit :: String, selectWinner :: String, payout :: String}
type PolicyId = String
type TokenName = String
type ContractId = String



main :: IO ()
main = do
  s_address <- C.unpack <$> (cat "./addresses/sponsor/address.txt" |> captureTrim)
  s_collateral <- C.unpack <$> (cat "./addresses/sponsor/collateral.txt" |> captureTrim)
  nodeSocketPath <- getEnv "CARDANO_NODE_SOCKET_PATH" 
  rootPath <- C.unpack <$> (pwd |> captureTrim)
  echo s_address
  echo s_collateral
  let runtimeURI = RuntimeURI "preprod.marlowe.run" 3700 3780-- "0.0.0.0" 56063
      raffleConfiguration =
        RaffleConfiguration
          { contract = rootPath ++ "/raffle.json"
          , state = rootPath ++ "/raffle-state.json"
          , tmpTxToSign = rootPath ++ "/unsigned-tx.txt"
          , tmpTxToSubmit = rootPath ++ "/signed-tx.txt"
          , chunkSize = 2
          }

      sponsor = 
        Sponsor{ s_address = s_address
               , s_collateral = s_collateral
               , s_privateKeyFilePath = rootPath ++ "/addresses/sponsor/extended-private-key.json"}
      oracle =
        Oracle
          { o_address = s_address
              --"addr_test1qzy4aluz9zcmxuhuqgjhl457ee0ma0hcq38w62ckaagjwgvnjlzdfawd9kvva673fht7737e3r5j322v7090uqhpn0wq7a2gly"
          }
      deadlines = Deadlines "10d" "10d" "10d"

      parties =
        [ "addr_test1qpj0y6wace4m6qfrs3gnw8jefql4ajy9y8fuu7hdpgdqv9vtgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42qnm58x5"
        , "addr_test1qpx28jszrx6n90shly5qvdqzaxmrhrkr7nleq3u6ayfz375tgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42q4ckex2"
        , "addr_test1qpx28jszrx6n90shly5qvdqzaxmrhrkr7nleq3u6ayfz375tgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42q4ckex2"
        , "addr_test1qpx28jszrx6n90shly5qvdqzaxmrhrkr7nleq3u6ayfz375tgcwrpyv2qstmfxty443vdld98ram4j2gjhqsdsq5w42q4ckex2"
        ]

  prizes <- mintPrizeTokens runtimeURI raffleConfiguration sponsor nodeSocketPath ["1stPrize"]

  contractId <- genAndInitializeRaffle
    runtimeURI
    raffleConfiguration
    sponsor
    oracle
    deadlines
    parties
    prizes
  
  runRaffleStateMachine
    runtimeURI
    raffleConfiguration
    sponsor
    oracle
    deadlines
    parties
    prizes 
    contractId -- "8b51ad1f2252688b9e1aff4d8e00e52464de17abcb4f2e3d4f36584b81888055#1"

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




mintPrizeTokens :: RuntimeURI -> RaffleConfiguration -> Sponsor -> TokenName -> [String] ->IO [(PolicyId,TokenName)]
mintPrizeTokens runtime raffleConfiguration sponsor nodeSocketPath tokenNames = do
  policyID <- (C.unpack) <$> ( marlowe_cli 
      "util" 
      "mint"
      "--testnet-magic" 1 --preprod
      "--socket-path" nodeSocketPath
      "--issuer" ((s_address sponsor) ++ ":" ++ (s_privateKeyFilePath sponsor))
      "--count" 1
      "--out-file" (tmpTxToSign raffleConfiguration)
      (asArg $ (\tokenName -> tokenName ++ ":" ++ (s_address sponsor)) <$> tokenNames) |> captureTrim)
  
  echo $ " >> tx unsigned"
  cardano_cli 
    "transaction" 
    "sign"
    "--signing-key-file" (s_privateKeyFilePath sponsor)
    "--tx-body-file" (tmpTxToSign raffleConfiguration)
    "--out-file" (tmpTxToSubmit raffleConfiguration)
  echo $ " >> tx signed"
  marlowe_runtime_cli
    "--marlowe-runtime-host" (host runtime)
    "--marlowe-runtime-port" (proxy_port runtime)
    "submit" 
    (tmpTxToSubmit raffleConfiguration)
  let mintedTokens = (policyID,) <$> tokenNames
  echo $ ">> minted NFT Tokens : " ++  show mintedTokens
  return mintedTokens


runRaffleStateMachine :: RuntimeURI -> RaffleConfiguration -> Sponsor -> Oracle -> Deadlines -> [String] -> [(PolicyId,TokenName)]  -> ContractId -> IO()
runRaffleStateMachine runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId
  = runRaffleStateMachine' False runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId

runRaffleStateMachine' :: Bool -> RuntimeURI -> RaffleConfiguration -> Sponsor -> Oracle -> Deadlines -> [String] -> [(PolicyId,TokenName)]  -> ContractId ->  IO()
runRaffleStateMachine' True _ _ _ _ _ _ _ _ = return ()
runRaffleStateMachine' False runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId = do 
  state <- getState runtime contractId
  case state of 
    WaitingForPrizeDeposit -> do
      echo "#########################"
      echo "WaitingForPrizeDeposit"
      sequence $ (\(a,b) -> depositNFT contractId a b) <$> prizes
      echo ">>>> Prize NFTs Deposited"
      echo "#########################"
      runRaffleStateMachine' False runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId
      where 
            depositNFT :: ContractId -> PolicyId -> TokenName -> IO()
            depositNFT contractId policyId tokenName = do
              echo $ " >> Depositing " ++ tokenName
              marlowe_runtime_cli
                "--marlowe-runtime-host" (host runtime)
                "--marlowe-runtime-port" (proxy_port runtime)
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
              echo $ " >> tx unsigned"
              cardano_cli 
                "transaction" 
                "sign"
                "--signing-key-file" (s_privateKeyFilePath sponsor)
                "--tx-body-file" (tmpTxToSign raffleConfiguration)
                "--out-file" (tmpTxToSubmit raffleConfiguration)
              echo $ " >> tx signed"
              marlowe_runtime_cli
                "--marlowe-runtime-host" (host runtime)
                "--marlowe-runtime-port" (proxy_port runtime)
                "submit" 
                (tmpTxToSubmit raffleConfiguration)
              echo $ " >> " ++ tokenName ++ " deposited" 
    WaitingForOracle -> do
      echo "#########################"
      echo "WaitingForOracle"
      echo "#########################" 
      simulateOracle
      echo ">>>> Oracle has answered"
      echo "#########################" 
      runRaffleStateMachine' False runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId 
       where 
        waitOracle :: IO ()
        waitOracle = do 
          state <- getState runtime contractId
          if (state /= WaitingForOracle) 
            then return ()
          else do 
            threadDelay 5_000_000
            echo ">>>> Oracle has not answered yet... retrying (every 5s)"
            waitOracle
        simulateOracle :: IO()
        simulateOracle = do
          echo $ " >> Simlulating Oracle " 
          marlowe_runtime_cli
            "--marlowe-runtime-host" (host runtime)
            "--marlowe-runtime-port" (proxy_port runtime)
            "choose"
            "--change-address"  (s_address sponsor)
            "--collateral-utxo" (s_collateral sponsor)
            "--manual-sign"     (tmpTxToSign raffleConfiguration)
            "--contract" contractId
            "--choice" "RANDOM"
            "--party" (s_address sponsor)
            "--value" 0 
          echo $ " >> tx unsigned"
          cardano_cli 
            "transaction" 
            "sign"
            "--signing-key-file" (s_privateKeyFilePath sponsor)
            "--tx-body-file" (tmpTxToSign raffleConfiguration)
            "--out-file" (tmpTxToSubmit raffleConfiguration)
          echo $ " >> tx signed"
          marlowe_runtime_cli
            "--marlowe-runtime-host" (host runtime)
            "--marlowe-runtime-port" (proxy_port runtime)
            "submit" 
            (tmpTxToSubmit raffleConfiguration)
          echo $ " >> Random Number Given" 
    WaitingForNotify -> do
      echo "#########################"
      echo "WaitingForNotify"
      echo "#########################"
      notify
      echo $ " >> Notified"
      echo "#########################"   
      runRaffleStateMachine' False runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId
        where 
          notify  = do 
            marlowe_runtime_cli
              "--marlowe-runtime-host" (host runtime)
              "--marlowe-runtime-port" (proxy_port runtime)
              "notify"
              "--change-address"  (s_address sponsor)
              "--collateral-utxo" (s_collateral sponsor)
              "--manual-sign"     (tmpTxToSign raffleConfiguration)
              "--contract" contractId
            echo $ " >> tx unsigned"
            cardano_cli 
              "transaction" 
              "sign"
              "--signing-key-file" (s_privateKeyFilePath sponsor)
              "--tx-body-file" (tmpTxToSign raffleConfiguration)
              "--out-file" (tmpTxToSubmit raffleConfiguration)
            echo $ " >> tx signed"
            marlowe_runtime_cli
              "--marlowe-runtime-host" (host runtime)
              "--marlowe-runtime-port" (proxy_port runtime)
              "submit" 
              (tmpTxToSubmit raffleConfiguration)
             
    Close -> do 
      echo "#########################"
      echo "Raflle Closed"
      echo "#########################"  
      return () 
    Unknown payload -> do
      echo "#########################"
      echo "Unknown State : " 
      echo payload
      echo "#########################"  
      runRaffleStateMachine' False runtime raffleConfiguration sponsor oracle deadlines parties prizes contractId 

genAndInitializeRaffle :: RuntimeURI -> RaffleConfiguration -> Sponsor -> Oracle -> Deadlines -> [String] -> [(PolicyId,TokenName)] -> IO ContractId
genAndInitializeRaffle runtime raffleConfiguration sponsor oracle deadlines parties prizes = do
  echo "#########################"
  echo "Raffle Contract Generation & Initialisation"
  echo "-------------------------"
  generateContract
  echo "-------------------------"
  echo "Raffle Contract Generated"
  echo "-------------------------"
  contractHash <- loadContractToStore
  contractId <- initialize contractHash
  echo "-------------------------"
  echo $ "Raffle Contract Initialized : " ++ contractId 
  echo "-------------------------"
  echo "#########################"
  return contractId
  where
    generateContract :: IO ()
    generateContract = do
      marlowe_cli
        "template"
        "raffle"
        "--minimum-ada" 2_000_000
        "--sponsor" (s_address sponsor)
        "--oracle"  (o_address oracle)
        "--chunk-size" (chunkSize raffleConfiguration)
        (asArg $ (\party -> ["--parties", party]) <$> parties)
        "--deposit-deadline" (deposit deadlines)
        "--select-deadline"  (selectWinner deadlines)
        "--payout-deadline"  (payout deadlines)
        (asArg $ (\(a,b) -> ["--prizes", a ++ "." ++ b]) <$> prizes)
        "--out-contract-file" (contract raffleConfiguration)
        "--out-state-file" (state raffleConfiguration) 
      echo $ " >> Raffle Contract saved in : " ++ (contract raffleConfiguration)

    loadContractToStore = do
      contractHash <-
        C.unpack
          <$> ( marlowe_runtime_cli
                  "--marlowe-runtime-host" (host runtime)
                  "--marlowe-runtime-port" (proxy_port runtime)
                  "load"
                  (contract raffleConfiguration)
                  |> captureTrim
              )
      echo $ " >> Contract stored with hash :" ++ contractHash
      return contractHash

    initialize :: String -> IO ContractId
    initialize contractHash = do
      contractId <- C.unpack  <$> (marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (proxy_port runtime)
        "create"
        "--min-utxo" 2_000_000
        "--change-address"  (s_address sponsor)
        "--collateral-utxo" (s_collateral sponsor)
        "--manual-sign"     (tmpTxToSign raffleConfiguration)
        "--contract-hash"   contractHash |> captureTrim)
      echo $ " >> tx unsigned"
      echo $ contractId
      cardano_cli 
        "transaction" 
        "sign"
        "--signing-key-file" (s_privateKeyFilePath sponsor)
        "--tx-body-file" (tmpTxToSign raffleConfiguration)
        "--out-file" (tmpTxToSubmit raffleConfiguration)
      echo $ " >> tx signed"
      marlowe_runtime_cli
        "--marlowe-runtime-host" (host runtime)
        "--marlowe-runtime-port" (proxy_port runtime)
        "submit" 
        (tmpTxToSubmit raffleConfiguration)
      echo $ " >> Contract initialzed (tx appended)"
      return contractId


