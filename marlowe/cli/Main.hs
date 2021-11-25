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


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main (
-- * Entry Point
  main
) where


import           Cardano.Api                     (NetworkId (..), NetworkMagic (..), StakeAddressReference (..))
import           Control.Monad                   (when)
import           Control.Monad.Except            (ExceptT, liftIO)
import           Data.Aeson.Encode.Pretty        (encodePretty)
import           Language.Marlowe.CLI            (mainCLI)
import           Language.Marlowe.CLI.Export     (printMarlowe)
import           Language.Marlowe.CLI.Types      (CliError (..), liftCli)
import           Language.Marlowe.Client         (defaultMarloweParams)
import           Language.Marlowe.SemanticsTypes (Action (..), Case (..), Contract (..), Input (..), Party (..),
                                                  Payee (..), State (..), Token (..), Value (..))
import           Language.Marlowe.Util           (ada)
import           Ledger.Ada                      (adaSymbol, adaToken)
import           Paths_marlowe                   (version)
import           Plutus.V1.Ledger.Api            (PubKeyHash (..), defaultCostModelParams, toBuiltin)

import qualified Data.ByteString.Base16          as Base16 (decode)
import qualified Data.ByteString.Lazy            as LBS (writeFile)
import qualified PlutusTx.AssocMap               as AM (empty, singleton)


-- | Run the Marlow CLI tool.
main :: IO () -- ^ Action to run the tool.
main = mainCLI version example


-- | Hardwired example, which can be run by executing `marlowe-cli example`.
example :: Bool                   -- ^ Whether to write example files.
        -> ExceptT CliError IO () -- ^ Action to run the example.
example writeFiles =

  do

    ownPubKey <-
      fmap (PubKeyHash . toBuiltin)
        . liftCli
        $ Base16.decode "d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657"
    let
      party = PK ownPubKey :: Party
      adatoken = Token adaSymbol adaToken :: Token
      testnet = Testnet $ NetworkMagic 1097911063
    Just costModelParams <- pure defaultCostModelParams

    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "===== Contract 0 ====="
    let
      state0 =
        State
        {
          accounts    = AM.singleton (party, adatoken) 8_000_000
        , choices     = AM.empty
        , boundValues = AM.empty
        , minSlot     = 40_000_000
        }
      contract0 = Close
      inputs0 = [] :: [Input]
    printMarlowe
      defaultMarloweParams costModelParams
      testnet NoStakeAddress
      contract0 state0 inputs0
    when writeFiles
      $ do
        liftIO
          . LBS.writeFile "example-0.state"
          $ encodePretty state0
        liftIO
          . LBS.writeFile "example-0.contract"
          $ encodePretty contract0
        liftIO
          . LBS.writeFile "example-0.inputs"
          $ encodePretty inputs0

    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "===== Contract 1 ====="
    let
      state1 =
        State
        {
          accounts    = AM.singleton (party, adatoken) 13_000_000
        , choices     = AM.empty
        , boundValues = AM.empty
        , minSlot     = 40_000_000
        }
      contract1 = Pay party (Party party) adatoken (Constant 5_000_000) contract0
      inputs1 = [] :: [Input]
    printMarlowe
      defaultMarloweParams costModelParams
      testnet NoStakeAddress
      contract1 state1 inputs1
    when writeFiles
      $ do
        liftIO
          . LBS.writeFile "example-1.state"
          $ encodePretty state1
        liftIO
          . LBS.writeFile "example-1.contract"
          $ encodePretty contract1
        liftIO
          . LBS.writeFile "example-1.inputs"
          $ encodePretty inputs1

    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "===== Contract 2 ====="
    let
      state2 =
        State
        {
          accounts    = AM.singleton (party, adatoken) 3_000_000
        , choices     = AM.empty
        , boundValues = AM.empty
        , minSlot     = 40_000_000
        }
      contract2 = When [Case (Deposit party party ada (Constant 10_000_000)) contract1] 45_000_000 Close
      inputs2 = [IDeposit party party adatoken 10_000_000]
    printMarlowe
      defaultMarloweParams costModelParams
      testnet NoStakeAddress
      contract2 state2 inputs2
    when writeFiles
      $ do
        liftIO
          . LBS.writeFile "example-2.state"
          $ encodePretty state2
        liftIO
          . LBS.writeFile "example-2.contract"
          $ encodePretty contract2
        liftIO
          . LBS.writeFile "example-2.inputs"
          $ encodePretty inputs2
