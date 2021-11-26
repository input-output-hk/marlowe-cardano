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


{-# LANGUAGE OverloadedStrings #-}


module Main (
-- * Entry Point
  main
) where


import           Cardano.Api                     (NetworkId (..), NetworkMagic (..), StakeAddressReference (..))
import           Control.Monad.Except            (ExceptT, liftIO)
import           Language.Marlowe.CLI            (mainCLI)
import           Language.Marlowe.CLI.Export     (printMarlowe)
import           Language.Marlowe.CLI.Types      (CliError (..), liftCli)
import           Language.Marlowe.Client         (defaultMarloweParams)
import           Language.Marlowe.SemanticsTypes (Action (..), Case (..), Contract (..), Input (..), InputContent (..),
                                                  Party (..), State (..), Token (..), Value (..))
import           Language.Marlowe.Util           (ada)
import           Ledger.Ada                      (adaSymbol, adaToken)
import           Paths_marlowe                   (version)
import           Plutus.V1.Ledger.Api            (PubKeyHash (..), defaultCostModelParams, toBuiltin)

import qualified Data.ByteString.Base16          as Base16 (decode)
import qualified PlutusTx.AssocMap               as AM (empty, singleton)


-- | Run the Marlow CLI tool.
main :: IO () -- ^ Action to run the tool.
main = mainCLI version example


-- | Hardwired example, which can be run by executing `marlowe-cli example`.
example :: ExceptT CliError IO ()
example =

  do

    ownPubKey <-
      fmap (PubKeyHash . toBuiltin)
        . liftCli
        $ Base16.decode "d7604c51452bf9c135d63c686ba306d268fcae8494c877e12c44c657"
    let
      party = PK ownPubKey
      slotRange = (1000, 43000000)
      adatoken = Token adaSymbol adaToken
      testnet = Testnet $ NetworkMagic 1097911063
    Just costModelParams <- pure defaultCostModelParams

    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "===== Contract 1 ====="
    let
      state1 =
        State
        {
          accounts    = AM.singleton (party, adatoken) 3000000
        , choices     = AM.empty
        , boundValues = AM.empty
        , minSlot     = 10
        }
      contract1 = Close
      inputs1 = []
    printMarlowe
      defaultMarloweParams costModelParams
      testnet NoStakeAddress
      contract1 state1 inputs1
      slotRange

    liftIO $ putStrLn ""
    liftIO $ putStrLn ""
    liftIO $ putStrLn "===== Contract 2 ====="
    let
      state2 =
        State
        {
          accounts    = AM.singleton (party, adatoken) 3000000
        , choices     = AM.empty
        , boundValues = AM.empty
        , minSlot     = 42293000
        }
      contract2 = When [Case (Deposit party party ada (Constant 12000000)) Close] 42294000 Close
      inputs2 = [NormalInput $ IDeposit party party adatoken 12000000]
    printMarlowe
      defaultMarloweParams costModelParams
      testnet NoStakeAddress
      contract2 state2 inputs2
      slotRange
