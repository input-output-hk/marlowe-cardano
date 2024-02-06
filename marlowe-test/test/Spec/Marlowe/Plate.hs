-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Marlowe.Plate (
  tests,
) where

import Test.Tasty (TestTree, testGroup)

import Data.Map.Strict as M
import Data.Set as S
import qualified Language.Marlowe.Core.V1.Plate as P
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified PlutusLedgerApi.V2 as PV2
import qualified PlutusTx.AssocMap as AM
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "Plate"
    [ testCase "Parties are extracted from state, contract and continuations" extractPartiesTest
    , testCase "Parties are extracted from Pay contract" extractPayContractPartiesTest
    , testCase "Tokens are extracted from Pay contract" extractPayContractTokensTest
    ]

extractPartiesTest :: IO ()
extractPartiesTest = do
  let ada = V1.Token PV2.adaSymbol PV2.adaToken
      alice = V1.Role "alice"
      bob = V1.Role "bob"
      hash = "hash"
      datumHash = PV2.DatumHash "hash"
      contract =
        V1.Pay
          alice
          (V1.Account bob)
          ada
          (V1.Constant 1)
          (V1.When [V1.MerkleizedCase (V1.Notify V1.TrueObs) hash] (PV2.POSIXTime 0) V1.Close)

      charlie = V1.Role "charlie"
      continuations = M.fromList [(datumHash, V1.Pay charlie (V1.Account alice) ada (V1.Constant 1) V1.Close)]

      eve = V1.Role "eve"
      accounts = AM.fromList [((eve, ada), 1)]
      state = (V1.emptyState (PV2.POSIXTime 0)){V1.accounts = accounts}

      actual = P.extractParties (Just state) contract continuations
  assertEqual "extracted parties" (S.fromList [alice, bob, charlie, eve]) actual

extractPayContractPartiesTest :: IO ()
extractPayContractPartiesTest = do
  let ada = V1.Token PV2.adaSymbol PV2.adaToken
      alice = V1.Role "alice"
      bob = V1.Role "bob"
      contract = V1.Pay alice (V1.Account bob) ada (V1.Constant 1) V1.Close
      actual = P.extractAll contract
  assertEqual "extracted parties" (S.fromList [alice, bob]) actual

extractPayContractTokensTest :: IO ()
extractPayContractTokensTest = do
  let ada = V1.Token PV2.adaSymbol PV2.adaToken
      alice = V1.Role "alice"
      bob = V1.Role "bob"
      contract = V1.Pay alice (V1.Account bob) ada (V1.Constant 1) V1.Close
      actual = P.extractAll contract
  assertEqual "extracted parties" (S.fromList [ada]) actual
