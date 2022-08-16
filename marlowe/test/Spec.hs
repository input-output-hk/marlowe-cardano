
{-# LANGUAGE OverloadedStrings #-}


module Main(
  main
) where


import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import qualified Spec.Marlowe.Marlowe (prop_contractJsonLoops, prop_marloweParamsJsonLoops, prop_noFalsePositives,
                                       tests)
import qualified Spec.Marlowe.Semantics (tests)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests =
  testGroup "Marlowe"
    [
      Spec.Marlowe.Marlowe.tests
    , testGroup "Static Analysis"
      [
        testProperty "No false positives" Spec.Marlowe.Marlowe.prop_noFalsePositives
      ]
    , testGroup "JSON Serialisation"
      [
        testProperty "Serialise deserialise Contract loops" Spec.Marlowe.Marlowe.prop_contractJsonLoops
      , testProperty "Serialise deserialise MarloweParams loops" Spec.Marlowe.Marlowe.prop_marloweParamsJsonLoops
      ]
    , Spec.Marlowe.Semantics.tests
    ]
