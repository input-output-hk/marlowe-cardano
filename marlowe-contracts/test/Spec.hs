{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import qualified Spec.Marlowe.Analysis
import qualified Spec.Marlowe.Contracts

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marlowe"
  [ testGroup "Marlowe unit tests" [ Spec.Marlowe.Contracts.tests ]
  , testGroup "Static analysis" [ Spec.Marlowe.Analysis.tests ]
  ]
