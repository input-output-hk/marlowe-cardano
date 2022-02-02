{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import qualified Spec.Marlowe.Contracts

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Marlowe"
    [ testGroup "Contracts" [ Spec.Marlowe.Contracts.tests ]
    ]
