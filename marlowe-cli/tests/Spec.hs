-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Marlowe tests.
module Main (
  -- * Testing
  main,
) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Analysis qualified (tests)

-- | Entry point for the tests.
main :: IO ()
main = defaultMain tests

-- | Run the tests.
tests :: TestTree
tests =
  testGroup
    "Marlowe CLI"
    [ Spec.Analysis.tests
    ]
