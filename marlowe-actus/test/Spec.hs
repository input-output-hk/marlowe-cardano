{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Spec.Actus.TestFramework
import Spec.Marlowe.ACTUS.Examples
import Spec.Marlowe.ACTUS.QCTests
import Spec.Marlowe.ACTUS.TestFrameworkMarlowe
import System.Environment
import Test.Tasty

main :: IO ()
main = do
  p <- getEnv "ACTUS_TEST_DATA_DIR"

  pamTests <- testCasesFromFile [] $ p ++ "actus-tests-pam.json"
  lamTests <- testCasesFromFile [] $ p ++ "actus-tests-lam.json"
  namTests <- testCasesFromFile [] $ p ++ "actus-tests-nam.json"
  annTests <-
    testCasesFromFile
      [ "ann09" -- ann09: currently unsupported, see also actus-core AnnuityTest.java
      ]
      $ p ++ "actus-tests-ann.json"
  stkTests <- testCasesFromFile [] $ p ++ "actus-tests-stk.json"
  optnsTests <- testCasesFromFile [] $ p ++ "actus-tests-optns.json"
  futurTests <- testCasesFromFile [] $ p ++ "actus-tests-futur.json"
  comTests <- testCasesFromFile [] $ p ++ "actus-tests-com.json"
  clmTests <-
    testCasesFromFile
      [ "clm07",
        "clm08",
        "clm09",
        "clm13",
        "clm14" -- same as in CallMoneyTest.java
      ]
      $ p ++ "actus-tests-clm.json"
  swppvTests <- testCasesFromFile [] $ p ++ "actus-tests-swppv.json"

  defaultMain $
    testGroup
      "ACTUS test cases"
      [ testGroup
          "ACTUS test-framework for Marlowe"
          [ Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "PAM" pamTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "LAM" lamTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "NAM" namTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "ANN" annTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "STK" stkTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "OPTNS" optnsTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "FUTUR" futurTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "COM" comTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "CLM" clmTests,
            Spec.Marlowe.ACTUS.TestFrameworkMarlowe.tests "SWPPV" swppvTests
          ],
        testGroup
          "ACTUS examples"
          [ Spec.Marlowe.ACTUS.Examples.tests
          ],
        testGroup
          "QuickCheck"
          [ Spec.Marlowe.ACTUS.QCTests.tests
          ]
      ]
