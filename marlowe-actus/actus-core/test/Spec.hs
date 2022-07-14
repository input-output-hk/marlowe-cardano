module Main (main) where

import Spec.TestFramework (testCasesFromFile, tests)
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
  cshTests <- testCasesFromFile [] $ p ++ "actus-tests-csh.json"
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
  cegTests <-
    testCasesFromFile
      [ "guarantee14" -- result in reference test is not precise: 3508695 but should be 3508695.652174
      ] $ p ++ "actus-tests-ceg.json"
  -- cecTests <- testCasesFromFile [] $ p ++ "actus-tests-cec.json"

  defaultMain $
    testGroup
      "ACTUS test framework"
      [ testGroup
          "ACTUS test-framework"
          [ tests "PAM" pamTests,
            tests "LAM" lamTests,
            tests "NAM" namTests,
            tests "ANN" annTests,
            tests "STK" stkTests,
            tests "OPTNS" optnsTests,
            tests "FUTUR" futurTests,
            tests "COM" comTests,
            tests "CSH" cshTests,
            tests "CLM" clmTests,
            tests "SWPPV" swppvTests,
            tests "CEG" cegTests
          ]
      ]
