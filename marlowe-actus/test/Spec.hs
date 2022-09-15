module Main
  ( main
  ) where

import Spec.Actus.Examples
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "ACTUS test cases"
      [ testGroup
          "ACTUS examples"
          [ Spec.Actus.Examples.tests
          ]
      ]
