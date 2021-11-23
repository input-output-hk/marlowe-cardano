module Data.FloatParser (parseFloat) where

import Prologue
import Data.Number (isNaN)

foreign import unsafeParseFloat :: String -> Number

parseFloat :: String -> Maybe Number
parseFloat s =
  let
    x = unsafeParseFloat s
  in
    if isNaN x then
      Nothing
    else
      Just x
