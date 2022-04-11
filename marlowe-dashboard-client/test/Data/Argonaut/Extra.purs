module Test.Data.Argonaut.Extra where

import Prologue

import Data.Argonaut (Json)
import Data.Function.Uncurried (Fn5, runFn5)

foreign import _compare :: Fn5 Ordering Ordering Ordering Json Json Ordering

bigIntCompare :: Json -> Json -> Ordering
bigIntCompare = runFn5 _compare EQ GT LT

bigIntEq :: Json -> Json -> Boolean
bigIntEq a b = compare a b == EQ

bigIntNeq :: Json -> Json -> Boolean
bigIntNeq a b = compare a b /= EQ
