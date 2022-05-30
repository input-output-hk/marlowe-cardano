module Contrib.Data.Decimal where

import Prelude

import Data.Array as A
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BI
import Data.Decimal (Decimal)
import Data.Decimal as D
import Data.Maybe (Maybe(..))
import Data.String as S
import Partial.Unsafe (unsafeCrashWith)

fromBigInt :: BigInt -> Decimal
fromBigInt i = case D.fromString (BI.toString i) of
  Just d -> d
  Nothing -> unsafeCrashWith $ "Decimal.fromBigInt` fails for a given input:" <>
    BI.toString i

toBigInt :: Decimal -> BigInt
toBigInt d = do
  let
    dString = D.toString d
  case A.head (S.split (S.Pattern ".") dString) >>= BI.fromString of
    Just i -> i
    Nothing -> unsafeCrashWith $ "Decimal.toBigInt` fails for a given input:" <>
      D.toString d
