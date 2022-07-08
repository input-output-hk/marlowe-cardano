
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Util (
  roundedDivide
, flattenMoney
, stateEq
, checkEntropy
) where


import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (group, sort)
import Language.Marlowe.Core.V1.Semantics.Money (Money)
import qualified Language.Marlowe.Core.V1.Semantics.Money as Money
import Language.Marlowe.Core.V1.Semantics.Token
import Language.Marlowe.Core.V1.Semantics.Types
import Spec.Marlowe.Util.AssocMap
import Test.Tasty.HUnit (Assertion, assertBool)
import Test.Tasty.QuickCheck (Gen, generate)

import qualified PlutusTx.Prelude as P


canonicalState :: State Token -> State Token
canonicalState State{..} =
  State
    (assocMapSort accounts)
    (assocMapSort choices)
    (assocMapSort boundValues)
    minTime


stateEq :: State Token -> State Token -> Bool
stateEq = (==) `on` canonicalState


flattenMoney :: Money Token -> [(Token, Integer)]
flattenMoney = Money.toList


roundedDivide :: Integer
              -> Integer
              -> Integer
roundedDivide x y = maybe 0 P.round $ x `P.ratio` y


checkEntropy :: Ord a => Int -> (Double, Double) -> Gen a -> Assertion
checkEntropy n (min', max') gen =
  do
    sample'' <- generate $ replicateM n gen
    let
      n' = fromIntegral n
      histogram = fmap (fromIntegral . length) . group . sort $ sample''
      entropy = sum $ (\f -> - f * logBase 2 f) . (/ n') <$> histogram
    assertBool ("!(" <> show min' <> " <= " <> show entropy <> " <= " <> show max' <> ")")
      $ min' <= entropy && entropy <= max'
