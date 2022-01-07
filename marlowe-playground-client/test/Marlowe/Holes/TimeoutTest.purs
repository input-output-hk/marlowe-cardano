module Marlowe.Holes.TimeoutTest where

import Prologue
import Marlowe.Gen (genContract, GenerationOptions(..))
import Marlowe.GenWithHoles (GenWithHoles, contractQuickCheck)
import Marlowe.Holes (fromTerm)
import Marlowe.Semantics (timeouts)
import Marlowe.Semantics as S
import Test.QuickCheck (Result, (===))
import Test.Spec (Spec, describe, it)

all :: Spec Unit
all =
  describe "Marlowe.Holes.Timeout" do
    it "Term and Semantic contract has the same timeouts" $ contractQuickCheck
      (GenerationOptions { withHoles: false, withExtendedConstructs: false })
      sameTimeouts

sameTimeouts :: GenWithHoles Result
sameTimeouts = do
  termContract <- genContract
  let
    (mSContract :: Maybe S.Contract) = fromTerm termContract

    termTimeouts = timeouts termContract

    mSemanticTimeouts = timeouts <$> mSContract
  pure (Just termTimeouts === mSemanticTimeouts)
