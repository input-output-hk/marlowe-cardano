-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Check the entropy of arbitrary instances.
--
-----------------------------------------------------------------------------


module Spec.Marlowe.Semantics.Entropy
  ( -- * Testing
    checkEntropy
  , tests
  ) where


import Control.Monad (replicateM)
import Data.List (group, sort)
import Language.Marlowe.Core.V1.Semantics.Types (Accounts, ChoiceId, ChosenNum, Party, Token, ValueId)
import Plutus.V2.Ledger.Api (CurrencySymbol, PubKeyHash, TokenName)
import Spec.Marlowe.Semantics.Arbitrary (arbitraryChoiceName)
import Spec.Marlowe.Semantics.Orphans ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck (Arbitrary(arbitrary), Gen, generate)

import qualified PlutusTx.AssocMap as AM (Map, keys)


-- | Check the entropy of an arbitrary instance.
checkEntropy :: Ord a
             => Int               -- ^ The number of values to generate.
             -> (Double, Double)  -- ^ The valid rand for the entropy.
             -> Gen a             -- ^ The generator being tested.
             -> Assertion         -- ^ The test.
checkEntropy n (min', max') gen =
  do
    sample'' <- generate $ replicateM n gen
    let
      n' = fromIntegral n
      histogram = fmap (fromIntegral . length) . group . sort $ sample''
      entropy = sum $ (\f -> - f * logBase 2 f) . (/ n') <$> histogram
    assertBool ("!(" <> show min' <> " <= " <> show entropy <> " <= " <> show max' <> ")")
      $ min' <= entropy && entropy <= max'


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Arbitrary"
    [
      testGroup "Entropy"
        [
          testCase "PubKeyHash"     $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen PubKeyHash                               )
        , testCase "CurrencySymbol" $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen CurrencySymbol                           )
        , testCase "TokenName"      $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen TokenName                                )
        , testCase "Token"          $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen Token                                    )
        , testCase "Party"          $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen Party                                    )
        , testCase "ChoiceName"     $ checkEntropy 1000 (logBase 2 5, logBase 2 100)  arbitraryChoiceName
        , testCase "ChoiceId"       $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen ChoiceId                                 )
        , testCase "ValueId"        $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen ValueId                                  )
        , testCase "accounts"       $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> (arbitrary :: Gen Accounts                   ))
        , testCase "choices"        $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> (arbitrary :: Gen (AM.Map ChoiceId ChosenNum)))
        , testCase "boundValues"    $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> (arbitrary :: Gen (AM.Map ValueId Integer   )))
        ]
    ]
