-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of Marlowe's Plutus implementation against its on-chain specification.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ScopedTypeVariables #-}


module Spec.Marlowe.Plutus.Specification (
-- * Testing
  tests
) where


import Data.Proxy
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Scripts
import Plutus.V1.Ledger.Api
import Spec.Marlowe.Plutus.Arbitrary ()
import Spec.Marlowe.Plutus.Types ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, forAll, property, suchThat, testProperty)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Marlowe On-Chain Specification"
    [
      testGroup "Semantics Validator"
        [
          testGroup "Constraint 1. Typed validation"
            [
              testProperty "Valid datum deserializes"                    $ check1Valid   (arbitrary :: Gen MarloweData  )
            , testProperty "Valid redeemer deserializes"                 $ check1Valid   (arbitrary :: Gen MarloweInput )
            , testProperty "Valid script context deserializes"           $ check1Valid   (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize"          $ check1Invalid (Proxy :: Proxy MarloweData  ) True
            , testProperty "Invalid redeemer does not deserialize"       $ check1Invalid (Proxy :: Proxy MarloweInput ) False
            , testProperty "Invalid script context does not deserialize" $ check1Invalid (Proxy :: Proxy ScriptContext) True
            ]
        , testGroup "Constraint 2. Single Marlowe script input"
            [
            ]
        , testGroup "Constraint 3. Single Marlowe output"
            [
            ]
        , testGroup "Constraint 4. No output to script on close"
            [
            ]
        , testGroup "Constraint 5. Input value from script"
            [
            ]
        , testGroup "Constraint 6. Output value to script"
            [
            ]
        , testGroup "Constraint 7. Input state"
            [
            ]
        , testGroup "Constraint 8. Input contract"
            [
            ]
        , testGroup "Constraint 9. Marlowe parameters"
            [
            ]
        , testGroup "Constraint 10. Output state"
            [
            ]
        , testGroup "Constraint 11. Output contract"
            [
            ]
        , testGroup "Constraint 12. Merkleized continuations"
            [
            ]
        , testGroup "Constraint 13. Positive balances"
            [
            ]
        , testGroup "Constraint 14. Inputs authorized"
            [
            ]
        , testGroup "Constraint 15. Sufficient payment"
            [
            ]
        ]
    , testGroup "Payout Validator"
        [
          testGroup "Constraint 16. Typed validation"
            [
            ]
        , testGroup "Constraint 17. Payment authorized"
            [
            ]
        ]
    ]


check1Valid :: (FromData a, ToData a, Eq a, Show a) => Gen a -> Property
check1Valid gen =
  property
    . forAll gen
    $ \x -> fromBuiltinData (toBuiltinData x) == Just x


check1Invalid :: forall a . (FromData a, Eq a) => Proxy a -> Bool -> Property
check1Invalid _ allowEmptyList =
  property
    -- FIXME: There is a very slight chance that a valid item might be generated at random.
    . forAll (arbitrary `suchThat` (\x -> allowEmptyList || x /= BuiltinData (List [])))
    $ \x -> fromBuiltinData x == (Nothing :: Maybe a)
