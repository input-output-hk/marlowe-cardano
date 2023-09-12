{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Tests of Marlowe's Plutus implementation against its on-chain specification.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Spec.Marlowe.Plutus.Specification (
  -- * Testing
  tests,
) where

import Data.Proxy (Proxy (..))
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import Language.Marlowe.Scripts.Types (MarloweInput)
import PlutusLedgerApi.V2 (
  BuiltinData (BuiltinData),
  Data (B, Constr, List),
  FromData (..),
  ScriptContext,
  ToData (..),
  TokenName,
 )
import Spec.Marlowe.Plutus.Arbitrary ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (
  Arbitrary (..),
  Gen,
  Property,
  forAll,
  property,
  suchThat,
  testProperty,
 )

-- | Run tests.
tests :: TestTree
tests =
  testGroup
    "Marlowe On-Chain Specification"
    [ testGroup
        "Semantics Validator"
        [ testGroup
            "Constraint 1. Typed validation"
            [ testProperty "Valid datum deserializes" $ check1Valid (arbitrary :: Gen MarloweData)
            , testProperty "Valid redeemer deserializes" $ check1Valid (arbitrary :: Gen MarloweInput)
            , testProperty "Valid script context deserializes" $ check1Valid (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize" $
                check1Invalid (Proxy :: Proxy MarloweData) True True True
            , testProperty "Invalid redeemer does not deserialize" $
                check1Invalid (Proxy :: Proxy MarloweInput) False True True
            , testProperty "Invalid script context does not deserialize" $
                check1Invalid (Proxy :: Proxy ScriptContext) True True True
            ]
        , testGroup
            "Constraint 7. Input state"
            []
        , -- TODO: This test requires instrumenting the Plutus script. For now, this constraint is enforced manually by code inspection.

          testGroup
            "Constraint 8. Input contract"
            []
        ] -- TODO: This test requires instrumenting the Plutus script. For now, this constraint is enforced manually by code inspection.
    , testGroup
        "Payout Validator"
        [ testGroup
            "Constraint 16. Typed validation"
            [ testProperty "Valid datum deserializes" $ check1Valid (arbitrary :: Gen TokenName)
            , testProperty "Valid redeemer deserializes" $ check1Valid (arbitrary :: Gen ())
            , testProperty "Valid script context deserializes" $ check1Valid (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize" $
                check1Invalid (Proxy :: Proxy TokenName) True False True
            , testProperty "Invalid redeemer does not deserialize" $
                check1Invalid (Proxy :: Proxy ()) False True False
            , testProperty "Invalid script context does not deserialize" $
                check1Invalid (Proxy :: Proxy ScriptContext) True True True
            ]
        ]
    ]

-- | Check round-trip serialization of `Data`.
check1Valid
  :: (FromData a, ToData a, Eq a, Show a)
  => Gen a
  -> Property
check1Valid gen =
  property
    . forAll gen
    $ \x -> fromBuiltinData (toBuiltinData x) == Just x

-- | Check that invalid `BuiltinData` fails to deserialize to `Data`.
check1Invalid
  :: forall a
   . (FromData a, Eq a)
  => Proxy a
  -- ^ The type for conversion to `BuiltinData`.
  -> Bool
  -- ^ Whether to allow an empty list in the `Data`.
  -> Bool
  -- ^ Whether to all byte strings in the `Data`.
  -> Bool
  -- ^ Whether to allow `()` in the `Data`.
  -> Property
  -- ^ The test property.
check1Invalid _ allowEmptyList allowByteString allowUnit =
  property $
    let gen =
          let restrictEmptyList (BuiltinData (List [])) = allowEmptyList
              restrictEmptyList _ = True
              restrictByteString (BuiltinData (B _)) = allowByteString
              restrictByteString _ = True
              restrictUnit (BuiltinData (Constr 0 [])) = allowUnit
              restrictUnit _ = True
           in -- TODO: There is a very slight chance that a valid item might be generated at random.
              arbitrary `suchThat` (\x -> restrictEmptyList x && restrictByteString x && restrictUnit x)
     in forAll gen $
          \x -> fromBuiltinData x == (Nothing :: Maybe a)
