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


{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Spec.Marlowe.Plutus.Specification (
-- * Testing
  tests
) where


import Control.Lens ((^.))
import Control.Lens.Tuple (_5)
import Data.Bifunctor (bimap)
import Data.List (permutations)
import Data.Proxy
import Data.These
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Scripts
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Value
import Spec.Marlowe.Plutus.Arbitrary (arbitraryPayoutTransaction, arbitrarySemanticsTransaction)
import Spec.Marlowe.Plutus.Script
import Spec.Marlowe.Plutus.Types ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, elements, forAll, property, suchThat, testProperty)


-- | Run tests.
tests :: TestTree
tests =
  testGroup "Marlowe On-Chain Specification"
    [
      testGroup "Semantics Validator"
        [
          testGroup "Valid transaction succeeds"
            [
              testProperty "Noiseless" $ checkSemanticsTransaction False
            , testProperty "Noisy"     $ checkSemanticsTransaction True
            ]
        , testGroup "Constraint 1. Typed validation"
            [
              testProperty "Valid datum deserializes"                    $ check1Valid   (arbitrary :: Gen MarloweData  )
            , testProperty "Valid redeemer deserializes"                 $ check1Valid   (arbitrary :: Gen MarloweInput )
            , testProperty "Valid script context deserializes"           $ check1Valid   (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize"          $ check1Invalid (Proxy :: Proxy MarloweData  ) True  True True
            , testProperty "Invalid redeemer does not deserialize"       $ check1Invalid (Proxy :: Proxy MarloweInput ) False True True
            , testProperty "Invalid script context does not deserialize" $ check1Invalid (Proxy :: Proxy ScriptContext) True  True True
            ]
        , testGroup "Constraint 2. Single Marlowe script input"
            [
              testProperty "Invalid attempt to run two Marlowe scripts" checkDoubleInput
            ]
        , testGroup "Constraint 3. Single Marlowe output"
            [
              testProperty "Invalid attempt to split Marlowe output" checkMultipleOutput
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
              -- TODO: This test requires instrumenting the Plutus script.
            ]
        , testGroup "Constraint 8. Input contract"
            [
              -- TODO: This test requires instrumenting the Plutus script.
            ]
        , testGroup "Constraint 9. Marlowe parameters"
            [
              -- TODO: This test requires instrumenting the Plutus script.
            ]
        , testGroup "Constraint 10. Output state"
            [
            ]
        , testGroup "Constraint 11. Output contract"
            [
            ]
        , testGroup "Constraint 12. Merkleized continuations"
            [
              -- TODO: Add merkleization support to contract generation.
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
          testGroup "Valid transaction succeeds"
            [
              testProperty "Noiseless" $ checkPayoutTransaction False
            , testProperty "Noisy"     $ checkPayoutTransaction True
            ]
        , testGroup "Constraint 16. Typed validation"
            [
              testProperty "Valid datum deserializes"                    $ check1Valid   (arbitrary :: Gen TokenName    )
            , testProperty "Valid redeemer deserializes"                 $ check1Valid   (arbitrary :: Gen ()           )
            , testProperty "Valid script context deserializes"           $ check1Valid   (arbitrary :: Gen ScriptContext)
            , testProperty "Invalid datum does not deserialize"          $ check1Invalid (Proxy :: Proxy TokenName    ) True  False True
            , testProperty "Invalid redeemer does not deserialize"       $ check1Invalid (Proxy :: Proxy ()           ) False True  False
            , testProperty "Invalid script context does not deserialize" $ check1Invalid (Proxy :: Proxy ScriptContext) True  True  True
            ]
        , testGroup "Constraint 17. Payment authorized"
            [
            ]
        ]
    ]


-- | Check round-trip serialization of `Data`.
check1Valid :: (FromData a, ToData a, Eq a, Show a) => Gen a -> Property
check1Valid gen =
  property
    . forAll gen
    $ \x -> fromBuiltinData (toBuiltinData x) == Just x


-- | Check that invalid `BuiltinData` fails to deserialize to `Data`.
check1Invalid :: forall a . (FromData a, Eq a) => Proxy a -> Bool -> Bool -> Bool -> Property
check1Invalid _ allowEmptyList allowByteString allowUnit =
  property
    $ let
        gen =
          let
            restrictEmptyList (BuiltinData (List [])) = allowEmptyList
            restrictEmptyList _                       = True
            restrictByteString (BuiltinData (B _)) = allowByteString
            restrictByteString _                   = True
            restrictUnit (BuiltinData (Constr 0 [])) = allowUnit
            restrictUnit _                           = True
          in
            -- FIXME: There is a very slight chance that a valid item might be generated at random.
            arbitrary `suchThat` (\x -> restrictEmptyList x && restrictByteString x && restrictUnit x)
      in
        forAll gen
          $ \x -> fromBuiltinData x == (Nothing :: Maybe a)


-- | Check that a semantically valid transaction succeeds.
checkSemanticsTransaction :: Bool -> Property
checkSemanticsTransaction noisy =
  property
    . forAll (arbitrarySemanticsTransaction noisy)
    $ \(marloweParams, marloweData, marloweInput, scriptContext, _) ->
      case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
        This  e   -> error $ show e
        These e l -> error $ show (e, l)
        That    _ -> True


-- | Check that a valid payout transaction succeeds.
checkPayoutTransaction :: Bool -> Property
checkPayoutTransaction noisy =
  property
    . forAll (arbitraryPayoutTransaction noisy)
    $ \(marloweParams, role, scriptContext) ->
      case evaluatePayout marloweParams (toData role) (toData ()) (toData scriptContext) of
        This  e   -> error $ show e
        These e l -> error $ show (e, l)
        That    _ -> True


-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: Property
checkDoubleInput =
  property
    $ let
        gen =
          do
            (marloweParams, marloweData, marloweInput, scriptContext, _) <- arbitrarySemanticsTransaction False
            txInInfoOutRef <- arbitrary
            txOutValue <- arbitrary
            txOutDatumHash <- Just <$> arbitrary
            let
              txOutAddress = semanticsAddress marloweParams
              txInInfoResolved = TxOut{..}
            txInfoInputs' <- elements . permutations $ TxInInfo{..} : txInfoInputs (scriptContextTxInfo scriptContext)
            let
              scriptContext' = scriptContext {scriptContextTxInfo = (scriptContextTxInfo scriptContext) {txInfoInputs = txInfoInputs'}}
            pure (marloweParams, marloweData, marloweInput, scriptContext')
      in
        forAll gen
          $ \(marloweParams, marloweData, marloweInput, scriptContext) ->
            case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
              That{} -> False
              _      -> True


-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: Property
checkMultipleOutput =
  property
    $ let
        gen =
          do
            (marloweParams, marloweData, marloweInput, scriptContext, _) <-
              arbitrarySemanticsTransaction False
                `suchThat` ((/= Close) . txOutContract . (^. _5))
            let
              ownAddress = semanticsAddress marloweParams
              outputs = txInfoOutputs (scriptContextTxInfo scriptContext)
              matchOwnOutput (TxOut address _ _) = address == ownAddress
              ownOutputs' =
                concat
                  [
                    let
                      (half, half') =
                        bimap mconcat mconcat
                          $ unzip
                          [
                            (singleton c n (i `div` 2), singleton c n (i - (i `div` 2)))
                          |
                            (c, n, i) <- flattenValue value
                          ]
                    in
                      [
                        TxOut address half  datum
                      , TxOut address half' datum
                      ]
                  |
                    TxOut address value datum <- filter matchOwnOutput outputs
                  ]
            txInfoOutputs' <- elements . permutations $ filter (not . matchOwnOutput) outputs <> ownOutputs'
            let
              scriptContext' = scriptContext {scriptContextTxInfo = (scriptContextTxInfo scriptContext) {txInfoOutputs = txInfoOutputs'}}
            pure (marloweParams, marloweData, marloweInput, scriptContext')
      in
        forAll gen
          $ \(marloweParams, marloweData, marloweInput, scriptContext) ->
            case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
              That{} -> False
              _      -> True
