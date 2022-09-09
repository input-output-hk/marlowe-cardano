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
import Spec.Marlowe.Plutus.Arbitrary (SemanticsTest, SemanticsTest', arbitraryPayoutTransaction,
                                      arbitrarySemanticsTransaction, arbitrarySemanticsTransactionModifyState)
import Spec.Marlowe.Plutus.Script
import Spec.Marlowe.Plutus.Types ()
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, Property, elements, forAll, property, suchThat, testProperty)

import qualified PlutusTx.AssocMap as AM


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
              testProperty "Invalid attempt to output to Marlowe on close" checkCloseOutput
            ]
        , testGroup "Constraint 5. Input value from script"
            [
              testProperty "Invalid mismatch between state and script input" checkValueInput
            ]
        , testGroup "Constraint 6. Output value to script"
            [
              testProperty "Invalid mismatch between state and script input" checkValueOutput
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
              testProperty "Invalid mismatch between state and script output" checkStateOutput
            ]
        , testGroup "Constraint 11. Output contract"
            [
              testProperty "Invalid mismatch between contract and script output" checkContractOutput
            ]
        , testGroup "Constraint 12. Merkleized continuations"
            [
            ]
        , testGroup "Constraint 13. Positive balances"
            [
              testProperty "Invalid non-positive balance" checkPositiveAccounts
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
        These e l -> error $ show e <> ": " <> show l
        That    _ -> True


-- | Check that an invalid semantics transaction fails.
checkInvalidSemantics :: (SemanticsTest -> Bool)
                      -> (SemanticsTest' -> Gen TxInfo)
                      -> Property
checkInvalidSemantics condition modify =
  property
    $ let
        gen =
          do
            (marloweParams, marloweData, marloweInput, scriptContext, output)
              <- arbitrarySemanticsTransaction False `suchThat` condition
            txInfo' <- modify (marloweParams, marloweData, marloweInput, scriptContextTxInfo scriptContext, output)
            pure (marloweParams, marloweData, marloweInput, scriptContext {scriptContextTxInfo = txInfo'})
      in
        forAll gen
          $ \(marloweParams, marloweData, marloweInput, scriptContext) ->
            case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
              That{} -> False
              _      -> True


-- | Check that an invalid semantics transaction fails.
checkInvalidSemanticsModifyState :: (SemanticsTest -> Bool)
                                 -> (State -> Gen State)
                                 -> Property
checkInvalidSemanticsModifyState condition modify =
  property
    . forAll (arbitrarySemanticsTransactionModifyState modify False `suchThat` condition)
      $ \(marloweParams, marloweData, marloweInput, scriptContext, _) ->
        case evaluateSemantics marloweParams (toData marloweData) (toData marloweInput) (toData scriptContext) of
          That{} -> False
          _      -> True


-- | Check that validation fails if two Marlowe scripts are run.
checkDoubleInput :: Property
checkDoubleInput =
  checkInvalidSemantics (const True)
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        txInInfoOutRef <- arbitrary
        txOutValue <- arbitrary
        txOutDatumHash <- Just <$> arbitrary
        let
          txOutAddress = semanticsAddress marloweParams
          txInInfoResolved = TxOut{..}
        txInfoInputs' <- elements . permutations $ TxInInfo{..} : txInfoInputs txInfo
        pure $ txInfo {txInfoInputs = txInfoInputs'}


-- | Check that validation fails if there is more than one Marlowe output.
checkMultipleOutput :: Property
checkMultipleOutput =
  checkInvalidSemantics ((/= Close) . txOutContract . (^. _5))
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          outputs = txInfoOutputs txInfo
          matchOwnOutput (TxOut address _ _) = address == ownAddress
          ownOutputs' =
            concat
              [
                let
                  (half, half') =
                    bimap mconcat mconcat
                      $ unzip
                      [(singleton c n (i `div` 2), singleton c n (i - (i `div` 2))) | (c, n, i) <- flattenValue value]
                in
                  [TxOut address half datum, TxOut address half' datum]
              |
                TxOut address value datum <- filter matchOwnOutput outputs
              ]
        txInfoOutputs' <- elements . permutations $ filter (not . matchOwnOutput) outputs <> ownOutputs'
        pure $ txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that validation fails if there is more than one Marlowe output.
checkCloseOutput :: Property
checkCloseOutput =
  checkInvalidSemantics ((== Close) . txOutContract . (^. _5))
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          matchOwnInput (TxInInfo _ (TxOut address _ _)) = address == ownAddress
          inputs = txInfoInputs txInfo
          outputs = txInfoOutputs txInfo
          ownOutputs =
            [
              txOut
            |
              TxInInfo _ txOut <- filter matchOwnInput inputs
            ]
        txInfoOutputs' <- elements . permutations $ outputs <> ownOutputs
        pure $ txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that value input to a script matches its input state.
checkValueInput :: Property
checkValueInput =
  checkInvalidSemantics (const True)
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          txInfoInputs' =
            [
              if address == ownAddress
                then txInInfo {txInInfoResolved = txOut {txOutValue = value <> singleton adaSymbol adaToken 1}}
                else txInInfo
            |
              txInInfo@(TxInInfo _ txOut@(TxOut address value _)) <- txInfoInputs txInfo
            ]
        pure $ txInfo {txInfoInputs = txInfoInputs'}


-- | Check that value output to a script matches its output state.
checkValueOutput :: Property
checkValueOutput =
  checkInvalidSemantics ((/= Close) . txOutContract . (^. _5))
    $ \(marloweParams, _, _, txInfo, _) ->
      do
        let
          ownAddress = semanticsAddress marloweParams
          txInfoOutputs' =
            [
              if address == ownAddress
                then txOut {txOutValue = value <> singleton adaSymbol adaToken 1}
                else txOut
            |
              txOut@(TxOut address value _) <- txInfoOutputs txInfo
            ]
        pure $ txInfo {txInfoOutputs = txInfoOutputs'}


-- | Check that output datum to a script matches its semantic output.
checkDatumOutput :: (MarloweData -> Gen MarloweData) -> Property
checkDatumOutput modify =
  checkInvalidSemantics ((/= Close) . txOutContract . (^. _5))
    $ \(marloweParams, marloweData, _, txInfo, _) ->
      do
        marloweData' <- modify marloweData
        let
          ownAddress = semanticsAddress marloweParams
          outputHash =
            [
              h
            |
              TxOut address _ h <- txInfoOutputs txInfo
            , address == ownAddress
            ]
          txInfoData' =
            [
              if Just h `elem` outputHash
                then (h, Datum $ toBuiltinData marloweData')
                else pair
            |
              pair@(h, _) <- txInfoData txInfo
            ]
        pure $ txInfo {txInfoData = txInfoData'}


-- | Check that state output to a script matches its output state.
checkStateOutput :: Property
checkStateOutput =
  checkDatumOutput
    $ \marloweData ->
      do
        marloweState' <- arbitrary
        pure $ marloweData {marloweState = marloweState'}


-- | Check that state output to a script matches its output state.
checkContractOutput :: Property
checkContractOutput =
  checkDatumOutput
    $ \marloweData ->
      do
        marloweContract' <- arbitrary
        pure $ marloweData {marloweContract = marloweContract'}


-- | Check that non-positive accounts are rejected.
checkPositiveAccounts :: Property
checkPositiveAccounts =
  checkInvalidSemanticsModifyState (const True)
    $ \state ->
      do
        account <- arbitrary
        token <- arbitrary
        amount <- (1 -) <$> arbitraryPositiveInteger
        pure $ state {accounts = AM.insert (account, token) amount $ accounts state}
