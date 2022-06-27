
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Marlowe.Semantics (
  tests
) where

import Control.Monad (replicateM)
import Language.Marlowe.Semantics
import Language.Marlowe.Semantics.Types
import Plutus.V1.Ledger.Api (CurrencySymbol (..), POSIXTime (..), PubKeyHash (..), TokenName (..), adaSymbol, adaToken,
                             toBuiltin)
import Spec.Marlowe.Common
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Prelude as P


tests :: TestTree
tests =
  testGroup "Semantics"
    [
      testGroup "Evaluation of Value"
      [
        testProperty "AvailableMoney" checkAvailableMoney
      , testProperty "Constant" checkConstant
      , testProperty "NegValue" checkNegValue
      , testProperty "AddValue" checkAddValue
      , testProperty "SubValue" checkSubValue
      , testProperty "MulValue" checkMulValue
      , testGroup "DivValue"
        [
          testCase "Numerator and Denominator are zero" checkDivValueNumeratorDenominatorZero
        , testProperty "Numerator is zero" checkDivValueNumeratorZero
        , testProperty "Denominator is zero" checkDivValueDenominatorZero
        , testProperty "Exact multiple" checkDivValueMultiple
        , testProperty "Rounding" checkDivValueRounding
        ]
      , testProperty "ChoiceValue" checkChoiceValue
      , testProperty "TimeIntervalStart" checkTimeIntervalStart
      , testProperty "TimeIntervalEnd" checkTimeIntervalEnd
      , testProperty "UseValue" checkUseValue
      , testProperty "Cond" checkCond
      ]
    ]


instance Arbitrary POSIXTime where
  arbitrary = POSIXTime <$> arbitrary

instance Arbitrary CurrencySymbol where
  arbitrary = CurrencySymbol . toBuiltin . BS.pack <$> replicateM 28 arbitrary

instance Arbitrary TokenName where
  arbitrary = TokenName . toBuiltin . BS8.pack <$> replicateM 32 arbitrary

instance Arbitrary Token where
  arbitrary =
     do
       isAda <- frequency [(7, pure True), (3, pure False)]
       if isAda
         then pure $ Token adaSymbol adaToken
         else Token <$> arbitrary <*> arbitrary

instance Arbitrary PubKeyHash where
  arbitrary = PubKeyHash . toBuiltin . BS.pack <$> replicateM 28 arbitrary

instance Arbitrary Party where
  arbitrary =
    do
       isPubKeyHash <- frequency [(1, pure True), (9, pure False)]
       if isPubKeyHash
         then PK <$> arbitrary
         else Role <$> arbitrary

instance Arbitrary ValueId where
  arbitrary =
    do
      n <- chooseInt (0, 64)
      ValueId . toBuiltin . BS8.pack <$> replicateM n arbitrary

instance Arbitrary ChoiceId where
  arbitrary =
    do
      n <- chooseInt (0, 64)
      ChoiceId <$> (toBuiltin . BS8.pack <$> replicateM n arbitrary) <*> arbitrary

genChoiceName :: Gen ChoiceName
genChoiceName =
  do
    n <- chooseInt (0, 64)
    toBuiltin . BS8.pack <$> replicateM n arbitrary

genTimeInterval :: Gen TimeInterval
genTimeInterval =
  do
    start <- arbitrary
    count <- suchThat arbitrary (> 0)
    pure (start, start + count)

genAccounts :: Gen Accounts
genAccounts =
  do
    accounts <- replicateM 10 arbitrary
    tokens <- replicateM 10 arbitrary
    entries <- chooseInt (0, 10)
    fmap AM.fromList
      . replicateM entries
      $ (,) <$> elements accounts <*> elements tokens


genAssocMap :: Arbitrary k
            => Arbitrary v
            => Gen (AM.Map k v)
genAssocMap =
  do
    entries <- chooseInt (0, 10)
    fmap AM.fromList
      . replicateM entries
      $ (,) <$> arbitrary <*> arbitrary


instance Arbitrary State where
  arbitrary = State <$> genAccounts <*> genAssocMap <*> genAssocMap <*> arbitrary


instance Arbitrary Environment where
  arbitrary = Environment <$> genTimeInterval


checkValue :: Show a
           => (Environment -> State -> Gen a)
           -> ((Value Observation -> Integer) -> (Observation -> Bool) -> Environment -> State -> a -> Bool)
           -> Property
checkValue gen f =
  property $ do
  let gen' = do
        environment <- arbitrary
        state <- arbitrary
        x <- gen environment state
        pure (environment, state, x)
  forAll gen' $ \(environment, state, x) ->
    f (evalValue environment state) (evalObservation environment state) environment state x


checkAvailableMoney :: Property
checkAvailableMoney =
  let
     gen _ State{accounts} =
       do
         isElement <- frequency [(9, pure True), (1, pure False)]
         if isElement && not (AM.null accounts)
           then elements $ AM.keys accounts
           else (,) <$> arbitrary <*> arbitrary
  in
    checkValue gen $ \eval _ _ State{accounts} (account, token) ->
      let
        x = AvailableMoney account token
      in
        case (account, token) `AM.lookup` accounts of
          Nothing -> eval x == 0
          Just x' -> eval x == x'

checkConstant :: Property
checkConstant =
  checkValue (const . const $ arbitrary) $ \eval _ _ _ x ->
    eval (Constant x) == x


checkNegValue :: Property
checkNegValue =
  checkValue (const . const $ valueGen) $ \eval _ _ _ x ->
    eval (NegValue x) == - eval x


checkAddValue :: Property
checkAddValue =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval _ _ _ (x, y) ->
      eval (AddValue x y) == eval x + eval y


checkSubValue :: Property
checkSubValue =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval _ _ _ (x, y) ->
      eval (SubValue x y) == eval x - eval y


checkMulValue :: Property
checkMulValue =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval _ _ _ (x, y) ->
      eval (MulValue x y) == eval x * eval y


checkDivValueNumeratorDenominatorZero :: Assertion
checkDivValueNumeratorDenominatorZero =
  assertBool "DivValue 0 0 = 0"
    $ evalValue undefined undefined (DivValue (Constant 0) (Constant 0)) == 0


checkDivValueNumeratorZero :: Property
checkDivValueNumeratorZero =
  checkValue (const . const $ valueGen) $ \eval _ _ _ x ->
    eval (DivValue (Constant 0) x) == 0


checkDivValueDenominatorZero :: Property
checkDivValueDenominatorZero =
  checkValue (const . const $ valueGen) $ \eval _ _ _ x ->
    eval (DivValue x (Constant 0)) == 0


checkDivValueMultiple :: Property
checkDivValueMultiple =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval _ _ _ (x, n) ->
      eval (DivValue (MulValue x n) n) == eval x || eval n == 0


roundedDivide :: Integer
              -> Integer
              -> Integer
roundedDivide x y = maybe 0 P.round $ x `P.ratio` y


checkDivValueRounding :: Property
checkDivValueRounding =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval _ _ _ (x, y) ->
      eval (DivValue x y) == eval x `roundedDivide` eval y || eval y == 0


checkChoiceValue :: Property
checkChoiceValue =
  let
     gen _ State{choices} =
       do
         isElement <- frequency [(9, pure True), (1, pure False)]
         if isElement && not (AM.null choices)
           then elements $ AM.keys choices
           else arbitrary
  in
    checkValue gen $ \eval _ _ State{choices} choice ->
      let
        x = ChoiceValue choice
      in
        case choice `AM.lookup` choices of
          Nothing -> eval x == 0
          Just x' -> eval x == x'


checkTimeIntervalStart :: Property
checkTimeIntervalStart =
  checkValue (const . const $ pure ()) $ \eval _ Environment{timeInterval} _ () ->
    POSIXTime (eval TimeIntervalStart) == fst timeInterval


checkTimeIntervalEnd :: Property
checkTimeIntervalEnd =
  checkValue (const . const $ pure ()) $ \eval _ Environment{timeInterval} _ () ->
    POSIXTime (eval TimeIntervalEnd) == snd timeInterval


checkUseValue :: Property
checkUseValue =
  let
     gen _ State{boundValues} =
       do
         isElement <- frequency [(9, pure True), (1, pure False)]
         if isElement && not (AM.null boundValues)
           then elements $ AM.keys boundValues
           else arbitrary
  in
    checkValue gen $ \eval _ _ State{boundValues} variable ->
      let
        x = UseValue variable
      in
        case variable `AM.lookup` boundValues of
          Nothing -> eval x == 0
          Just x' -> eval x == x'


checkCond :: Property
checkCond =
  checkValue (const . const $ (,,) <$> observationGen <*> valueGen <*> valueGen) $ \eval eval' _ _ (condition, thenValue, elseValue) ->
    eval (Cond condition thenValue elseValue) == (if eval' condition then eval thenValue else eval elseValue)
