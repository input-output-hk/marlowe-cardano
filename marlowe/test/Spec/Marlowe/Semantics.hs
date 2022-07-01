
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module Spec.Marlowe.Semantics (
  tests
) where


import Control.Monad (replicateM)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (group, groupBy, sort, sortBy)
import Data.Maybe (fromMaybe)
import Language.Marlowe.Semantics
import Language.Marlowe.Semantics.Types
import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime (..), PubKeyHash, TokenName)
import Plutus.V1.Ledger.Value (flattenValue)
import Spec.Marlowe.Arbitrary
import Spec.Marlowe.Common (caseRelGenSized, contractGen, observationGen, simpleIntegerGen, valueGen)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Prelude as P


tests :: [TestTree]
tests =
  [
    testGroup "Semantics"
      [
        testGroup "fixInterval"
        [
          testProperty "Invalid interval" $ checkFixInterval True  False
        , testProperty "Interval in past" $ checkFixInterval False True
        , testProperty "Interval trimmed" $ checkFixInterval False False
        ]
      , testGroup "evalValue"
        [
          testGroup "AvailableMoney"
          [
            testProperty "Account exists" $ checkAvailableMoney True
          , testProperty "Account does not exist" $ checkAvailableMoney False
          ]
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
        , testGroup "ChoiceValue"
          [
            testProperty "Choice exists" $ checkChoiceValue True
          , testProperty "Choice does not exist" $ checkChoiceValue False
          ]
        , testProperty "TimeIntervalStart" checkTimeIntervalStart
        , testProperty "TimeIntervalEnd" checkTimeIntervalEnd
        , testGroup "UseValue"
          [
            testProperty "Value exists" $ checkUseValue True
          , testProperty "Value does not exist" $ checkUseValue False
          ]
        , testProperty "Cond" checkCond
        ]
      , testGroup "evalObservation"
        [
          testProperty "AndObs" checkAndObs
        , testProperty "OrObs" checkOrObs
        , testProperty "NotObs" checkNotObs
        , testGroup "ChoseSomething"
          [
            testProperty "Choice exists" $ checkChoseSomething True
          , testProperty "Choice does not exist" $ checkChoseSomething False
          ]
        , testProperty "ValueGE" checkValueGE
        , testProperty "ValueGT" checkValueGT
        , testProperty "ValueLT" checkValueLT
        , testProperty "ValueLE" checkValueLE
        , testProperty "ValueEQ" checkValueEQ
        , testCase "TrueObs" checkTrueObs
        , testCase "FalseObs" checkFalseObs
        ]
      , testGroup "refundOne"
        [
          testProperty "No accounts"       $ checkRefundOne (== 0)
        , testProperty "One account"       $ checkRefundOne (== 1)
        , testProperty "Multiple accounts" $ checkRefundOne (>= 2)
        ]
      , testProperty "moneyInAccount" checkMoneyInAccount
      , testProperty "updateMoneyInAccount" checkUpdateMoneyInAccount
      , testProperty "addMoneyToAccount" checkAddMoneyToAccount
      , testProperty "giveMoney" checkGiveMoney
      , testGroup "reduceContractStep"
        [
          testProperty "Close" checkReduceContractStepClose
        , testProperty "Pay" checkReduceContractStepPay
        , testProperty "If" checkReduceContractStepIf
        , testProperty "When" checkReduceContractStepWhen
        , testProperty "Let" checkReduceContractStepLet
        , testProperty "Assert" checkReduceContractStepAssert
        ]
      , testGroup "applyAction"
        [
          testProperty "Input does not match action" checkApplyActionMismatch
        , testGroup "IDeposit"
          [
            testProperty "AccountId does not match"                  $ checkIDeposit (Just False) Nothing     Nothing      Nothing
          , testProperty "Party does not match"                      $ checkIDeposit Nothing     (Just False) Nothing      Nothing
          , testProperty "Token does not match"                      $ checkIDeposit Nothing      Nothing     (Just False) Nothing
          , testProperty "Amount does not match"                     $ checkIDeposit Nothing      Nothing     Nothing      (Just False)
          , testProperty "AccountId, party, token, and amount match" $ checkIDeposit (Just True) (Just True)  (Just True)  (Just True)
          ]
        , testGroup "IChoice"
          [
            testProperty "ChoiceId does not match"       $ checkIChoice (Just False) Nothing
          , testProperty "ChoiceNum out of bounds" $ checkIChoice Nothing      (Just False)
          , testProperty "ChoiceNum in bounds"     $ checkIChoice (Just True)  (Just True)
          ]
        , testProperty "INotify" checkINotify
        ]
      ]
  , testGroup "Entropy of Arbitrary"
      [
        testCase "PubKeyHash"     $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen PubKeyHash     )
      , testCase "CurrencySymbol" $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen CurrencySymbol )
      , testCase "TokenName"      $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen TokenName      )
      , testCase "Token"          $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen Token          )
      , testCase "Party"          $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen Party          )
      , testCase "ChoiceName"     $ checkEntropy 1000 (logBase 2 5, logBase 2 100)  arbitraryChoiceName
      , testCase "ChoiceId"       $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen ChoiceId       )
      , testCase "ValueId"        $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (arbitrary :: Gen ValueId        )
      , testCase "accounts"       $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> arbitraryAccounts   )
      , testCase "choices"        $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> arbitraryChoices    )
      , testCase "boundValues"    $ checkEntropy 1000 (logBase 2 5, logBase 2 100) (AM.keys <$> arbitraryBoundValues)
      ]
    ]


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


checkFixInterval :: Bool -> Bool -> Property
checkFixInterval invalid inPast =
  property $ do
  let gen = do
        state <- arbitrary
        end   <- arbitrary `suchThat` (\t -> (t < minTime state) == inPast)
        start <- arbitrary `suchThat` (\t -> (t > end) == invalid && (t < minTime state) == inPast)
        pure ((start, end), state)
  forAll gen $ \(interval, state) ->
    case fixInterval interval state of
      IntervalTrimmed environment' state'     -> not invalid && not inPast
                                                   && timeInterval environment' == interval
                                                   && state' == state {minTime = maximum [minTime state, fst interval]}
      IntervalError (InvalidInterval     _  ) -> invalid
      IntervalError (IntervalInPastError _ _) -> not invalid && inPast


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


checkAvailableMoney :: Bool -> Property
checkAvailableMoney isElement =
  let
     gen _ State{accounts} =
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


checkChoiceValue :: Bool -> Property
checkChoiceValue isElement =
  let
     gen _ State{choices} =
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


checkUseValue :: Bool -> Property
checkUseValue isElement =
  let
     gen _ State{boundValues} =
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
  let
    gen _ _ = (,,) <$> observationGen <*> valueGen <*> valueGen
  in
    checkValue gen $ \eval eval' _ _ (condition, thenValue, elseValue) ->
      eval (Cond condition thenValue elseValue) == (if eval' condition then eval thenValue else eval elseValue)


checkAndObs :: Property
checkAndObs =
  let
    gen _ _ = (,) <$> observationGen <*> observationGen
  in
    checkValue gen $ \_ eval _ _ (x, y) ->
      eval (AndObs x y) == (eval x && eval y)


checkOrObs :: Property
checkOrObs =
  let
    gen _ _ = (,) <$> observationGen <*> observationGen
  in
    checkValue gen $ \_ eval _ _ (x, y) ->
      eval (OrObs x y) == (eval x || eval y)


checkNotObs :: Property
checkNotObs =
  checkValue (const . const $ observationGen) $ \_ eval _ _ x ->
    eval (NotObs x) == not (eval x)


checkChoseSomething :: Bool -> Property
checkChoseSomething isElement =
  let
     gen _ State{choices} =
       if isElement && not (AM.null choices)
         then elements $ AM.keys choices
         else arbitrary
  in
    checkValue gen $ \_ eval _ State{choices} choice ->
      let
        x = ChoseSomething choice
      in
        choice `AM.member` choices == eval x


checkValueGE :: Property
checkValueGE =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval eval' _ _ (x, y) ->
      eval' (ValueGE x y) == (eval x >= eval y)


checkValueGT :: Property
checkValueGT =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval eval' _ _ (x, y) ->
      eval' (ValueGT x y) == (eval x > eval y)


checkValueLT :: Property
checkValueLT =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval eval' _ _ (x, y) ->
      eval' (ValueLT x y) == (eval x < eval y)


checkValueLE :: Property
checkValueLE =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval eval' _ _ (x, y) ->
      eval' (ValueLE x y) == (eval x <= eval y)


checkValueEQ :: Property
checkValueEQ =
  let
    gen _ _ = (,) <$> valueGen <*> valueGen
  in
    checkValue gen $ \eval eval' _ _ (x, y) ->
      eval' (ValueEQ x y) == (eval x == eval y)


checkTrueObs :: Assertion
checkTrueObs =
  assertBool "TrueObs is true."
    $ evalObservation undefined undefined TrueObs


checkFalseObs :: Assertion
checkFalseObs =
  assertBool "FalseObs is false."
    . not $ evalObservation undefined undefined FalseObs


checkApplyActionMismatch :: Property
checkApplyActionMismatch = property $ do
  let gen = do
        let
          inputs = [IDeposit undefined undefined undefined undefined, IChoice undefined undefined, INotify]
          actions = [Deposit undefined undefined undefined undefined, Choice undefined undefined, Notify undefined]
        x <- chooseInt (0, length inputs - 1)
        y <- suchThat (chooseInt (0, length actions - 1)) (/= x)
        pure (inputs !! x, actions !! y)
  forAll gen $ \(x, y) ->
    case applyAction undefined undefined x y of
      NotAppliedAction -> True
      _                -> False


choiceInBoundsIfNonempty :: [Bound] -> Gen ChosenNum
choiceInBoundsIfNonempty [] = arbitrary
choiceInBoundsIfNonempty bounds =
  do
    Bound lower upper <- elements bounds
    chooseInteger (lower, upper)


choiceNotInBounds :: [Bound] -> Gen ChosenNum
choiceNotInBounds [] = arbitrary
choiceNotInBounds bounds =
  let
    inBound chosenNum (Bound lower upper) = chosenNum >= lower && chosenNum <= upper
  in
    suchThat arbitrary $ \chosenNum -> not $ any (inBound chosenNum) bounds


checkIDeposit :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Property
checkIDeposit accountMatches partyMatches tokenMatches amountMatches = property $ do
  let gen = do
        environment <- arbitrary
        state <- arbitrary
        ((account, token), _) <- arbitraryFromAccounts $ accounts state
        accountMatches' <- maybe arbitrary pure accountMatches
        account' <- if accountMatches' then pure account else suchThat arbitrary (/= account)
        partyMatches' <- maybe arbitrary pure partyMatches
        party <- arbitrary
        party' <- if partyMatches' then pure party else suchThat arbitrary (/= party)
        tokenMatches' <- maybe arbitrary pure tokenMatches
        token' <- if tokenMatches' then pure token else suchThat arbitrary (/= token)
        amountMatches' <- maybe arbitrary pure amountMatches
        amount <- valueGen
        let amountEvaluated = evalValue environment state amount
        amount' <- if amountMatches' then pure amountEvaluated else suchThat arbitrary (/= amountEvaluated)
        pure (environment, state, account', party', token', amount', Deposit account party token amount, accountMatches' && partyMatches' && tokenMatches' && amountMatches')
  forAll gen $ \(environment, state, account, party, token, amount, action, match) ->
    let
      amount' = maybe amount (+ amount) . AM.lookup (account, token) $ accounts state
      newState = state {accounts = AM.insert (account, token) amount' $ accounts state}
    in
      case applyAction environment state (IDeposit account party token amount) action of
        NotAppliedAction                    -> not match
        AppliedAction ApplyNoWarning state' -> match && amount >  0 && newState == state'
        AppliedAction _              state' -> match && amount <= 0 && state    == state'


checkIChoice :: Maybe Bool -> Maybe Bool -> Property
checkIChoice choiceMatches choiceInBounds = property $ do
  let gen = do
        choiceMatches' <- maybe arbitrary pure choiceMatches
        choiceInBounds' <- maybe arbitrary pure choiceInBounds
        environment <- arbitrary
        state <- arbitrary
        choiceId <- arbitrary
        bounds <- if choiceInBounds' then suchThat arbitrary (not . null) else arbitrary
        choiceId' <- if choiceMatches' then pure choiceId else suchThat arbitrary (/= choiceId)
        choiceNum <- if choiceInBounds' then choiceInBoundsIfNonempty bounds else choiceNotInBounds bounds
        pure (environment, state, choiceId', choiceNum, Choice choiceId bounds, choiceMatches' && choiceInBounds')
  forAll gen $ \(environment, state, choiceId, choiceNum, action, match) ->
    let
      newState = state {choices = AM.insert choiceId choiceNum $ choices state}
    in
      case applyAction environment state (IChoice choiceId choiceNum) action of
        NotAppliedAction                    -> not match
        AppliedAction ApplyNoWarning state' -> match && newState == state'
        AppliedAction _              state' -> state == state'


checkINotify :: Property
checkINotify = property $ do
  let gen = do
        environment <- arbitrary
        state <- arbitrary
        x <- observationGen
        pure (environment, state, x)
  forAll gen $ \(environment, state, x) ->
    let
      result = evalObservation environment state x
    in
      case applyAction environment state INotify (Notify x) of
        AppliedAction ApplyNoWarning state' -> result && state == state'
        AppliedAction _ _                   -> False
        NotAppliedAction                    -> not result


assocMapEq :: Ord k => Ord v => AM.Map k v -> AM.Map k v -> Bool
assocMapEq = (==) `on` (sort . AM.toList)

assocMapInsert :: Eq k => k -> v -> AM.Map k v -> AM.Map k v
assocMapInsert k v =
  AM.fromList
    . ((k, v) :)
    . filter ((/= k) . fst)
    . AM.toList

assocMapAdd :: Ord k => Ord v => Num v => k -> v -> AM.Map k v -> AM.Map k v
assocMapAdd k v =
  AM.fromList
    . fmap (bimap head sum . unzip)
    . groupBy ((==) `on` fst)
    . sort
    . ((k, v) :)
    . AM.toList


checkRefundOne :: (Int -> Bool) -> Property
checkRefundOne f =
  property
    $ forAll (arbitraryAccounts `suchThat` (f . length . AM.toList)) $ \accounts' ->
      case (AM.null accounts', refundOne accounts') of
         (True, Nothing                          ) -> True
         (True, _                                ) -> False
         (_   , Nothing                          ) -> False
         (_   , Just ((party, money), accounts'')) -> case flattenMoney money of
                                                        [(token, amount)] -> accounts' `assocMapEq` assocMapInsert (party, token) amount accounts''
                                                        _                 -> False


checkMoneyInAccount :: Property
checkMoneyInAccount =
  property $ do
  let gen =
        do
          accounts' <- arbitraryAccounts
          (, accounts') <$> arbitraryFromAccounts accounts'
  forAll gen $ \(((account, token), _), accounts') ->
    fromMaybe 0 ((account, token) `AM.lookup` accounts') == moneyInAccount account token accounts'


checkUpdateMoneyInAccount :: Property
checkUpdateMoneyInAccount =
  property $ do
  let gen =
        do
          accounts' <- arbitraryAccounts
          (, accounts') <$> arbitraryFromAccounts accounts'
  forAll gen $ \(((account, token), amount), accounts') ->
    let
      newAccounts = AM.filter (> 0) $ assocMapInsert (account, token) amount accounts'
    in
      newAccounts `assocMapEq` updateMoneyInAccount account token amount accounts'


checkAddMoneyToAccount :: Property
checkAddMoneyToAccount =
  property $ do
  let gen =
        do
          accounts' <- arbitraryAccounts
          (, accounts') <$> arbitraryFromAccounts accounts'
  forAll gen $ \(((account, token), amount), accounts') ->
    let
      newAccounts = assocMapAdd (account, token) amount accounts'
      accounts'' = addMoneyToAccount account token amount accounts'
    in
      if amount > 0
        then newAccounts `assocMapEq` accounts''
        else accounts' `assocMapEq` accounts''


checkGiveMoney :: Property
checkGiveMoney =
  property $ do
  let gen =
        do
          accounts' <- arbitraryAccounts
          (, , accounts') <$> arbitraryFromAccounts accounts' <*> arbitrary
  forAll gen $ \(((account, token), amount), payee, accounts') ->
    let
      newAccounts =
        case payee of
          Party   _        -> accounts'
          Account account' -> assocMapAdd (account', token) amount accounts'
      (result, accounts'') = giveMoney account payee token amount accounts'
    in
      (if amount > 0 then newAccounts else accounts') `assocMapEq` accounts''
        && case result of
             ReduceWithPayment (Payment account'' payee'' money'') -> case flattenMoney money'' of
                                                                        [(token', amount')] -> account'' == account
                                                                                                 && payee == payee''
                                                                                                 && token == token'
                                                                                                 && amount == amount'
                                                                        []                  -> amount <= 0
                                                                        _                     -> False
             _                                                     -> False


flattenMoney :: Money -> [(Token, Integer)]
flattenMoney = fmap (\(s, n, a) -> (Token s n, a)) .  flattenValue


checkReduceContractStepClose :: Property
checkReduceContractStepClose =
  property $ do
  forAll ((,) <$> arbitrary <*> arbitrary) $ \(environment, state) ->
    let
      checkPayment (Payment payee (Party payee') money) state' Close =
        payee == payee'
          && case flattenMoney money of
               [(token, amount)] -> AM.lookup (payee, token) (accounts state) == Just amount
                                      && AM.lookup (payee, token) (accounts state') == Nothing
                                      && state == state' {accounts = assocMapInsert (payee, token) amount (accounts state')}
               _                 -> False
      checkPayment _ _ _ = False
    in
      case reduceContractStep environment state Close of
        NotReduced                                                           -> AM.null $ accounts state
        Reduced ReduceNoWarning (ReduceWithPayment payment) state' contract' -> checkPayment payment state' contract'
        _                                                                    -> False


-- FIXME: Turn this off when semantics are fixed!
_ALLOW_ZERO_PAYMENT :: Bool
_ALLOW_ZERO_PAYMENT = True


checkReduceContractStepPay :: Property
checkReduceContractStepPay =
  property $ do
  let gen = do
        environment <- arbitrary
        state <- arbitrary
        ((account, token), _) <- arbitraryFromAccounts $ accounts state
        (environment, state, account, , token, , ) <$> arbitrary <*> valueGen <*> contractGen
  forAll gen $ \(environment, state, account, payee, token, value, contract) ->
    let
      prior = fromMaybe 0 $ AM.lookup (account, token) (accounts state)
      request = evalValue environment state value
      debit = minimum [prior, request]
      positiveAmount = debit > 0
      fullAmount = request == debit
      posterior = prior - debit
      newState = state {accounts = (if posterior == 0 then AM.delete else flip assocMapInsert posterior) (account, token) (accounts state)}
      checkPayment (Payment account' (Party payee') money) state' =
        account' == account
          && Party payee' == payee
          && case flattenMoney money of
               [(token', amount)] -> token == token' && amount == debit && state' `stateEq` newState
               []                 -> _ALLOW_ZERO_PAYMENT && state' `stateEq` state
               _                  -> False
      checkPayment (Payment account' (Account payee') money) state' =
        let
          other = fromMaybe 0 $ AM.lookup (payee', token) (accounts newState)
          newState' = if other + debit > 0 then newState {accounts = assocMapInsert (payee', token) (other + debit) (accounts newState)} else newState
        in
          account' == account
            && Account payee' == payee
            && case flattenMoney money of
                 [(token', amount)] -> token' == token && amount == debit && state' `stateEq` newState'
                 []                 -> _ALLOW_ZERO_PAYMENT && state' `stateEq` state
                 _                  -> False
    in
      case reduceContractStep environment state (Pay account payee token value contract) of
        Reduced (ReduceNonPositivePay account' payee' token' request')    ReduceNoPayment state' contract'             -> not positiveAmount
                                                                                                                            && account' == account
                                                                                                                            && payee' == payee
                                                                                                                            && token' == token
                                                                                                                            && request' == request
                                                                                                                            && state' `stateEq` state
                                                                                                                            && contract' == contract
        Reduced ReduceNoWarning                                           (ReduceWithPayment payment) state' contract' -> positiveAmount
                                                                                                                            && fullAmount
                                                                                                                            && checkPayment payment state'
                                                                                                                            && contract' == contract
        Reduced (ReducePartialPay account' payee' token' debit' request') (ReduceWithPayment payment) state' contract' -> (positiveAmount || _ALLOW_ZERO_PAYMENT && debit' == 0)
                                                                                                                            && not fullAmount
                                                                                                                            && account' == account
                                                                                                                            && payee' == payee
                                                                                                                            && token' == token
                                                                                                                            && debit' == debit
                                                                                                                            && request' == request
                                                                                                                            && checkPayment payment state'
                                                                                                                            && contract' == contract
        _                                                                                                               -> False


checkReduceContractStepIf :: Property
checkReduceContractStepIf =
  property $ do
  forAll ((,,,,) <$> arbitrary <*> arbitrary <*> observationGen <*> contractGen <*> contractGen) $ \(environment, state, observation, thenContract, elseContract) ->
    let
      passed = evalObservation environment state observation
    in
      case reduceContractStep environment state (If observation thenContract elseContract) of
        Reduced ReduceNoWarning ReduceNoPayment state' contract' -> state == state' && (if passed then thenContract else elseContract) == contract'
        _                                                        -> False


assocMapSort :: Ord k => AM.Map k v -> AM.Map k v
assocMapSort = AM.fromList . sortBy (compare `on` fst) . AM.toList


canonicalState :: State -> State
canonicalState State{..} =
  State
    (assocMapSort accounts)
    (assocMapSort choices)
    (assocMapSort boundValues)
    minTime


stateEq :: State -> State -> Bool
stateEq = (==) `on` canonicalState


checkReduceContractStepWhen :: Property
checkReduceContractStepWhen =
  property $ do
  forAll ((,,,,) <$> arbitrary <*> arbitrary <*> listOf caseGen <*> arbitrary <*> contractGen) $ \(environment, state, cases, timeout, contract) ->
    let
      before = snd (timeInterval environment) < timeout
      afterwards = fst (timeInterval environment) >= timeout
    in
      case reduceContractStep environment state (When cases timeout contract) of
        NotReduced                                               -> before
        Reduced ReduceNoWarning ReduceNoPayment state' contract' -> afterwards && contract == contract' && state == state'
        _                                                        -> not before && not afterwards


caseGen :: Gen (Case Contract)
caseGen = sized $ (simpleIntegerGen >>=) . caseRelGenSized


checkReduceContractStepLet :: Property
checkReduceContractStepLet =
  property $ do
  forAll ((,,,,) <$> arbitrary <*> arbitrary <*> arbitrary <*> valueGen <*> contractGen) $ \(environment, state, valueId, value, contract) ->
    let
      x = evalValue environment state value
      shadow = valueId `AM.member` boundValues state
    in
      case (shadow, reduceContractStep environment state (Let valueId value contract)) of
        (False, Reduced ReduceNoWarning                      ReduceNoPayment state' contract') -> contract == contract'
                                                                                                    && state {boundValues = assocMapInsert valueId x (boundValues state)} `stateEq` state'
        (True , Reduced (ReduceShadowing valueId' value' x') ReduceNoPayment state' contract') -> contract == contract'
                                                                                                    && state {boundValues = assocMapInsert valueId x (boundValues state)} `stateEq` state'
                                                                                                    && valueId == valueId'
                                                                                                    && AM.lookup valueId (boundValues state) == Just value'
                                                                                                    && x == x'
        _                                                                                      -> False


checkReduceContractStepAssert :: Property
checkReduceContractStepAssert =
  property $ do
  forAll ((,,,) <$> arbitrary <*> arbitrary <*> observationGen <*> contractGen) $ \(environment, state, observation, contract) ->
    let
      passed = evalObservation environment state observation
    in
      case reduceContractStep environment state (Assert observation contract) of
        Reduced ReduceNoWarning       ReduceNoPayment state' contract' -> passed     && state == state' && contract == contract'
        Reduced ReduceAssertionFailed ReduceNoPayment state' contract' -> not passed && state == state' && contract == contract'
        _                                                              -> False
