-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tests of implementation functions for `Language.Marlowe.Core.V1.Semantics.computeTransaction` of Marlowe semantics.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}


module Spec.Marlowe.Semantics.Functions
  ( -- * Testing
    tests
  ) where


import Data.Maybe (fromMaybe, isNothing)
import Language.Marlowe.Core.V1.Semantics
  ( ApplyAction(AppliedAction, NotAppliedAction)
  , ApplyResult(Applied, ApplyNoMatchError)
  , ApplyWarning(ApplyNoWarning, ApplyNonPositiveDeposit)
  , Payment(Payment)
  , ReduceEffect(ReduceNoPayment, ReduceWithPayment)
  , ReduceResult(ContractQuiescent, RRAmbiguousTimeIntervalError)
  , ReduceStepResult(NotReduced, Reduced)
  , ReduceWarning(ReduceAssertionFailed, ReduceNoWarning, ReduceNonPositivePay, ReducePartialPay, ReduceShadowing)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , addMoneyToAccount
  , applyAction
  , applyCases
  , applyInput
  , evalObservation
  , evalValue
  , fixInterval
  , getContinuation
  , giveMoney
  , isClose
  , moneyInAccount
  , notClose
  , playTrace
  , reduceContractStep
  , reduceContractUntilQuiescent
  , refundOne
  , updateMoneyInAccount
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Choice, Deposit, Notify)
  , Case(Case, MerkleizedCase)
  , Contract(Assert, Close, If, Let, Pay, When)
  , Environment(..)
  , Input(MerkleizedInput, NormalInput)
  , InputContent(IChoice, IDeposit, INotify)
  , IntervalError(IntervalInPastError, InvalidInterval)
  , IntervalResult(IntervalError, IntervalTrimmed)
  , Observation(..)
  , Payee(Account, Party)
  , State(..)
  , Value(..)
  )
import Language.Marlowe.FindInputs (getAllInputs)
import Plutus.Script.Utils.Scripts (dataHash)
import Plutus.V2.Ledger.Api (POSIXTime(..), toBuiltinData)
import Spec.Marlowe.Semantics.Arbitrary
  ( SemiArbitrary(semiArbitrary)
  , arbitraryAssocMap
  , arbitraryContractWeighted
  , arbitraryValidInputs
  , arbitraryValidStep
  , choiceInBoundsIfNonempty
  , choiceNotInBounds
  , whenContractWeights
  )
import Spec.Marlowe.Semantics.AssocMap (assocMapAdd, assocMapEq, assocMapInsert)
import Spec.Marlowe.Semantics.Orphans ()
import Spec.Marlowe.Semantics.Util (stateEq, truncatedDivide)
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, testCase)
import Test.Tasty.QuickCheck
  (Arbitrary(..), Gen, Property, Testable(property), chooseInt, elements, forAll, forAllShrink, suchThat, testProperty)

import qualified PlutusTx.AssocMap as AM (delete, empty, filter, fromList, insert, keys, lookup, member, null, toList)


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Support Functions"
    [
      testGroup "fixInterval"
      [
        testProperty "Invalid interval" $ checkFixInterval True  False
      , testProperty "Interval in past" $ checkFixInterval False True
      , testProperty "Interval trimmed" $ checkFixInterval False False
      , testProperty "Invalid interval in past" $ checkFixInterval True True
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
        testProperty "No accounts"         $ checkRefundOne (== 0)
      , testProperty "One account"         $ checkRefundOne (== 1)
      , testProperty "Multiple accounts"   $ checkRefundOne (>= 2)
      , testProperty "Nonpositive account"   checkRefundOneNotPositive
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
    , testProperty "reduceContractUntilQuiescent" checkReduceContractUntilQuiescent
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
    , testProperty "getContinuation" checkGetContinuation
    , testProperty "applyCases" checkApplyCases
    , testProperty "applyInput" checkApplyInput
--  , testProperty "applyAllInputs" checkApplyAllInputs
    , testCase "isClose" checkIsClose
    , testCase "notClose" checkNotClose
    , testProperty "computeTransaction (via playTrace)" checkComputeTransaction
    , testProperty "playTrace" checkPlayTrace
    ]


-- | Test properties with shrinkage.
forAll' :: Arbitrary a
        => Show a
        => Testable prop
        => Gen a        -- ^ The test-case generator.
        -> (a -> prop)  -- ^ The test.
        -> Property     -- ^ The result of multiple applications of the test.
forAll' = flip forAllShrink shrink


-- | Test the `Language.Marlowe.Core.V1.Semantics.fixInterval` function by
--   generating arbitrary valid and invalid intervals and checking that
--   results or errors re reported correctly.
checkFixInterval :: Bool      -- ^ Whether the validity interval should be invalid.
                 -> Bool      -- ^ Whether the validity interval should be in the past.
                 -> Property  -- ^ The test.
checkFixInterval invalid inPast =
  property $ do
  let gen = do
        state <- arbitrary
        (start, end) <-
          case (invalid, inPast) of
            (True, True) -> do
                              start' <- arbitrary `suchThat` (< minTime state)
                              end'   <- arbitrary `suchThat` (< start')
                              pure (start', end')
            _            -> do
                              end'   <- arbitrary `suchThat` (\t -> (t < minTime state) == inPast)
                              start' <- arbitrary `suchThat` (\t -> (t > end') == invalid && (t < minTime state) == inPast)
                              pure (start', end')
        pure ((start, end), state)
  forAll' gen $ \(interval, state) ->
    case fixInterval interval state of
      IntervalTrimmed environment' state'     -> not invalid && not inPast
                                                   && timeInterval environment' == interval
                                                   && state' == state {minTime = maximum [minTime state, fst interval]}
      IntervalError (InvalidInterval     _  ) -> invalid
      IntervalError (IntervalInPastError _ _) -> not invalid && inPast


-- | Test the evaluation of values and observations using a generation function and a test function.
checkValue :: Arbitrary a
           => Show a
           => (Environment -> State -> Gen a)                                                                 -- ^ The test-case generator.
           -> ((Value Observation -> Integer) -> (Observation -> Bool) -> Environment -> State -> a -> Bool)  -- ^ Perform the test.
           -> Property                                                                                        -- ^ The test.
checkValue gen f =
  property $ do
  let gen' = do
        environment <- arbitrary
        state <- arbitrary
        x <- gen environment state
        pure (environment, state, x)
  forAll' gen' $ \(environment, state, x) ->
    f (evalValue environment state) (evalObservation environment state) environment state x


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.AvailableMoney` according to
--   whether the account is indeed present in the state.
checkAvailableMoney :: Bool      -- ^ Whether the account is present in the state.
                    -> Property  -- ^ The test.
checkAvailableMoney isElement =
  let
     gen _ State{accounts} =
       if isElement && not (AM.null accounts)
         then elements $ AM.keys accounts
         else arbitrary
  in
    checkValue gen $ \eval _ _ State{accounts} (account, token) ->
      let
        x = AvailableMoney account token
      in
        case (account, token) `AM.lookup` accounts of
          Nothing -> eval x == 0
          Just x' -> eval x == x'


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.Constant` by evaluating
--   the value.
checkConstant :: Property
checkConstant =
  checkValue (const . const $ arbitrary) $ \eval _ _ _ x ->
    eval (Constant x) == x


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.NegValue` by evaluating
--   the value.
checkNegValue :: Property
checkNegValue =
  checkValue (const . const $ arbitrary) $ \eval _ _ _ x ->
    eval (NegValue x) == - eval x


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.AddValue` by evaluating
--   the values.
checkAddValue :: Property
checkAddValue =
  checkValue (const $ const arbitrary) $ \eval _ _ _ (x, y) ->
    eval (AddValue x y) == eval x + eval y


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.SubValue` by evaluating
--   the values.
checkSubValue :: Property
checkSubValue =
  checkValue (const $ const arbitrary) $ \eval _ _ _ (x, y) ->
    eval (SubValue x y) == eval x - eval y


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.MulValue` by evaluating
--   the values.
checkMulValue :: Property
checkMulValue =
  checkValue (const $ const arbitrary) $ \eval _ _ _ (x, y) ->
    eval (MulValue x y) == eval x * eval y


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.DivValue` for 0/0.
checkDivValueNumeratorDenominatorZero :: Assertion
checkDivValueNumeratorDenominatorZero =
  assertBool "DivValue 0 0 = 0"
    $ evalValue undefined undefined (DivValue (Constant 0) (Constant 0)) == 0


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.DivValue` for zero numerator.
checkDivValueNumeratorZero :: Property
checkDivValueNumeratorZero =
  checkValue (const . const $ arbitrary) $ \eval _ _ _ x ->
    eval (DivValue (Constant 0) x) == 0


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.DivValue` for zero denominator.
checkDivValueDenominatorZero :: Property
checkDivValueDenominatorZero =
  checkValue (const . const $ arbitrary) $ \eval _ _ _ x ->
    eval (DivValue x (Constant 0)) == 0


-- | Test ` Language.Marlowe.Core.V1.Semantics.Types.DivValue . Language.Marlowe.Core.V1.Semantics.Types.MulValue`.
checkDivValueMultiple :: Property
checkDivValueMultiple =
  checkValue (const $ const arbitrary) $ \eval _ _ _ (x, n) ->
    eval (DivValue (MulValue x n) n) == eval x || eval n == 0


-- | Test rounding of `Language.Marlowe.Core.V1.Semantics.Types.DivValue` by evaluating
--   the values and comparing to `truncatedDivide`.
checkDivValueRounding :: Property
checkDivValueRounding =
  checkValue (const $ const arbitrary) $ \eval _ _ _ (x, y) ->
    eval (DivValue x y) == eval x `truncatedDivide` eval y || eval y == 0


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ChoiceValue` depending
--   upon whether the choice has indeed been made.
checkChoiceValue :: Bool      -- ^ Whether the choice should be taken from the state.
                 -> Property  -- ^ The test.
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


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.TimeIntervalStart` by
--   evaluating the value in the context of the environment.
checkTimeIntervalStart :: Property
checkTimeIntervalStart =
  checkValue (const . const $ pure ()) $ \eval _ Environment{timeInterval} _ () ->
    POSIXTime (eval TimeIntervalStart) == fst timeInterval


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.TimeIntervalEnd` by
--   evaluating the value in the context of the environment.
checkTimeIntervalEnd :: Property
checkTimeIntervalEnd =
  checkValue (const . const $ pure ()) $ \eval _ Environment{timeInterval} _ () ->
    POSIXTime (eval TimeIntervalEnd) == snd timeInterval


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.UseValue` by
--   evaluating the value in the context of the state, for both its
--   presence and absence.
checkUseValue :: Bool      -- ^ Whether the bound value is present in the state.
              -> Property  -- ^ The test.
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


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.Cond` by comparison
--   to Haskell.
checkCond :: Property
checkCond =
  checkValue (const $ const arbitrary) $ \eval eval' _ _ (condition, thenValue, elseValue) ->
    eval (Cond condition thenValue elseValue) == (if eval' condition then eval thenValue else eval elseValue)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.AndObs` by comparison
--   to Haskell.
checkAndObs :: Property
checkAndObs =
  checkValue (const $ const arbitrary) $ \_ eval _ _ (x, y) ->
    eval (AndObs x y) == (eval x && eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.OrObs` by comparison
--   to Haskell.
checkOrObs :: Property
checkOrObs =
  checkValue (const $ const arbitrary) $ \_ eval _ _ (x, y) ->
    eval (OrObs x y) == (eval x || eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.NotObs` by comparison
--   to Haskell.
checkNotObs :: Property
checkNotObs =
  checkValue (const . const $ arbitrary) $ \_ eval _ _ x ->
    eval (NotObs x) == not (eval x)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ChoseSomething` according
--   to whether the choice has been made in the state.
checkChoseSomething :: Bool      -- ^ Whether the choice is present in the state.
                    -> Property  -- ^ The test.
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


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ValueGE`
--   by comparison to Haskell semantics.
checkValueGE :: Property
checkValueGE =
  checkValue (const $ const arbitrary) $ \eval eval' _ _ (x, y) ->
    eval' (ValueGE x y) == (eval x >= eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ValueGT`
--   by comparison to Haskell semantics.
checkValueGT :: Property
checkValueGT =
  checkValue (const $ const arbitrary) $ \eval eval' _ _ (x, y) ->
    eval' (ValueGT x y) == (eval x > eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ValueLT`
--   by comparison to Haskell semantics.
checkValueLT :: Property
checkValueLT =
  checkValue (const $ const arbitrary) $ \eval eval' _ _ (x, y) ->
    eval' (ValueLT x y) == (eval x < eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ValueLE`
--   by comparison to Haskell semantics.
checkValueLE :: Property
checkValueLE =
  checkValue (const $ const arbitrary) $ \eval eval' _ _ (x, y) ->
    eval' (ValueLE x y) == (eval x <= eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.ValueEQ`
--   by comparison to Haskell semantics.
checkValueEQ :: Property
checkValueEQ =
  checkValue (const $ const arbitrary) $ \eval eval' _ _ (x, y) ->
    eval' (ValueEQ x y) == (eval x == eval y)


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.TrueObs`
--   by comparison to Haskell semantics.
checkTrueObs :: Assertion
checkTrueObs =
  assertBool "TrueObs is true."
    $ evalObservation undefined undefined TrueObs


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.FalseObs`
--   by comparison to Haskell semantics.
checkFalseObs :: Assertion
checkFalseObs =
  assertBool "FalseObs is false."
    . not $ evalObservation undefined undefined FalseObs


-- | Test detection of actions not matching cases.
checkApplyActionMismatch :: Property
checkApplyActionMismatch = property $ do
  let gen = do
        let
          inputs = [IDeposit undefined undefined undefined undefined, IChoice undefined undefined, INotify]
          actions = [Deposit undefined undefined undefined undefined, Choice undefined undefined, Notify undefined]
        x <- chooseInt (0, length inputs - 1)
        y <- suchThat (chooseInt (0, length actions - 1)) (/= x)
        pure (inputs !! x, actions !! y)
  forAll' gen $ \(x, y) ->
    case applyAction undefined undefined x y of
      NotAppliedAction -> True
      _                -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.IDeposit` for a `Language.Marlowe.Core.V1.Semantics.Types.Deposit`.
checkIDeposit :: Maybe Bool  -- ^ Whether the accounts match.
              -> Maybe Bool  -- ^ Whether the parties match.
              -> Maybe Bool  -- ^ Whether the tokens match.
              -> Maybe Bool  -- ^ Whether the amounts match.
              -> Property    -- ^ The test.
checkIDeposit accountMatches partyMatches tokenMatches amountMatches = property $ do
  let perhapsMatches item shouldMatch =
        do
          matches' <- maybe arbitrary pure shouldMatch
          (matches', ) <$> if matches' then pure item else arbitrary `suchThat` (/= item)
  let gen = do
        context <- arbitrary
        environment <- semiArbitrary context
        state <- semiArbitrary context
        (account, token) <- semiArbitrary context
        party <- arbitrary
        amount <- arbitrary
        let amountEvaluated = evalValue environment state amount
        (accountMatches', account') <- perhapsMatches account         accountMatches
        (partyMatches'  , party'  ) <- perhapsMatches party           partyMatches
        (tokenMatches'  , token'  ) <- perhapsMatches token           tokenMatches
        (amountMatches' , amount' ) <- perhapsMatches amountEvaluated amountMatches
        pure (environment, state, account', party', token', amount', Deposit account party token amount, accountMatches' && partyMatches' && tokenMatches' && amountMatches')
  forAll' gen $ \(environment, state, account, party, token, amount, action, match) ->
    let
      amount' = maybe amount (+ amount) . AM.lookup (account, token) $ accounts state
      newState = state {accounts = AM.insert (account, token) amount' $ accounts state}
    in
      case applyAction environment state (IDeposit account party token amount) action of
        NotAppliedAction                                                               -> not match
        AppliedAction ApplyNoWarning state'                                            -> match && amount >  0 && newState == state'
        AppliedAction (ApplyNonPositiveDeposit party' account' token' amount'') state' -> match && amount <= 0 && state    == state'
                                                                                            && party == party' && account == account'
                                                                                            && token == token' && amount  == amount''


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.IChoice` for a `Language.Marlowe.Core.V1.Semantics.Types.Choice`.
checkIChoice :: Maybe Bool  -- ^ Whether the choice identifiers match.
             -> Maybe Bool  -- ^ Whether the choice is in bounds.
             -> Property    -- ^ The test.
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
  forAll' gen $ \(environment, state, choiceId, choiceNum, action, match) ->
    let
      newState = state {choices = AM.insert choiceId choiceNum $ choices state}
    in
      case applyAction environment state (IChoice choiceId choiceNum) action of
        NotAppliedAction                    -> not match
        AppliedAction ApplyNoWarning state' -> match && newState == state'
        AppliedAction _              state' -> state == state'


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.INotify` for a `Language.Marlowe.Core.V1.Semantics.Types.Notify`.
checkINotify :: Property
checkINotify = property $ do
  let gen = do
        environment <- arbitrary
        state <- arbitrary
        x <- arbitrary
        pure (environment, state, x)
  forAll' gen $ \(environment, state, x) ->
    let
      result = evalObservation environment state x
    in
      case applyAction environment state INotify (Notify x) of
        AppliedAction ApplyNoWarning state' -> result && state == state'
        AppliedAction _ _                   -> False
        NotAppliedAction                    -> not result


-- | Test `Language.Marlowe.Core.V1.Semantics.refundOne`, given a condition on the number of accounts.
checkRefundOne :: (Int -> Bool)  -- ^ A filtering condition for number of accounts in the test case.
               -> Property       -- ^ The test.
checkRefundOne f =
  property
    $ forAll' (arbitrary `suchThat` (f . length . AM.toList)) $ \accounts' ->
      case (AM.null accounts', refundOne accounts') of
         (True, Nothing                                  ) -> True
         (True, _                                        ) -> False
         (_   , Nothing                                  ) -> False
         (_   , Just ((party, token, amount), accounts'')) -> accounts' `assocMapEq` assocMapInsert (party, token) amount accounts''


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.refundOne` for a non-positive amount.
checkRefundOneNotPositive :: Property
checkRefundOneNotPositive =
  property
    $ forAll' (arbitraryAssocMap arbitrary arbitrary) $ \accountsMixed ->
      let
        positive = AM.fromList . filter ((> 0) . snd) . AM.toList
        accountsPositive = positive accountsMixed
      in
        case (refundOne accountsMixed, refundOne accountsPositive) of
          (Just (payment, accountsMixed'), Just (payment', accountsPositive')) -> payment == payment' && positive accountsMixed' == accountsPositive'
          (Nothing                       , Nothing                           ) -> True
          _                                                                    -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.moneyInAccount`.
checkMoneyInAccount :: Property
checkMoneyInAccount =
  property $ do
  let gen =
        do
          context <- arbitrary
          (,) <$> semiArbitrary context <*> semiArbitrary context
  forAll' gen $ \((account, token), accounts') ->
    fromMaybe 0 ((account, token) `AM.lookup` accounts') == moneyInAccount account token accounts'


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.updateMoneyInAccount`.
checkUpdateMoneyInAccount :: Property
checkUpdateMoneyInAccount =
  property $ do
  let gen =
        do
          context <- arbitrary
          (,,) <$> semiArbitrary context <*> semiArbitrary context <*> semiArbitrary context
  forAll' gen $ \((account, token), amount, accounts') ->
    let
      newAccounts = AM.filter (> 0) $ assocMapInsert (account, token) amount accounts'
    in
      newAccounts `assocMapEq` updateMoneyInAccount account token amount accounts'


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.addMoneyToAccount`.
checkAddMoneyToAccount :: Property
checkAddMoneyToAccount =
  property $ do
  let gen =
        do
          context <- arbitrary
          (,,) <$> semiArbitrary context <*> semiArbitrary context <*> semiArbitrary context
  forAll' gen $ \((account, token), amount, accounts') ->
    let
      newAccounts = assocMapAdd (account, token) amount accounts'
      accounts'' = addMoneyToAccount account token amount accounts'
    in
      if amount > 0
        then newAccounts `assocMapEq` accounts''
        else accounts' `assocMapEq` accounts''


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.giveMoney`.
checkGiveMoney :: Property
checkGiveMoney =
  property $ do
  let gen =
        do
          context <- arbitrary
          (,,,) <$> semiArbitrary context <*> semiArbitrary context <*> semiArbitrary context <*> semiArbitrary context
  forAll' gen $ \((account, token), amount, payee, accounts') ->
    let
      newAccounts =
        case payee of
          Party   _        -> accounts'
          Account account' -> assocMapAdd (account', token) amount accounts'
      (result, accounts'') = giveMoney account payee token amount accounts'
    in
      (if amount > 0 then newAccounts else accounts') `assocMapEq` accounts''
        && case result of
             ReduceWithPayment (Payment account'' payee'' token' amount') -> if amount' /= 0
                                                                               then account'' == account
                                                                                      && payee == payee''
                                                                                      && token == token'
                                                                                      && amount == amount'
                                                                               else amount <= 0
             _                                                     -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.reduceContractStep` for a `Language.Marlowe.Core.V1.Semantics.Types.Close`.
checkReduceContractStepClose :: Property
checkReduceContractStepClose =
  property $ do
  forAll' arbitrary $ \(environment, state) ->
    let
      checkPayment (Payment payee (Party payee') token amount) state' Close =
        payee == payee'
          && amount /= 0
          && AM.lookup (payee, token) (accounts state) == Just amount
          && isNothing (AM.lookup (payee, token) (accounts state'))
          && state == state' {accounts = assocMapInsert (payee, token) amount (accounts state')}
      checkPayment _ _ _ = False
    in
      case reduceContractStep environment state Close of
        NotReduced                                                           -> AM.null $ accounts state
        Reduced ReduceNoWarning (ReduceWithPayment payment) state' contract' -> checkPayment payment state' contract'
        _                                                                    -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.reduceContractStep` for a `Language.Marlowe.Core.V1.Semantics.Types.Pay`.
checkReduceContractStepPay :: Property
checkReduceContractStepPay =
  property $ do
  let gen = do
        context <- arbitrary
        (,,,,,,)
          <$> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
          <*> semiArbitrary context
  forAll' gen $ \(environment, state, account, payee, token, value, contract) ->
    let
      prior = fromMaybe 0 $ AM.lookup (account, token) (accounts state)
      request = evalValue environment state value
      debit = minimum [prior, request]
      positiveAmount = debit > 0
      fullAmount = request == debit
      posterior = prior - debit
      newState = state {accounts = (if posterior == 0 then AM.delete else flip assocMapInsert posterior) (account, token) (accounts state)}
      checkPayment (Payment account' (Party payee') token' amount) state' =
        account' == account
          && Party payee' == payee
          && if amount /= 0
               then token == token' && amount == debit && state' `stateEq` newState
               else state' `stateEq` state
      checkPayment (Payment account' (Account payee') token' amount) state' =
        let
          other = fromMaybe 0 $ AM.lookup (payee', token) (accounts newState)
          newState' = if other + debit > 0 then newState {accounts = assocMapInsert (payee', token) (other + debit) (accounts newState)} else newState
        in
          account' == account
            && Account payee' == payee
            && if amount /= 0
                 then token' == token && amount == debit && state' `stateEq` newState'
                 else state' `stateEq` state
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
        Reduced (ReducePartialPay account' payee' token' debit' request') (ReduceWithPayment payment) state' contract' -> (positiveAmount || debit' == 0)
                                                                                                                            && not fullAmount
                                                                                                                            && account' == account
                                                                                                                            && payee' == payee
                                                                                                                            && token' == token
                                                                                                                            && debit' == debit
                                                                                                                            && request' == request
                                                                                                                            && checkPayment payment state'
                                                                                                                            && contract' == contract
        _                                                                                                               -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.reduceContractStep` for an `Language.Marlowe.Core.V1.Semantics.Types.If`.
checkReduceContractStepIf :: Property
checkReduceContractStepIf =
  property $ do
  forAll' arbitrary $ \(environment, state, observation, thenContract, elseContract) ->
    let
      passed = evalObservation environment state observation
    in
      case reduceContractStep environment state (If observation thenContract elseContract) of
        Reduced ReduceNoWarning ReduceNoPayment state' contract' -> state == state' && (if passed then thenContract else elseContract) == contract'
        _                                                        -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.Types.reduceContractStep` for a `Language.Marlowe.Core.V1.Semantics.Types.When`.
checkReduceContractStepWhen :: Property
checkReduceContractStepWhen =
  property $ do
  forAll' arbitrary $ \(environment, state, cases, timeout, contract) ->
    let
      before = snd (timeInterval environment) < timeout
      afterwards = fst (timeInterval environment) >= timeout
    in
      case reduceContractStep environment state (When cases timeout contract) of
        NotReduced                                               -> before
        Reduced ReduceNoWarning ReduceNoPayment state' contract' -> afterwards && contract == contract' && state == state'
        _                                                        -> not before && not afterwards


-- | Test `Language.Marlowe.Core.V1.Semantics.reduceContractStep` for a `Language.Marlowe.Core.V1.Semantics.Types.Let`.
checkReduceContractStepLet :: Property
checkReduceContractStepLet =
  property $ do
  forAll' arbitrary $ \(environment, state, valueId, value, contract) ->
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


-- | Test `Language.Marlowe.Core.V1.Semantics.reduceContractStep` for an `Language.Marlowe.Core.V1.Semantics.Types.Assert`.
checkReduceContractStepAssert :: Property
checkReduceContractStepAssert =
  property $ do
  forAll' arbitrary $ \(environment, state, observation, contract) ->
    let
      passed = evalObservation environment state observation
    in
      case reduceContractStep environment state (Assert observation contract) of
        Reduced ReduceNoWarning       ReduceNoPayment state' contract' -> passed     && state == state' && contract == contract'
        Reduced ReduceAssertionFailed ReduceNoPayment state' contract' -> not passed && state == state' && contract == contract'
        _                                                              -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.reduceContractStepUntilQuiescent`.
checkReduceContractUntilQuiescent :: Property
checkReduceContractUntilQuiescent =
  property $ do
    forAll' arbitrary $ \(environment, state, contract) ->
      case reduceContractUntilQuiescent environment state contract of
        ContractQuiescent _ _ _ _ Close              -> True
        ContractQuiescent _ _ _ _ (When _ timeout _) -> snd (timeInterval environment) < timeout
        ContractQuiescent{}                          -> False
        RRAmbiguousTimeIntervalError                 -> True


-- | Test `Language.Marlowe.Core.V1.Semantics.getContinuation`.
checkGetContinuation :: Property
checkGetContinuation =
  property $ do
    let gen =
         do
           sameContract <- arbitrary
           correctHash <- arbitrary
           contract <- arbitrary
           contract' <- if sameContract then pure contract else arbitrary
           contract'' <- arbitrary
           let contractHash  = dataHash $ toBuiltinData contract
               contractHash' = dataHash . toBuiltinData $ if correctHash then contract' else contract''
           content <- arbitrary
           action <- arbitrary
           input <- elements [NormalInput content, MerkleizedInput content contractHash' contract']
           case' <- elements [Case action contract, MerkleizedCase action contractHash]
           pure (input, case', contract)
    forAll' gen $ \(input, case', contract) ->
      case (input, case', getContinuation input case') of
        (NormalInput{}                   , Case{}                        , contract') -> Just contract == contract'
        (MerkleizedInput _ contractHash _, MerkleizedCase _ contractHash', Just _   ) -> contractHash == contractHash'
        (MerkleizedInput _ contractHash _, MerkleizedCase _ contractHash', Nothing  ) -> contractHash /= contractHash'
        (NormalInput{}                   , MerkleizedCase{}              , Nothing  ) -> True
        (MerkleizedInput{}               , Case{}                        , Nothing  ) -> True
        _                                                                             -> False


-- | Test `Language.Marlowe.Core.V1.Semantics.applyCases`.
checkApplyCases :: Property
checkApplyCases =
  property $ do
    let gen =
          do
            context <- arbitrary
            state <- semiArbitrary context
            contract <- arbitraryContractWeighted [whenContractWeights] context
            input <- arbitraryValidStep state contract
            pure (state, contract, input)
    forAll' gen $ \(state, contract, TransactionInput times inputs) ->
      case (contract, inputs) of
        (When cases _ _, [input]) -> case applyCases (Environment times) state input cases of
                                       Applied{} -> True
                                       _         -> False
        _                         -> True


-- | Test `Language.Marlowe.Core.V1.Semantics.applyInput`.
checkApplyInput :: Property
checkApplyInput =
  property $ do
    forAll arbitrary $ \(environment, state, input, contract) ->
      case (contract, applyInput environment state input contract) of
        (When cases _ _, result           ) -> result == applyCases environment state input cases
        (_             , ApplyNoMatchError) -> True
        e                                   -> error $ show e


-- | Test that `Language.Marlowe.Core.V1.Semantics.isClose` reports correctly.
checkIsClose :: Assertion
checkIsClose =
  do
    assertBool "isClose Close = True"  $ isClose Close
    assertBool "isClose Pay = False"   . not . isClose $ Pay undefined undefined undefined undefined undefined
    assertBool "isClose If = False"    . not . isClose $ If undefined undefined undefined
    assertBool "isClose When = False"  . not . isClose $ When undefined undefined undefined
    assertBool "isClose Let = False"   . not . isClose $ Let undefined undefined undefined
    assertBool "isClose Asset = False" . not . isClose $ Assert undefined undefined


-- | Test that `Language.Marlowe.Core.V1.Semantics.notClose` reports correctly.
checkNotClose :: Assertion
checkNotClose =
  do
    assertBool "notClose Close = False" . not $ notClose Close
    assertBool "notClose Pay = True"   . notClose $ Pay undefined undefined undefined undefined undefined
    assertBool "notClose If = True"    . notClose $ If undefined undefined undefined
    assertBool "notClose When = True"  . notClose $ When undefined undefined undefined
    assertBool "notClose Let = True"   . notClose $ Let undefined undefined undefined
    assertBool "notClose Asset = True" . notClose $ Assert undefined undefined


-- | Test `Language.Marlowe.Core.V1.Semantics.computeTransaction` against static analysis cases that should succeed.
checkComputeTransaction :: Property
checkComputeTransaction =
  monadicIO $ do
    contract <- pick $ arbitrary `suchThat` (/= Close)
    let
      play (time, inputs) =
        case playTrace time contract inputs of
          TransactionOutput{} -> True
          e                   -> error $ show (time, contract, inputs, e)
    either (error . ("`getAllInputs` failed with " <>) . show) (all play)
      <$> run (getAllInputs contract)


-- | Test `Language.Marlowe.Core.V1.Semantics.playTrace` somewhat tautologically ☹️.
checkPlayTrace :: Property
checkPlayTrace =
  property $ do
    let gen =
          do
            start <- arbitrary
            contract <- arbitrary
            -- NOTE: This is somewhat tautological in that `arbitraryValidInputs` uses `computeTransaction` as a filter.
            inputs <- arbitraryValidInputs (State AM.empty AM.empty AM.empty start) contract
            pure (start, contract, inputs)
    forAll gen $ \(start, contract, inputs) ->
      case playTrace start contract inputs of
        TransactionOutput{} -> True
        e                   -> error $ show e
