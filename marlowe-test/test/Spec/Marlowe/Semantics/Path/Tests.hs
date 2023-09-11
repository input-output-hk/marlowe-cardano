{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Marlowe.Semantics.Path.Tests (
  tests,
) where

import Language.Marlowe.Core.V1.Semantics (
  ApplyAllResult (..),
  ReduceResult (..),
  applyAllInputs,
  evalObservation,
  evalValue,
  reduceContractUntilQuiescent,
 )
import Language.Marlowe.Core.V1.Semantics.Types
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Path
import Test.QuickCheck
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

runPath :: Environment -> State -> ContractPath -> Maybe Contract
runPath env state = \case
  PathStop c -> case reduceContractUntilQuiescent env state c of
    ContractQuiescent _ _ _ _ contract -> Just contract
    RRAmbiguousTimeIntervalError -> Nothing
  PathPay account (Account account') token (MemoExpr _ value) c -> do
    state' <- creditAccount account' token value <$> debitAccount account token value state
    runPath env state' c
  PathPay account _ token (MemoExpr _ value) c -> do
    state' <- debitAccount account token value state
    runPath env state' c
  PathIfL _ l _ -> runPath env state l
  PathIfR _ _ r -> runPath env state r
  PathWhenCases WhenPath{..} -> uncurry (runPathAction env state) pathCase
  PathWhenTimeout _ _ c -> runPath env state c
  PathLet valueId (MemoExpr _ value) c -> runPath env (setValue valueId value state) c
  PathAssert _ c -> runPath env state c

creditAccount :: AccountId -> Token -> Integer -> State -> State
creditAccount account token value state@State{..} = case AM.lookup (account, token) accounts of
  Nothing -> state{accounts = AM.insert (account, token) value accounts}
  Just value' -> state{accounts = AM.insert (account, token) (value' + value) accounts}

debitAccount :: AccountId -> Token -> Integer -> State -> Maybe State
debitAccount account token value state@State{..} = do
  value' <- AM.lookup (account, token) accounts
  case compare value value' of
    LT -> Just state{accounts = AM.insert (account, token) (value' - value) accounts}
    EQ -> Just state{accounts = AM.delete (account, token) accounts}
    GT -> Nothing

runPathAction :: Environment -> State -> PathAction -> ContractPath -> Maybe Contract
runPathAction env state = \case
  PathDeposit account _ token (MemoExpr _ value) ->
    runPath env (creditAccount account token value state)
  PathChoice choiceId bounds ->
    runPath env (makeChoice choiceId (getChosen bounds) state)
  PathNotify _ -> runPath env state

makeChoice :: ChoiceId -> ChosenNum -> State -> State
makeChoice choiceId chosen state = state{choices = AM.insert choiceId chosen $ choices state}

setValue :: ValueId -> Integer -> State -> State
setValue valueId value state = state{boundValues = AM.insert valueId value $ boundValues state}

getChosen :: PathBounds -> ChosenNum
getChosen = \case
  PathBoundsNext _ next -> getChosen next
  PathBoundsHere low dChoice _ _ -> low + dChoice

exprToValue :: Expr Integer -> Value Observation
exprToValue = \case
  ConstantExpr i -> Constant i
  AvailableMoneyExpr account token -> AvailableMoney account token
  ChoiceValueExpr choiceId -> ChoiceValue choiceId
  UseValueExpr valueId -> UseValue valueId
  NegExpr (MemoExpr expr _) -> NegValue $ exprToValue expr
  AddExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> AddValue (exprToValue expr1) (exprToValue expr2)
  SubExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> SubValue (exprToValue expr1) (exprToValue expr2)
  MulExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> MulValue (exprToValue expr1) (exprToValue expr2)
  DivExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> DivValue (exprToValue expr1) (exprToValue expr2)
  CondExpr (MemoExpr expr1 _) (MemoExpr expr2 _) (MemoExpr expr3 _) ->
    Cond (exprToObs expr1) (exprToValue expr2) (exprToValue expr3)
  TimeIntervalStartExpr -> TimeIntervalStart
  TimeIntervalEndExpr -> TimeIntervalEnd

exprToObs :: Expr Bool -> Observation
exprToObs = \case
  AndExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> AndObs (exprToObs expr1) (exprToObs expr2)
  OrExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> OrObs (exprToObs expr1) (exprToObs expr2)
  NotExpr (MemoExpr expr _) -> NotObs (exprToObs expr)
  ChoseSomethingExpr choiceId -> ChoseSomething choiceId
  ValueGEExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> ValueGE (exprToValue expr1) (exprToValue expr2)
  ValueGTExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> ValueGT (exprToValue expr1) (exprToValue expr2)
  ValueLEExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> ValueLE (exprToValue expr1) (exprToValue expr2)
  ValueLTExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> ValueLT (exprToValue expr1) (exprToValue expr2)
  ValueEQExpr (MemoExpr expr1 _) (MemoExpr expr2 _) -> ValueEQ (exprToValue expr1) (exprToValue expr2)
  TrueExpr -> TrueObs
  FalseExpr -> FalseObs

tests :: TestTree
tests =
  testGroup
    "Path"
    [ exprTests
    ]

exprTests :: TestTree
exprTests =
  testGroup
    "MemoExpr"
    [ exprRelationTest "valueLT" "<" ">=" genValueLT (<)
    , exprRelationTest "valueLE" "<=" ">" genValueLE (<=)
    , exprRelationTest "valueGT" ">" "<=" genValueGT (>)
    , exprRelationTest "valueGE" ">=" "<" genValueGE (>=)
    , exprRelationTest "valueEQ" "==" "/=" genValueEQ (==)
    , exprRelationTest "valueNE" "/=" "==" genValueNE (/=)
    , testProperty "genTrueObs satisfies (== True)" checkTrueObs
    , testProperty "genFalseObs satisfies (== False)" checkFalseObs
    , evalExprBound "genValueLT" "evalValue" genValueLT exprToValue evalValue
    , evalExprBound "genValueLE" "evalValue" genValueLE exprToValue evalValue
    , evalExprBound "genValueGT" "evalValue" genValueGT exprToValue evalValue
    , evalExprBound "genValueGE" "evalValue" genValueGE exprToValue evalValue
    , evalExprBound "genValueEQ" "evalValue" genValueEQ exprToValue evalValue
    , evalExprBound "genValueNE" "evalValue" genValueNE exprToValue evalValue
    , evalExpr "genValue" "evalValue" genValue exprToValue evalValue
    , evalExpr "genObs" "evalObservation" genObs exprToObs evalObservation
    , evalExpr "genTrueObs" "evalObservation" genTrueObs exprToObs evalObservation
    , evalExpr "genFalseObs" "evalObservation" genFalseObs exprToObs evalObservation
    , testProperty "contract path applies" checkContractPath
    ]

checkContractPath :: Environment -> State -> Property
checkContractPath env state = forAll (genWhenPath env state) \whenPath ->
  let path = PathWhenCases whenPath
      contract = getContract path
      inputs = NormalInput <$> getInputs [] path
      expected = applyAllInputs env state contract inputs
      actual = runPath env state path
   in counterexample (show inputs) $
        counterexample (show contract) $
          counterexample (show $ reduceContractUntilQuiescent env state contract) $
            counterexample (show expected) case expected of
              ApplyAllSuccess _ _ _ _ c -> actual === Just c
              _ -> property False

checkTrueObs :: Environment -> State -> Property
checkTrueObs env state = forAll (genTrueObs env state) \(MemoExpr _ b) -> b === True

checkFalseObs :: Environment -> State -> Property
checkFalseObs env state = forAll (genFalseObs env state) \(MemoExpr _ b) -> b === False

exprRelationTest
  :: (Arbitrary a, Show a)
  => TestName
  -> TestName
  -> TestName
  -> (a -> Environment -> State -> Gen (MemoExpr a))
  -> (a -> a -> Bool)
  -> TestTree
exprRelationTest genName relName invRelName gen rel =
  testProperty ("`" <> genName <> " a` satisfies `(" <> relName <> " a)`") \a env state ->
    forAll (gen a env state) \(MemoExpr _ b) ->
      let res = rel b a
          interpret True = relName
          interpret False = invRelName
       in counterexample (show b <> " " <> interpret res <> " " <> show a) res

evalExprBound
  :: (Arbitrary a, Show a, Eq a)
  => TestName
  -> TestName
  -> (a -> Environment -> State -> Gen (MemoExpr a))
  -> (Expr a -> expr)
  -> (Environment -> State -> expr -> a)
  -> TestTree
evalExprBound genName evalName gen toExpr eval =
  testProperty ("`" <> genName <> " a`: value matches " <> evalName) \a env state ->
    forAll (gen a env state) \(MemoExpr expr b) ->
      b === (eval env state $ toExpr expr)

evalExpr
  :: (Show a, Eq a)
  => TestName
  -> TestName
  -> (Environment -> State -> Gen (MemoExpr a))
  -> (Expr a -> expr)
  -> (Environment -> State -> expr -> a)
  -> TestTree
evalExpr genName evalName gen toExpr eval =
  testProperty ("`" <> genName <> "`: value matches " <> evalName) \env state ->
    forAll (gen env state) \(MemoExpr expr b) ->
      b === (eval env state $ toExpr expr)
