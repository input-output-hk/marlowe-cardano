{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Marlowe.Semantics.Path (
  ContractPath (..),
  Expr (..),
  MemoExpr (..),
  PathAction (..),
  PathBounds (..),
  WhenPath (..),
  genContractPath,
  genFalseObs,
  genObs,
  genTrueObs,
  genValue,
  genValueGE,
  genValueGT,
  genValueLE,
  genValueLT,
  genWhenPath,
  getContract,
  getInput,
  getInputs,
  tests,
) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Bifunctor (bimap, first)
import Data.Functor (($>), (<&>))
import Data.Maybe (catMaybes)
import Language.Marlowe.Core.V1.Semantics (
  ApplyAllResult (..),
  ReduceResult (..),
  applyAllInputs,
  evalObservation,
  evalValue,
  reduceContractUntilQuiescent,
 )
import Language.Marlowe.Core.V1.Semantics.Types
import Plutus.V2.Ledger.Api (POSIXTime (..))
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Eq
import Spec.Marlowe.Semantics.Arbitrary (arbitraryPositiveInteger)
import Test.QuickCheck
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

data ContractPath
  = PathStop Contract
  | PathPay AccountId Payee Token (MemoExpr Integer) ContractPath
  | PathIfL (MemoExpr Bool) ContractPath Contract
  | PathIfR (MemoExpr Bool) Contract ContractPath
  | PathWhenCases WhenPath
  | PathWhenTimeout [(Action, Contract)] POSIXTime ContractPath
  | PathLet ValueId (MemoExpr Integer) ContractPath
  | PathAssert Observation ContractPath
  deriving (Show, Eq)

getContract :: ContractPath -> Contract
getContract = \case
  PathStop contract -> contract
  PathPay accountId payee token (MemoExpr expr _) c -> Pay accountId payee token (exprToValue expr) $ getContract c
  PathIfL (MemoExpr expr _) l r -> If (exprToObs expr) (getContract l) r
  PathIfR (MemoExpr expr _) l r -> If (exprToObs expr) l (getContract r)
  PathWhenCases WhenPath{..} ->
    When
      ( concat
          [ uncurry Case . first pathActionToAction <$> casesBefore
          , [uncurry Case $ bimap pathActionToAction getContract pathCase]
          , uncurry Case <$> casesAfter
          ]
      )
      timeout
      continuation
  PathWhenTimeout cases timeout c -> When (uncurry Case <$> cases) timeout $ getContract c
  PathLet valueId (MemoExpr expr _) c -> Let valueId (exprToValue expr) $ getContract c
  PathAssert obs c -> Assert obs $ getContract c

getInputs :: [InputContent] -> ContractPath -> [InputContent]
getInputs acc = \case
  PathStop _ -> reverse acc
  PathPay _ _ _ _ c -> getInputs acc c
  PathIfL _ l _ -> getInputs acc l
  PathIfR _ _ r -> getInputs acc r
  PathWhenCases WhenPath{..} -> getInputs (getInput (fst pathCase) : acc) (snd pathCase)
  PathWhenTimeout _ _ c -> getInputs acc c
  PathLet _ _ c -> getInputs acc c
  PathAssert _ c -> getInputs acc c

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

pathActionToAction :: PathAction -> Action
pathActionToAction = \case
  PathDeposit account party token (MemoExpr expr _) -> Deposit account party token (exprToValue expr)
  PathChoice choiceId bounds -> Choice choiceId (pathBoundsToBounds [] bounds)
  PathNotify (MemoExpr expr _) -> Notify $ exprToObs expr

getInput :: PathAction -> InputContent
getInput = \case
  PathDeposit account party token (MemoExpr _ value) -> IDeposit account party token value
  PathChoice choiceId bounds -> IChoice choiceId $ getChosen bounds
  PathNotify _ -> INotify

pathBoundsToBounds :: [Bound] -> PathBounds -> [Bound]
pathBoundsToBounds acc = \case
  PathBoundsNext bound next -> pathBoundsToBounds (bound : acc) next
  PathBoundsHere lo dChoice dHi bounds -> Bound lo (lo + dChoice + dHi) : (reverse acc <> bounds)

genContractPath :: Environment -> State -> Gen ContractPath
genContractPath env state = sized \size ->
  if size <= 0
    then PathStop . translateContract env <$> arbitrary
    else
      frequency
        [ (size, genPathSegment env state)
        , (1, PathStop . translateContract env <$> arbitrary)
        ]

translateContract :: Environment -> Contract -> Contract
translateContract env = \case
  Close -> Close
  Pay accountId payee token value c -> Pay accountId payee token value $ translateContract env c
  If obs l r -> If obs (translateContract env l) (translateContract env r)
  When cases timeout c -> When (translateCase env <$> cases) (abs timeout + 1 + (snd $ timeInterval env)) $ translateContract env c
  Let valueId value c -> Let valueId value $ translateContract env c
  Assert obs c -> Assert obs $ translateContract env c

translateCase :: Environment -> Case Contract -> Case Contract
translateCase env = \case
  Case action c -> Case action $ translateContract env c
  c -> c

genPathSegment :: Environment -> State -> Gen ContractPath
genPathSegment env state =
  oneof
    [ genPathPay env state
    , genIf env state
    , genWhen env state
    , genLet env state
    , genAssert env state
    ]

genPathPay :: Environment -> State -> Gen ContractPath
genPathPay env state@State{..} = case AM.toList accounts of
  [] -> genPathSegment env state
  accountList -> sized \size -> do
    ((account, token), maxValue) <- elements accountList
    payee <- arbitrary
    value <- chooseInteger (1, maxValue)
    expr <- genValueEQ value env state
    let accounts' =
          if value == maxValue
            then AM.delete (account, token) accounts
            else AM.insert (account, token) (maxValue - value) accounts
        accounts'' = case payee of
          Account account' -> case AM.lookup (account', token) accounts' of
            Nothing -> AM.insert (account', token) value accounts'
            Just value' -> AM.insert (account', token) (value' + value) accounts'
          _ -> accounts'
        state' = state{accounts = accounts''}
    PathPay account payee token expr <$> resize (pred size) (genContractPath env state')

genIf :: Environment -> State -> Gen ContractPath
genIf env state =
  oneof
    [ PathIfL <$> genTrueObs env state <*> genContractPath env state <*> arbitrary
    , PathIfR <$> genFalseObs env state <*> arbitrary <*> genContractPath env state
    ]

genWhen :: Environment -> State -> Gen ContractPath
genWhen env state =
  frequency
    [ (4, PathWhenCases <$> genWhenPath env state)
    , (1, genWhenContinuation env state)
    ]

genLet :: Environment -> State -> Gen ContractPath
genLet env state = sized \size -> do
  valueId <- arbitrary
  expr@(MemoExpr _ value) <- genValueGT 0 env state
  let state' = state{boundValues = AM.insert valueId value (boundValues state)}
  PathLet valueId expr <$> resize (pred size) (genContractPath env state')

genAssert :: Environment -> State -> Gen ContractPath
genAssert env state = sized \size ->
  PathAssert <$> arbitrary <*> resize (pred size) (genContractPath env state)

data WhenPath = WhenPath
  { casesBefore :: [(PathAction, Contract)]
  , pathCase :: (PathAction, ContractPath)
  , casesAfter :: [(Action, Contract)]
  , timeout :: POSIXTime
  , continuation :: Contract
  }
  deriving (Show, Eq)

genWhenContinuation :: Environment -> State -> Gen ContractPath
genWhenContinuation env state = sized \size -> do
  let maxCases = floor $ sqrt @Double $ fromIntegral size
  numCases <- chooseInt (0, maxCases)
  let totalSubContracts = numCases + 1
  let subContractSize = size `div` totalSubContracts
  cases <- vectorOf numCases $ (,) <$> arbitrary <*> resize subContractSize arbitrary
  timeout <- (+ fst (timeInterval env)) . POSIXTime . negate . abs <$> arbitrary
  continuation <- resize subContractSize $ genContractPath env state
  pure $ PathWhenTimeout cases timeout continuation

genWhenPath :: Environment -> State -> Gen WhenPath
genWhenPath env state = sized \size -> do
  let maxCases = floor $ sqrt @Double $ fromIntegral size
  numCasesBefore <- chooseInt (0, maxCases)
  numCasesAfter <- chooseInt (0, maxCases - numCasesBefore)
  let totalSubContracts = numCasesBefore + numCasesAfter + 2 -- path case and continuation
  let subContractSize = size `div` totalSubContracts
  (pathAction, state') <- genPathAction (genTrueObs env state) env state
  casesBefore <-
    vectorOf numCasesBefore $
      (,)
        <$> genPathActionNoMatch pathAction env state
        <*> resize subContractSize arbitrary
  casesAfter <- vectorOf numCasesAfter $ (,) <$> arbitrary <*> resize subContractSize arbitrary
  pathCase <- (pathAction,) <$> resize subContractSize (genContractPath env state')
  timeout <- (snd (timeInterval env) +) . POSIXTime <$> arbitraryPositiveInteger
  continuation <- resize subContractSize arbitrary
  pure WhenPath{..}

data PathAction
  = PathDeposit AccountId Party Token (MemoExpr Integer)
  | PathChoice ChoiceId PathBounds
  | PathNotify (MemoExpr Bool)
  deriving (Show, Eq)

genPathAction :: Gen (MemoExpr Bool) -> Environment -> State -> Gen (PathAction, State)
genPathAction genObs' env state =
  frequency
    [ (4, genPathDeposit env state)
    , (4, genPathChoice state)
    , (1, (,state) . PathNotify <$> genObs')
    ]

genPathActionNoMatch :: PathAction -> Environment -> State -> Gen PathAction
genPathActionNoMatch action env state =
  (fst <$> genPathAction (genObs env state) env state) `suchThat` noOverlap action

noOverlap :: PathAction -> PathAction -> Bool
noOverlap l r = case (l, r) of
  (PathDeposit account1 party1 token1 (MemoExpr _ value1), PathDeposit account2 party2 token2 (MemoExpr _ value2)) ->
    account1 /= account2 || party1 /= party2 || token1 /= token2 || value1 /= value2
  (PathChoice choice1 bounds1, PathChoice choice2 bounds2) ->
    choice1 /= choice2 || (bounds1 `noOverlapBounds` bounds2)
  (PathNotify (MemoExpr _ obs1), PathNotify (MemoExpr _ obs2)) -> obs1 /= obs2
  _ -> True

noOverlapBounds :: PathBounds -> PathBounds -> Bool
noOverlapBounds bounds1 bounds2 = (chosen1 `notInBounds` bounds2) && (chosen2 `notInBounds` bounds1)
  where
    chosen1 = getChosen bounds1
    chosen2 = getChosen bounds2

getChosen :: PathBounds -> ChosenNum
getChosen = \case
  PathBoundsNext _ next -> getChosen next
  PathBoundsHere low dChoice _ _ -> low + dChoice

notInBounds :: ChosenNum -> PathBounds -> Bool
notInBounds chosenNum = \case
  PathBoundsNext bound next -> notInBound chosenNum bound && notInBounds chosenNum next
  PathBoundsHere low dChoice dHi next ->
    notInBound chosenNum (Bound low (low + dChoice + dHi))
      && all (notInBound chosenNum) next

notInBound :: ChosenNum -> Bound -> Bool
notInBound chosenNum (Bound lo hi) = chosenNum < min lo hi || chosenNum > max lo hi

genPathDeposit :: Environment -> State -> Gen (PathAction, State)
genPathDeposit env state = do
  account <- arbitrary
  party <- arbitrary
  token <- arbitrary
  expr@(MemoExpr _ value) <- genValueGT 0 env state
  let oldAccounts = accounts state
  let key = (account, token)
  let newAccounts = case AM.lookup key oldAccounts of
        Nothing -> AM.insert key value oldAccounts
        Just value' -> AM.insert key (value' + value) oldAccounts
  pure
    ( PathDeposit account party token expr
    , state{accounts = newAccounts}
    )

genPathChoice :: State -> Gen (PathAction, State)
genPathChoice state = do
  choiceId <- arbitrary
  bounds <- arbitrary
  pure
    ( PathChoice choiceId bounds
    , state{choices = AM.insert choiceId (getChosen bounds) $ choices state}
    )

data PathBounds
  = PathBoundsNext Bound PathBounds
  | PathBoundsHere Integer Integer Integer [Bound]
  deriving (Show, Eq)

instance Arbitrary PathBounds where
  arbitrary = sized \size ->
    if size == 0
      then PathBoundsHere <$> arbitrary <*> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> pure []
      else
        oneof
          [ PathBoundsHere <$> arbitrary <*> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> arbitrary
          , PathBoundsNext <$> arbitrary <*> resize (pred size) arbitrary
          ]

-- | An expression with a memoized evaluated value.
data MemoExpr a = MemoExpr (Expr a) a
  deriving (Show, Eq)

-- | An expression of some type. Unifies Observation and Value.
data Expr a where
  -- Integer expressions
  ConstantExpr :: Integer -> Expr Integer
  AvailableMoneyExpr :: AccountId -> Token -> Expr Integer
  ChoiceValueExpr :: ChoiceId -> Expr Integer
  UseValueExpr :: ValueId -> Expr Integer
  NegExpr :: MemoExpr Integer -> Expr Integer
  AddExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Integer
  SubExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Integer
  MulExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Integer
  DivExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Integer
  CondExpr :: MemoExpr Bool -> MemoExpr Integer -> MemoExpr Integer -> Expr Integer
  TimeIntervalStartExpr :: Expr Integer
  TimeIntervalEndExpr :: Expr Integer
  -- Boolean expressions
  AndExpr :: MemoExpr Bool -> MemoExpr Bool -> Expr Bool
  OrExpr :: MemoExpr Bool -> MemoExpr Bool -> Expr Bool
  NotExpr :: MemoExpr Bool -> Expr Bool
  ChoseSomethingExpr :: ChoiceId -> Expr Bool
  ValueGEExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Bool
  ValueGTExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Bool
  ValueLEExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Bool
  ValueLTExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Bool
  ValueEQExpr :: MemoExpr Integer -> MemoExpr Integer -> Expr Bool
  TrueExpr :: Expr Bool
  FalseExpr :: Expr Bool

deriving instance Show (Expr a)
deriving instance Eq (Expr a)

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

-- | Generate a value.
genValue :: Environment -> State -> Gen (MemoExpr Integer)
genValue env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNeg env state
        , resize (size `quot` 2) $ genAdd env state
        , resize (size `quot` 2) $ genSub env state
        , resize (size `quot` 2) $ genMul env state
        , resize (size `quot` 2) $ genDiv env state
        , resize (size `quot` 3) $ genCond env state
        ]
  where
    genLeaf =
      oneof
        [ genConstant
        , genAvailableMoney state
        , genChoiceValue state
        , genUseValue state
        , genTimeIntervalStart env
        , genTimeIntervalEnd env
        ]

-- | Generate a value larger than a given number.
genValueGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueGT bound env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNegGT bound env state
        , resize (size `quot` 2) $ genAddGT bound env state
        , resize (size `quot` 2) $ genSubGT bound env state
        , resize (size `quot` 2) $ genMulGT bound env state
        , resize (size `quot` 2) $ genDivGT bound env state
        , resize (size `quot` 3) $ genCondGT bound env state
        ]
  where
    genLeaf =
      oneof $
        catMaybes
          [ Just $ genConstantGT bound
          , genAvailableMoneyGT bound state
          , genChoiceValueGT bound state
          , genUseValueGT bound state
          , genTimeIntervalStartGT bound env
          , genTimeIntervalEndGT bound env
          ]

-- | Generate a value greater than or equal to a given number.
genValueGE :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueGE bound env state =
  frequency
    [ (9, genValueGT bound env state)
    , (1, genValueEQ bound env state)
    ]

-- | Generate a value less than a given number.
genValueLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueLT bound env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNegLT bound env state
        , resize (size `quot` 2) $ genAddLT bound env state
        , resize (size `quot` 2) $ genSubLT bound env state
        , resize (size `quot` 2) $ genMulLT bound env state
        , resize (size `quot` 2) $ genDivLT bound env state
        , resize (size `quot` 3) $ genCondLT bound env state
        ]
  where
    genLeaf =
      oneof $
        catMaybes
          [ Just $ genConstantLT bound
          , genAvailableMoneyLT bound state
          , genChoiceValueLT bound state
          , genUseValueLT bound state
          , genTimeIntervalStartLT bound env
          , genTimeIntervalEndLT bound env
          ]

-- | Generate a value less than or equal to a given number.
genValueLE :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueLE bound env state =
  frequency
    [ (9, genValueLT bound env state)
    , (1, genValueEQ bound env state)
    ]

-- | Generate a value equal to a given number
genValueEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueEQ bound env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNegEQ bound env state
        , resize (size `quot` 2) $ genAddEQ bound env state
        , resize (size `quot` 2) $ genSubEQ bound env state
        , resize (size `quot` 2) $ genMulEQ bound env state
        , resize (size `quot` 2) $ genDivEQ bound env state
        , resize (size `quot` 3) $ genCondEQ bound env state
        ]
  where
    genLeaf =
      oneof $
        catMaybes
          [ Just $ genConstantEQ bound
          , genAvailableMoneyEQ bound state
          , genChoiceValueEQ bound state
          , genUseValueEQ bound state
          , genTimeIntervalStartEQ bound env
          , genTimeIntervalEndEQ bound env
          ]

-- | Generate a value not equal to a given number
genValueNE :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueNE bound env state = oneof [genValueLT bound env state, genValueGT bound env state]

genCond :: Environment -> State -> Gen (MemoExpr Integer)
genCond env state = do
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <-
    (,,) <$> genObs env state <*> genValue env state <*> genValue env state
  pure $ MemoExpr (CondExpr cExpr aExpr bExpr) if c then a else b

genAdd :: Environment -> State -> Gen (MemoExpr Integer)
genAdd env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (AddExpr expr1 expr2) $ a + b

genSub :: Environment -> State -> Gen (MemoExpr Integer)
genSub env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (SubExpr expr1 expr2) $ a - b

genMul :: Environment -> State -> Gen (MemoExpr Integer)
genMul env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (MulExpr expr1 expr2) $ a * b

genDiv :: Environment -> State -> Gen (MemoExpr Integer)
genDiv env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b

genNeg :: Environment -> State -> Gen (MemoExpr Integer)
genNeg env state = genValue env state <&> \expr@(MemoExpr _ i) -> MemoExpr (NegExpr expr) (negate i)

genConstant :: Gen (MemoExpr Integer)
genConstant = uncurry MemoExpr . (ConstantExpr &&& id) <$> arbitrary

genAvailableMoney :: State -> Gen (MemoExpr Integer)
genAvailableMoney State{..} =
  oneof $
    (flip MemoExpr 0 . uncurry AvailableMoneyExpr <$> arbitraryNotIn accounts)
      : fmap pure (uncurry MemoExpr . first (uncurry AvailableMoneyExpr) <$> AM.toList accounts)

genChoiceValue :: State -> Gen (MemoExpr Integer)
genChoiceValue State{..} =
  oneof $
    (flip MemoExpr 0 . ChoiceValueExpr <$> arbitraryNotIn choices)
      : fmap pure (uncurry MemoExpr . first ChoiceValueExpr <$> AM.toList choices)

genUseValue :: State -> Gen (MemoExpr Integer)
genUseValue State{..} =
  oneof $
    (flip MemoExpr 0 . UseValueExpr <$> arbitraryNotIn boundValues)
      : fmap pure (uncurry MemoExpr . first UseValueExpr <$> AM.toList boundValues)

genTimeIntervalStart :: Environment -> Gen (MemoExpr Integer)
genTimeIntervalStart (Environment (POSIXTime start, _)) = pure $ MemoExpr TimeIntervalStartExpr start

genTimeIntervalEnd :: Environment -> Gen (MemoExpr Integer)
genTimeIntervalEnd (Environment (_, POSIXTime end)) = pure $ MemoExpr TimeIntervalEndExpr end

genCondGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genCondGT bound env state = do
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <-
    oneof
      [ (,,) <$> genTrueObs env state <*> genValueGT bound env state <*> genValue env state
      , (,,) <$> genFalseObs env state <*> genValue env state <*> genValueGT bound env state
      ]
  pure $ MemoExpr (CondExpr cExpr aExpr bExpr) if c then a else b

genAddGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genAddGT bound env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueGT (bound - a) env state
  pure $ MemoExpr (AddExpr expr1 expr2) $ a + b

genSubGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genSubGT bound env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueLT (a - bound) env state
  pure $ MemoExpr (SubExpr expr1 expr2) $ a - b

genMulGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genMulGT bound env state = case compare bound 0 of
  GT -> do
    expr1@(MemoExpr _ a) <- genValue env state `suchThat` nonzero
    expr2@(MemoExpr _ b) <-
      if a < 0
        then case compare (abs a) bound of
          GT -> genValueLT 0 env state
          EQ -> genValueLT (-1) env state
          LT -> genValueLT (bound `quot` a) env state `suchThat` nonzero
        else genValueGT (bound `quot` a) env state `suchThat` nonzero
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
  EQ ->
    oneof
      [ do
          expr1@(MemoExpr _ a) <- genValueGT 0 env state
          expr2@(MemoExpr _ b) <- genValueGT 0 env state
          pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
      , do
          expr1@(MemoExpr _ a) <- genValueLT 0 env state
          expr2@(MemoExpr _ b) <- genValueLT 0 env state
          pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
      ]
  LT -> do
    expr1@(MemoExpr _ a) <- genValue env state
    expr2@(MemoExpr _ b) <- case compare a 0 of
      GT ->
        if a < abs bound
          then genValueGT (bound `quot` a) env state
          else genValueGE 0 env state
      EQ -> genValue env state
      LT ->
        if a > bound
          then genValueLT (bound `quot` a) env state
          else genValueLT 0 env state
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b

genDivGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genDivGT bound env state = case compare bound 0 of
  GT ->
    oneof
      [ do
          expr1@(MemoExpr _ a) <- genValueGT bound env state
          let smallEnough b = (a `quot` b) > bound
          b <- elements $ takeWhile smallEnough [1 ..]
          pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
      ]
  EQ ->
    oneof
      [ do
          expr1@(MemoExpr _ a) <- genValueGT 0 env state
          b <- chooseInteger (1, a)
          pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
      , do
          expr1@(MemoExpr _ a) <- genValueLT 0 env state
          b <- chooseInteger (-1, a)
          pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
      ]
  LT ->
    oneof
      [ do
          expr1@(MemoExpr _ a) <- genValueGE 0 env state
          expr2@(MemoExpr _ b) <- genValueGE 0 env state
          pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
      , do
          expr1@(MemoExpr _ a) <- genValueLE 0 env state
          expr2@(MemoExpr _ b) <- genValueGT (a `quot` bound) env state
          pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
      , do
          expr1@(MemoExpr _ a) <- genValueLE 0 env state
          expr2@(MemoExpr _ b) <- genValueLE 0 env state
          pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
      ]

genNegGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genNegGT bound env state =
  genValueLT (negate bound) env state <&> \expr@(MemoExpr _ i) ->
    MemoExpr (NegExpr expr) (negate i)

genConstantGT :: Integer -> Gen (MemoExpr Integer)
genConstantGT = fmap (uncurry MemoExpr . (ConstantExpr &&& id) . (+ 1)) . genIntegerGE

genAvailableMoneyGT :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genAvailableMoneyGT bound State{..} = case filteredAccounts of
  []
    | bound < 0 -> Just $ flip MemoExpr 0 . uncurry AvailableMoneyExpr <$> arbitraryNotIn accounts
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first (uncurry AvailableMoneyExpr) <$> filteredAccounts
  where
    filteredAccounts = AM.toList $ AM.filter (> bound) accounts

genChoiceValueGT :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genChoiceValueGT bound State{..} = case filteredChoices of
  []
    | bound < 0 -> Just $ flip MemoExpr 0 . ChoiceValueExpr <$> arbitraryNotIn choices
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first ChoiceValueExpr <$> filteredChoices
  where
    filteredChoices = AM.toList $ AM.filter (> bound) choices

genUseValueGT :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genUseValueGT bound State{..} = case filteredBoundValues of
  []
    | bound < 0 -> Just $ flip MemoExpr 0 . UseValueExpr <$> arbitraryNotIn boundValues
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first UseValueExpr <$> filteredBoundValues
  where
    filteredBoundValues = AM.toList $ AM.filter (> bound) boundValues

genTimeIntervalStartGT :: Integer -> Environment -> Maybe (Gen (MemoExpr Integer))
genTimeIntervalStartGT bound (Environment (POSIXTime start, _))
  | start > bound = Just $ pure $ MemoExpr TimeIntervalStartExpr start
  | otherwise = Nothing

genTimeIntervalEndGT :: Integer -> Environment -> Maybe (Gen (MemoExpr Integer))
genTimeIntervalEndGT bound (Environment (_, POSIXTime end))
  | end > bound = Just $ pure $ MemoExpr TimeIntervalEndExpr end
  | otherwise = Nothing

genCondLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genCondLT bound env state = do
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <-
    oneof
      [ (,,) <$> genTrueObs env state <*> genValueLT bound env state <*> genValue env state
      , (,,) <$> genFalseObs env state <*> genValue env state <*> genValueLT bound env state
      ]
  pure $ MemoExpr (CondExpr cExpr aExpr bExpr) if c then a else b

genAddLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genAddLT bound env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueLT (bound - a) env state
  pure $ MemoExpr (AddExpr expr1 expr2) $ a + b

genSubLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genSubLT bound env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueGT (a - bound) env state
  pure $ MemoExpr (SubExpr expr1 expr2) $ a - b

genMulLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genMulLT bound env state = case compare bound 0 of
  GT -> do
    expr1@(MemoExpr _ a) <- genValue env state
    expr2@(MemoExpr _ b) <- case compare a 0 of
      GT ->
        if a < bound
          then genValueLT (bound `quot` a) env state
          else genValueLE 0 env state
      EQ -> genValue env state
      LT ->
        if abs a < bound
          then genValueGT (bound `quot` a) env state
          else genValueGT 0 env state
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
  EQ ->
    oneof
      [ do
          expr1@(MemoExpr _ a) <- genValueGT 0 env state
          expr2@(MemoExpr _ b) <- genValueLT 0 env state
          pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
      , do
          expr1@(MemoExpr _ a) <- genValueLT 0 env state
          expr2@(MemoExpr _ b) <- genValueGT 0 env state
          pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
      ]
  LT -> do
    expr1@(MemoExpr _ a) <- genValue env state `suchThat` nonzero
    expr2@(MemoExpr _ b) <-
      if a > 0
        then case compare a (abs bound) of
          GT -> genValueLT 0 env state
          EQ -> genValueLT (-1) env state
          LT -> genValueLT (bound `quot` a) env state `suchThat` nonzero
        else genValueGT (bound `quot` a) env state `suchThat` nonzero
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b

nonzero :: MemoExpr Integer -> Bool
nonzero (MemoExpr _ 0) = False
nonzero _ = True

genDivLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genDivLT bound env state = case compare bound 0 of
  GT ->
    oneof
      -- a >= 0 && b <= 0 && c > 0 => a / b < c
      [ do
          expr1@(MemoExpr _ a) <- genValueGE 0 env state
          expr2@(MemoExpr _ b) <- genValueLE 0 env state
          pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
      , -- a >= 0 && b > a / c && c > 0 => a / b < c
        do
          expr1@(MemoExpr _ a) <- genValueGE 0 env state
          expr2@(MemoExpr _ b) <- genValueGT (a `quot` bound) env state
          pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
      , -- a <= 0 && b >= 0 && c > 0 => a / b < c
        do
          expr1@(MemoExpr _ a) <- genValueLE 0 env state
          expr2@(MemoExpr _ b) <- genValueGE 0 env state
          pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
      ]
  EQ ->
    oneof
      -- a > 0 && b < 0 => a / b < 0
      [ do
          expr1@(MemoExpr _ a) <- genValueGT 0 env state
          b <- chooseInteger (-1, negate a)
          pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
      , -- a < 0 && b > 0 => a / b < 0
        do
          expr1@(MemoExpr _ a) <- genValueLT 0 env state
          b <- chooseInteger (1, negate a)
          pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
      ]
  LT ->
    oneof
      [ do
          expr1@(MemoExpr _ a) <- genValueLT bound env state
          let smallEnough b = (a `quot` b) < bound
          b <- elements $ takeWhile smallEnough [1 ..]
          pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
      ]

genNegLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genNegLT bound env state =
  genValueGT (negate bound) env state <&> \expr@(MemoExpr _ i) ->
    MemoExpr (NegExpr expr) (negate i)

genConstantLT :: Integer -> Gen (MemoExpr Integer)
genConstantLT = fmap (uncurry MemoExpr . (ConstantExpr &&& id) . pred) . genIntegerLE

genAvailableMoneyLT :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genAvailableMoneyLT bound State{..} = case filteredAccounts of
  []
    | bound > 0 -> Just $ flip MemoExpr 0 . uncurry AvailableMoneyExpr <$> arbitraryNotIn accounts
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first (uncurry AvailableMoneyExpr) <$> filteredAccounts
  where
    filteredAccounts = AM.toList $ AM.filter (< bound) accounts

genChoiceValueLT :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genChoiceValueLT bound State{..} = case filteredChoices of
  []
    | bound > 0 -> Just $ flip MemoExpr 0 . ChoiceValueExpr <$> arbitraryNotIn choices
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first ChoiceValueExpr <$> filteredChoices
  where
    filteredChoices = AM.toList $ AM.filter (< bound) choices

arbitraryNotIn :: (Arbitrary k, PlutusTx.Eq.Eq k) => AM.Map k v -> Gen k
arbitraryNotIn values = arbitrary `suchThat` (not . flip AM.member values)

genUseValueLT :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genUseValueLT bound State{..} = case filteredBoundValues of
  []
    | bound > 0 -> Just $ flip MemoExpr 0 . UseValueExpr <$> arbitraryNotIn boundValues
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first UseValueExpr <$> filteredBoundValues
  where
    filteredBoundValues = AM.toList $ AM.filter (< bound) boundValues

genTimeIntervalStartLT :: Integer -> Environment -> Maybe (Gen (MemoExpr Integer))
genTimeIntervalStartLT bound (Environment (POSIXTime start, _))
  | start < bound = Just $ pure $ MemoExpr TimeIntervalStartExpr start
  | otherwise = Nothing

genTimeIntervalEndLT :: Integer -> Environment -> Maybe (Gen (MemoExpr Integer))
genTimeIntervalEndLT bound (Environment (_, POSIXTime end))
  | end < bound = Just $ pure $ MemoExpr TimeIntervalEndExpr end
  | otherwise = Nothing

genCondEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genCondEQ bound env state = do
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <-
    oneof
      [ (,,) <$> genTrueObs env state <*> genValueEQ bound env state <*> genValue env state
      , (,,) <$> genFalseObs env state <*> genValue env state <*> genValueEQ bound env state
      ]
  pure $ MemoExpr (CondExpr cExpr aExpr bExpr) if c then a else b

genAddEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genAddEQ bound env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueEQ (bound - a) env state
  pure $ MemoExpr (AddExpr expr1 expr2) $ a + b

genSubEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genSubEQ bound env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueEQ (a - bound) env state
  pure $ MemoExpr (SubExpr expr1 expr2) $ a - b

genMulEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genMulEQ bound env state
  | bound == 0 =
      oneof
        [ do
            expr1@(MemoExpr _ a) <- genValueEQ 0 env state
            expr2@(MemoExpr _ b) <- genValue env state
            pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
        , do
            expr1@(MemoExpr _ a) <- genValue env state
            expr2@(MemoExpr _ b) <- genValueEQ 0 env state
            pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
        ]
  | otherwise =
      oneof $
        divisors >>= \a ->
          [ do
              expr1 <- genValueEQ a env state
              expr2@(MemoExpr _ b) <- genValueEQ (bound `div` a) env state
              pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
          , do
              expr1@(MemoExpr _ b) <- genValueEQ (bound `div` a) env state
              expr2 <- genValueEQ a env state
              pure $ MemoExpr (MulExpr expr1 expr2) $ b * a
          ]
  where
    divisors :: [Integer]
    divisors = concatMap catNeg $ filter (`divides` bound) [1 .. (abs bound)]

    catNeg :: Integer -> [Integer]
    catNeg i = [i, -i]

    divides :: Integer -> Integer -> Bool
    divides a b = a `mod` b == 0

genDivEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genDivEQ bound env state =
  if bound /= 0
    then do
      a <- oneof [genIntegerGE 1, genIntegerLE (-1)]
      expr1@(MemoExpr _ a') <- genValueEQ (bound * a) env state
      expr2@(MemoExpr _ b) <- genValueEQ (a' `quot` bound) env state
      pure $ MemoExpr (DivExpr expr1 expr2) $ a' `quot` b
    else do
      expr1 <- genValueEQ 0 env state
      expr2 <- genValue env state
      pure $ MemoExpr (DivExpr expr1 expr2) 0

genNegEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genNegEQ bound env state =
  genValueEQ (negate bound) env state <&> \expr@(MemoExpr _ i) ->
    MemoExpr (NegExpr expr) (negate i)

genConstantEQ :: Integer -> Gen (MemoExpr Integer)
genConstantEQ = pure . uncurry MemoExpr . (ConstantExpr &&& id)

genAvailableMoneyEQ :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genAvailableMoneyEQ bound State{..} = case filteredAccounts of
  []
    | bound == 0 -> Just $ flip MemoExpr 0 . uncurry AvailableMoneyExpr <$> arbitraryNotIn accounts
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first (uncurry AvailableMoneyExpr) <$> filteredAccounts
  where
    filteredAccounts = AM.toList $ AM.filter (== bound) accounts

genChoiceValueEQ :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genChoiceValueEQ bound State{..} = case filteredChoices of
  []
    | bound == 0 -> Just $ flip MemoExpr 0 . ChoiceValueExpr <$> arbitraryNotIn choices
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first ChoiceValueExpr <$> filteredChoices
  where
    filteredChoices = AM.toList $ AM.filter (== bound) choices

genUseValueEQ :: Integer -> State -> Maybe (Gen (MemoExpr Integer))
genUseValueEQ bound State{..} = case filteredBoundValues of
  []
    | bound == 0 -> Just $ flip MemoExpr 0 . UseValueExpr <$> arbitraryNotIn boundValues
    | otherwise -> Nothing
  _ -> Just $ elements $ uncurry MemoExpr . first UseValueExpr <$> filteredBoundValues
  where
    filteredBoundValues = AM.toList $ AM.filter (== bound) boundValues

genTimeIntervalStartEQ :: Integer -> Environment -> Maybe (Gen (MemoExpr Integer))
genTimeIntervalStartEQ bound (Environment (POSIXTime start, _))
  | start == bound = Just $ pure $ MemoExpr TimeIntervalStartExpr start
  | otherwise = Nothing

genTimeIntervalEndEQ :: Integer -> Environment -> Maybe (Gen (MemoExpr Integer))
genTimeIntervalEndEQ bound (Environment (_, POSIXTime end))
  | end == bound = Just $ pure $ MemoExpr TimeIntervalEndExpr end
  | otherwise = Nothing

genIntegerGE :: Integer -> Gen Integer
genIntegerGE bound = case compare bound 0 of
  LT -> negate <$> genIntegerLE (negate bound)
  EQ -> abs <$> arbitrary
  GT -> (+ bound) . abs <$> arbitrary

genIntegerLE :: Integer -> Gen Integer
genIntegerLE bound = case compare bound 0 of
  LT -> negate <$> genIntegerGE (negate bound)
  EQ -> negate . abs <$> arbitrary
  GT -> (+ bound) . negate . abs <$> arbitrary

genObs :: Environment -> State -> Gen (MemoExpr Bool)
genObs env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNot env state
        , resize (size `quot` 2) $ genAnd env state
        , resize (size `quot` 2) $ genOr env state
        , resize (size `quot` 2) $ genGE env state
        , resize (size `quot` 2) $ genGT env state
        , resize (size `quot` 3) $ genLE env state
        , resize (size `quot` 3) $ genLT env state
        , resize (size `quot` 3) $ genEQ env state
        ]
  where
    genLeaf =
      oneof
        [ pure $ MemoExpr TrueExpr True
        , pure $ MemoExpr FalseExpr False
        , genChoseSomething state
        ]

genFalseObs :: Environment -> State -> Gen (MemoExpr Bool)
genFalseObs env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNotFalse env state
        , resize (size `quot` 2) $ genAndFalse env state
        , resize (size `quot` 2) $ genOrFalse env state
        , resize (size `quot` 2) $ genGEFalse env state
        , resize (size `quot` 2) $ genGTFalse env state
        , resize (size `quot` 3) $ genLEFalse env state
        , resize (size `quot` 3) $ genLTFalse env state
        , resize (size `quot` 3) $ genEQFalse env state
        ]
  where
    genLeaf =
      oneof
        [ pure $ MemoExpr FalseExpr False
        , genChoseSomethingFalse state
        ]

genTrueObs :: Environment -> State -> Gen (MemoExpr Bool)
genTrueObs env state = sized \size ->
  if size <= 0
    then genLeaf
    else
      oneof
        [ genLeaf
        , resize (pred size) $ genNotTrue env state
        , resize (size `quot` 2) $ genAndTrue env state
        , resize (size `quot` 2) $ genOrTrue env state
        , resize (size `quot` 2) $ genGETrue env state
        , resize (size `quot` 2) $ genGTTrue env state
        , resize (size `quot` 3) $ genLETrue env state
        , resize (size `quot` 3) $ genLTTrue env state
        , resize (size `quot` 3) $ genEQTrue env state
        ]
  where
    genLeaf =
      oneof $
        catMaybes
          [ Just $ pure $ MemoExpr TrueExpr True
          , genChoseSomethingTrue state
          ]

genNot :: Environment -> State -> Gen (MemoExpr Bool)
genNot env state = do
  expr@(MemoExpr _ a) <- genObs env state
  pure $ MemoExpr (NotExpr expr) $ not a

genAnd :: Environment -> State -> Gen (MemoExpr Bool)
genAnd env state = do
  expr1@(MemoExpr _ a) <- genObs env state
  expr2@(MemoExpr _ b) <- genObs env state
  pure $ MemoExpr (AndExpr expr1 expr2) $ a && b

genOr :: Environment -> State -> Gen (MemoExpr Bool)
genOr env state = do
  expr1@(MemoExpr _ a) <- genObs env state
  expr2@(MemoExpr _ b) <- genObs env state
  pure $ MemoExpr (OrExpr expr1 expr2) $ a || b

genGE :: Environment -> State -> Gen (MemoExpr Bool)
genGE env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (ValueGEExpr expr1 expr2) $ a >= b

genGT :: Environment -> State -> Gen (MemoExpr Bool)
genGT env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (ValueGTExpr expr1 expr2) $ a > b

genLE :: Environment -> State -> Gen (MemoExpr Bool)
genLE env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (ValueLEExpr expr1 expr2) $ a <= b

genLT :: Environment -> State -> Gen (MemoExpr Bool)
genLT env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValue env state
  pure $ MemoExpr (ValueLTExpr expr1 expr2) $ a < b

genEQ :: Environment -> State -> Gen (MemoExpr Bool)
genEQ env state =
  oneof
    [ genEQTrue env state
    , genEQFalse env state
    ]

genChoseSomething :: State -> Gen (MemoExpr Bool)
genChoseSomething State{..} =
  oneof $
    catMaybes
      [ Just do
          choiceId <- arbitrary
          pure $ MemoExpr (ChoseSomethingExpr choiceId) $ AM.member choiceId choices
      , guard (not $ AM.null choices) $> do
          choiceId <- elements $ AM.keys choices
          pure $ MemoExpr (ChoseSomethingExpr choiceId) True
      ]

genNotFalse :: Environment -> State -> Gen (MemoExpr Bool)
genNotFalse env state = do
  expr@(MemoExpr _ a) <- genTrueObs env state
  pure $ MemoExpr (NotExpr expr) $ not a

genAndFalse :: Environment -> State -> Gen (MemoExpr Bool)
genAndFalse env state = do
  expr1@(MemoExpr _ a) <- genObs env state
  expr2@(MemoExpr _ b) <- if a then genFalseObs env state else genObs env state
  pure $ MemoExpr (AndExpr expr1 expr2) $ a && b

genOrFalse :: Environment -> State -> Gen (MemoExpr Bool)
genOrFalse env state = do
  expr1@(MemoExpr _ a) <- genFalseObs env state
  expr2@(MemoExpr _ b) <- genFalseObs env state
  pure $ MemoExpr (OrExpr expr1 expr2) $ a || b

genGEFalse :: Environment -> State -> Gen (MemoExpr Bool)
genGEFalse env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueGT a env state
  pure $ MemoExpr (ValueGEExpr expr1 expr2) $ a >= b

genGTFalse :: Environment -> State -> Gen (MemoExpr Bool)
genGTFalse env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueGE a env state
  pure $ MemoExpr (ValueGTExpr expr1 expr2) $ a > b

genLEFalse :: Environment -> State -> Gen (MemoExpr Bool)
genLEFalse env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueLT a env state
  pure $ MemoExpr (ValueLEExpr expr1 expr2) $ a <= b

genLTFalse :: Environment -> State -> Gen (MemoExpr Bool)
genLTFalse env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueLE a env state
  pure $ MemoExpr (ValueLTExpr expr1 expr2) $ a < b

genEQFalse :: Environment -> State -> Gen (MemoExpr Bool)
genEQFalse env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueNE a env state
  pure $ MemoExpr (ValueEQExpr expr1 expr2) $ a == b

genChoseSomethingFalse :: State -> Gen (MemoExpr Bool)
genChoseSomethingFalse State{..} =
  oneof $
    catMaybes
      [ Just do
          choiceId <- arbitrary `suchThat` (not . flip AM.member choices)
          pure $ MemoExpr (ChoseSomethingExpr choiceId) $ AM.member choiceId choices
      ]

genNotTrue :: Environment -> State -> Gen (MemoExpr Bool)
genNotTrue env state = do
  expr@(MemoExpr _ a) <- genFalseObs env state
  pure $ MemoExpr (NotExpr expr) $ not a

genAndTrue :: Environment -> State -> Gen (MemoExpr Bool)
genAndTrue env state = do
  expr1@(MemoExpr _ a) <- genTrueObs env state
  expr2@(MemoExpr _ b) <- genTrueObs env state
  pure $ MemoExpr (AndExpr expr1 expr2) $ a && b

genOrTrue :: Environment -> State -> Gen (MemoExpr Bool)
genOrTrue env state = do
  expr1@(MemoExpr _ a) <- genObs env state
  expr2@(MemoExpr _ b) <- if a then genObs env state else genTrueObs env state
  pure $ MemoExpr (OrExpr expr1 expr2) $ a || b

genGETrue :: Environment -> State -> Gen (MemoExpr Bool)
genGETrue env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueLE a env state
  pure $ MemoExpr (ValueGEExpr expr1 expr2) $ a >= b

genGTTrue :: Environment -> State -> Gen (MemoExpr Bool)
genGTTrue env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueLT a env state
  pure $ MemoExpr (ValueGTExpr expr1 expr2) $ a > b

genLETrue :: Environment -> State -> Gen (MemoExpr Bool)
genLETrue env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueGE a env state
  pure $ MemoExpr (ValueLEExpr expr1 expr2) $ a <= b

genLTTrue :: Environment -> State -> Gen (MemoExpr Bool)
genLTTrue env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueGT a env state
  pure $ MemoExpr (ValueLTExpr expr1 expr2) $ a < b

genEQTrue :: Environment -> State -> Gen (MemoExpr Bool)
genEQTrue env state = do
  expr1@(MemoExpr _ a) <- genValue env state
  expr2@(MemoExpr _ b) <- genValueEQ a env state
  pure $ MemoExpr (ValueEQExpr expr1 expr2) $ a == b

genChoseSomethingTrue :: State -> Maybe (Gen (MemoExpr Bool))
genChoseSomethingTrue State{..} =
  guard (not $ AM.null choices) $> do
    choiceId <- elements $ AM.keys choices
    pure $ MemoExpr (ChoseSomethingExpr choiceId) True

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
