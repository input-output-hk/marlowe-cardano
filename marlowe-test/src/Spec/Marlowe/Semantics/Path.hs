{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.Marlowe.Semantics.Path
  ( genFalseObs
  , genObs
  , genTrueObs
  , genValue
  , genValueGE
  , genValueGT
  , genValueLE
  , genValueLT
  , tests
  ) where

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Bifunctor (first)
import Data.Functor (($>), (<&>))
import Data.Maybe (catMaybes)
import Language.Marlowe.Core.V1.Semantics (evalObservation, evalValue)
import Language.Marlowe.Core.V1.Semantics.Types
import Plutus.V2.Ledger.Api (POSIXTime(..))
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Eq
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.QuickCheck
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

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
    else oneof
      [ genLeaf
      , resize (pred size) $ genNeg env state
      , resize (size `quot` 2) $ genAdd env state
      , resize (size `quot` 2) $ genSub env state
      , resize (size `quot` 2) $ genMul env state
      , resize (size `quot` 2) $ genDiv env state
      , resize (size `quot` 3) $ genCond env state
      ]
  where
    genLeaf = oneof
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
    else oneof
      [ genLeaf
      , resize (pred size) $ genNegGT bound env state
      , resize (size `quot` 2) $ genAddGT bound env state
      , resize (size `quot` 2) $ genSubGT bound env state
      , resize (size `quot` 2) $ genMulGT bound env state
      , resize (size `quot` 2) $ genDivGT bound env state
      , resize (size `quot` 3) $ genCondGT bound env state
      ]
    where
      genLeaf = oneof $ catMaybes
        [ Just $ genConstantGT bound
        , genAvailableMoneyGT bound state
        , genChoiceValueGT bound state
        , genUseValueGT bound state
        , genTimeIntervalStartGT bound env
        , genTimeIntervalEndGT bound env
        ]

-- | Generate a value greater than or equal to a given number.
genValueGE :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueGE bound env state = frequency
  [ (9, genValueGT bound env state)
  , (1, genValueEQ bound env state)
  ]

-- | Generate a value less than a given number.
genValueLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueLT bound env state = sized \size ->
  if size <= 0
    then genLeaf
    else oneof
      [ genLeaf
      , resize (pred size) $ genNegLT bound env state
      , resize (size `quot` 2) $ genAddLT bound env state
      , resize (size `quot` 2) $ genSubLT bound env state
      , resize (size `quot` 2) $ genMulLT bound env state
      , resize (size `quot` 2) $ genDivLT bound env state
      , resize (size `quot` 3) $ genCondLT bound env state
      ]
    where
      genLeaf = oneof $ catMaybes
        [ Just $ genConstantLT bound
        , genAvailableMoneyLT bound state
        , genChoiceValueLT bound state
        , genUseValueLT bound state
        , genTimeIntervalStartLT bound env
        , genTimeIntervalEndLT bound env
        ]

-- | Generate a value less than or equal to a given number.
genValueLE :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueLE bound env state = frequency
  [ (9, genValueLT bound env state)
  , (1, genValueEQ bound env state)
  ]

-- | Generate a value equal to a given number
genValueEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genValueEQ bound env state = sized \size ->
  if size <= 0
    then genLeaf
    else oneof
      [ genLeaf
      , resize (pred size) $ genNegEQ bound env state
      , resize (size `quot` 2) $ genAddEQ bound env state
      , resize (size `quot` 2) $ genSubEQ bound env state
      , resize (size `quot` 2) $ genMulEQ bound env state
      , resize (size `quot` 2) $ genDivEQ bound env state
      , resize (size `quot` 3) $ genCondEQ bound env state
      ]
    where
      genLeaf = oneof $ catMaybes
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
genAvailableMoney State{..} = oneof
  $ (flip MemoExpr 0 . uncurry AvailableMoneyExpr <$> arbitraryNotIn accounts)
  : fmap pure (uncurry MemoExpr . first (uncurry AvailableMoneyExpr) <$> AM.toList accounts)

genChoiceValue :: State -> Gen (MemoExpr Integer)
genChoiceValue State{..} = oneof
  $ (flip MemoExpr 0 . ChoiceValueExpr <$> arbitraryNotIn choices)
  : fmap pure (uncurry MemoExpr . first ChoiceValueExpr <$> AM.toList choices)

genUseValue :: State -> Gen (MemoExpr Integer)
genUseValue State{..} = oneof
  $ (flip MemoExpr 0 . UseValueExpr <$> arbitraryNotIn boundValues)
  : fmap pure (uncurry MemoExpr . first UseValueExpr <$> AM.toList boundValues)

genTimeIntervalStart :: Environment -> Gen (MemoExpr Integer)
genTimeIntervalStart (Environment (POSIXTime start, _)) = pure $ MemoExpr TimeIntervalStartExpr start

genTimeIntervalEnd :: Environment -> Gen (MemoExpr Integer)
genTimeIntervalEnd (Environment (_, POSIXTime end)) = pure $ MemoExpr TimeIntervalEndExpr end

genCondGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genCondGT bound env state = do
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <- oneof
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
    expr2@(MemoExpr _ b) <- if a < 0
      then case compare (abs a) bound of
        GT -> genValueLT 0 env state
        EQ -> genValueLT (-1) env state
        LT -> genValueLT (bound `quot` a) env state `suchThat` nonzero
      else genValueGT (bound `quot` a) env state `suchThat` nonzero
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
  EQ -> oneof
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
      GT -> if a < abs bound
        then genValueGT (bound `quot` a) env state
        else genValueGE 0 env state
      EQ -> genValue env state
      LT -> if a > bound
        then genValueLT (bound `quot` a) env state
        else genValueLT 0 env state
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b

genDivGT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genDivGT bound env state = case compare bound 0 of
  GT -> oneof
    [ do
      expr1@(MemoExpr _ a) <- genValueGT bound env state
      let smallEnough b = (a `quot` b) > bound
      b <- elements $ takeWhile smallEnough [1..]
      pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
    ]
  EQ -> oneof
    [ do
      expr1@(MemoExpr _ a) <- genValueGT 0 env state
      b <- chooseInteger (1, a)
      pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
    , do
      expr1@(MemoExpr _ a) <- genValueLT 0 env state
      b <- chooseInteger (-1, a)
      pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
    ]
  LT -> oneof
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
genNegGT bound env state = genValueLT (negate bound) env state <&> \expr@(MemoExpr _ i) ->
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
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <- oneof
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
      GT -> if a < bound
        then genValueLT (bound `quot` a) env state
        else genValueLE 0 env state
      EQ -> genValue env state
      LT -> if abs a < bound
        then genValueGT (bound `quot` a) env state
        else genValueGT 0 env state
    pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
  EQ -> oneof
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
    expr2@(MemoExpr _ b) <- if a > 0
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
  GT -> oneof
    -- a >= 0 && b <= 0 && c > 0 => a / b < c
    [ do
      expr1@(MemoExpr _ a) <- genValueGE 0 env state
      expr2@(MemoExpr _ b) <- genValueLE 0 env state
      pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
    -- a >= 0 && b > a / c && c > 0 => a / b < c
    , do
      expr1@(MemoExpr _ a) <- genValueGE 0 env state
      expr2@(MemoExpr _ b) <- genValueGT (a `quot` bound) env state
      pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
    -- a <= 0 && b >= 0 && c > 0 => a / b < c
    , do
      expr1@(MemoExpr _ a) <- genValueLE 0 env state
      expr2@(MemoExpr _ b) <- genValueGE 0 env state
      pure $ MemoExpr (DivExpr expr1 expr2) if b == 0 then 0 else a `quot` b
    ]
  EQ -> oneof
    -- a > 0 && b < 0 => a / b < 0
    [ do
      expr1@(MemoExpr _ a) <- genValueGT 0 env state
      b <- chooseInteger (-1, negate a)
      pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
    -- a < 0 && b > 0 => a / b < 0
    , do
      expr1@(MemoExpr _ a) <- genValueLT 0 env state
      b <- chooseInteger (1, negate a)
      pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
    ]
  LT -> oneof
    [ do
      expr1@(MemoExpr _ a) <- genValueLT bound env state
      let smallEnough b = (a `quot` b) < bound
      b <- elements $ takeWhile smallEnough [1..]
      pure $ MemoExpr (DivExpr expr1 (MemoExpr (ConstantExpr b) b)) $ a `quot` b
    ]

genNegLT :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genNegLT bound env state = genValueGT (negate bound) env state <&> \expr@(MemoExpr _ i) ->
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
  (cExpr@(MemoExpr _ c), aExpr@(MemoExpr _ a), bExpr@(MemoExpr _ b)) <- oneof
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
  | bound == 0 = oneof
      [ do
        expr1@(MemoExpr _ a) <- genValueEQ 0 env state
        expr2@(MemoExpr _ b) <- genValue env state
        pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
      , do
        expr1@(MemoExpr _ a) <- genValue env state
        expr2@(MemoExpr _ b) <- genValueEQ 0 env state
        pure $ MemoExpr (MulExpr expr1 expr2) $ a * b
      ]
  | otherwise = oneof $ divisors >>= \a ->
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
    divisors = concatMap catNeg $ filter (`divides` bound) [1..(abs bound)]

    catNeg :: Integer -> [Integer]
    catNeg i = [i, -i]

    divides :: Integer -> Integer -> Bool
    divides a b = a `mod` b == 0

genDivEQ :: Integer -> Environment -> State -> Gen (MemoExpr Integer)
genDivEQ bound env state = if bound /= 0
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
genNegEQ bound env state = genValueEQ (negate bound) env state <&> \expr@(MemoExpr _ i) ->
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
    else oneof
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
    genLeaf = oneof
      [ pure $ MemoExpr TrueExpr True
      , pure $ MemoExpr FalseExpr False
      , genChoseSomething state
      ]

genFalseObs :: Environment -> State -> Gen (MemoExpr Bool)
genFalseObs env state = sized \size ->
  if size <= 0
    then genLeaf
    else oneof
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
    genLeaf = oneof
      [ pure $ MemoExpr FalseExpr False
      , genChoseSomethingFalse state
      ]

genTrueObs :: Environment -> State -> Gen (MemoExpr Bool)
genTrueObs env state = sized \size ->
  if size <= 0
    then genLeaf
    else oneof
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
    genLeaf = oneof $ catMaybes
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
genEQ env state = oneof
  [ genEQTrue env state
  , genEQFalse env state
  ]

genChoseSomething :: State -> Gen (MemoExpr Bool)
genChoseSomething State{..} = oneof $ catMaybes
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
genChoseSomethingFalse State{..} = oneof $ catMaybes
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
genChoseSomethingTrue State{..} = guard (not $ AM.null choices) $> do
  choiceId <- elements $ AM.keys choices
  pure $ MemoExpr (ChoseSomethingExpr choiceId) True

tests :: TestTree
tests = testGroup "Path"
  [ valueTests
  , obsTests
  ]

valueTests :: TestTree
valueTests = testGroup "Value"
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
  ]

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
      let
        res = rel b a
        interpret True = relName
        interpret False = invRelName
      in
        counterexample (show b <> " " <> interpret res <> " " <> show a) res

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

obsTests :: TestTree
obsTests = testGroup "Observation"
  [
  ]
