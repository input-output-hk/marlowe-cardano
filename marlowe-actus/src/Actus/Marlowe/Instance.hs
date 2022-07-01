{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Actus.Marlowe.Instance
  (
  -- * Marlowe types
    CashFlowMarlowe
  , ContractStateMarlowe
  , ContractTermsMarlowe
  , RiskFactorsMarlowe
  -- * Contract reduction
  , reduceContract
  )
where

import Actus.Domain (ActusNum (..), ActusOps (..), CashFlow, ContractState, ContractTerms, RiskFactors, RoleSignOps,
                     ScheduleOps (..), YearFractionOps (..), marloweFixedPoint)
import Actus.Utility (yearFraction)
import Language.Marlowe

type CashFlowMarlowe = CashFlow (Value Observation)
type ContractStateMarlowe = ContractState (Value Observation)
type ContractTermsMarlowe = ContractTerms (Value Observation)
type RiskFactorsMarlowe = RiskFactors (Value Observation)

instance YearFractionOps (Value Observation) where
    _y a b c d = Constant . toMarloweFixedPoint $ yearFraction a b c d
      where
        toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint Prelude.*)

instance ScheduleOps (Value Observation) where
    _ceiling (Constant a) = ceiling $ (fromInteger  a :: Double) Prelude./ (fromInteger marloweFixedPoint :: Double)
    -- ACTUS is implemented only for Fixed Schedules
    -- that means schedules are known before the contract
    -- is exectued, resp. the schedule do not depend on
    -- riskfactors
    _ceiling _            = error "Precondition: Fixed schedules"

instance ActusOps (Value Observation) where
    _min a b = Cond (ValueLT a b) a b
    _max a b = Cond (ValueGT a b) a b
    _abs a = _max a (SubValue _zero a)
    _zero = Constant 0
    _one  = Constant marloweFixedPoint
    _fromInteger n = Constant $ n Prelude.* marloweFixedPoint
    _negate a = NegValue a

instance RoleSignOps (Value Observation)

-- In order to have manageble contract sizes, we need to reduce Value as
-- good as possible. Note: this interfers with the semantics - ideally
-- we would have formally verified reduction semantics instead
instance ActusNum (Value Observation) where
  x + y = reduceValue $ AddValue x y
  x - y = reduceValue $ SubValue x y
  x * y = reduceValue $ DivValue (MulValue x y) (Constant marloweFixedPoint)
  x / y = reduceValue $ MulValue (DivValue x y) (Constant marloweFixedPoint)

-- | Reduce the contract representation in size, the semantics of the
-- contract are not changed. TODO: formal verification
reduceContract :: Contract -> Contract
reduceContract Close = Close
reduceContract (Pay a b c d e) = Pay a b c (reduceValue d) (reduceContract e)
reduceContract (When cs t c) = When (map f cs) t (reduceContract c)
  where
    f (Case a x)           = Case a (reduceContract x)
    f (MerkleizedCase a x) = MerkleizedCase a x
reduceContract (If obs a b) = let c = evalObs obs in if c then reduceContract a else reduceContract b
reduceContract (Let v o c) = Let v (reduceValue o) (reduceContract c)
reduceContract (Assert o c) = Assert (reduceObs o) (reduceContract c)

evalObs :: Observation -> Bool
evalObs (AndObs lhs rhs)   = evalObs lhs && evalObs rhs
evalObs (OrObs lhs rhs)    = evalObs lhs || evalObs rhs
evalObs (NotObs subObs)    = not (evalObs subObs)
evalObs (ChoseSomething _) = error "evalObs not implemented for ChoseSomething"
evalObs (ValueGE lhs rhs)  = evalVal lhs >= evalVal rhs
evalObs (ValueGT lhs rhs)  = evalVal lhs > evalVal rhs
evalObs (ValueLT lhs rhs)  = evalVal lhs < evalVal rhs
evalObs (ValueLE lhs rhs)  = evalVal lhs <= evalVal rhs
evalObs (ValueEQ lhs rhs)  = evalVal lhs == evalVal rhs
evalObs TrueObs            = True
evalObs FalseObs           = False

evalVal :: Value Observation -> Integer
evalVal (AvailableMoney _ _) = error "evalVal not implemented for AvailableMoney"
evalVal (Constant n) = n
evalVal (NegValue val) = negate (evalVal val)
evalVal (AddValue lhs rhs) = evalVal lhs Prelude.+ evalVal rhs
evalVal (SubValue lhs rhs) = evalVal lhs Prelude.- evalVal rhs
evalVal (MulValue lhs rhs) = evalVal lhs Prelude.* evalVal rhs
evalVal d@(DivValue (Constant _) (Constant _)) = evalValue env state d
  where
    env = Environment {timeInterval = (POSIXTime 0, POSIXTime 0)}
    state = emptyState $ POSIXTime 0
evalVal (DivValue n m) = evalVal $ DivValue (reduceValue n) (reduceValue m)
evalVal (ChoiceValue _) = error "evalVal not implemented for ChoiceValue"
evalVal TimeIntervalStart = error "evalVal not implemented for TimeIntervalStart"
evalVal TimeIntervalEnd = error "evalVal not implemented for TimeIntervalEnd"
evalVal (UseValue _) = error "evalVal not implemented for UseValue"
evalVal (Cond o a b) = if evalObs o then evalVal a else evalVal b

reduceObs :: Observation -> Observation
reduceObs (AndObs a b)  = AndObs (reduceObs a) (reduceObs b)
reduceObs (OrObs a b)   = OrObs (reduceObs a) (reduceObs b)
reduceObs (NotObs a)    = NotObs (reduceObs a)
reduceObs (ValueGE a b) = ValueGE (reduceValue a) (reduceValue b)
reduceObs (ValueGT a b) = ValueGT (reduceValue a) (reduceValue b)
reduceObs (ValueLE a b) = ValueLE (reduceValue a) (reduceValue b)
reduceObs (ValueLT a b) = ValueLT (reduceValue a) (reduceValue b)
reduceObs (ValueEQ a b) = ValueEQ (reduceValue a) (reduceValue b)
reduceObs x             = x

reduceValue :: Value Observation -> Value Observation
reduceValue = converge reduceValue'
  where
    converge :: Eq a => (a -> a) -> a -> a
    converge = until =<< ((==) =<<)

    reduceValue' :: Value Observation -> Value Observation
    reduceValue' (ChoiceValue i) = ChoiceValue i
    reduceValue' (UseValue i) = UseValue i
    reduceValue' (Constant i) = Constant i
    reduceValue' (AddValue (Constant x) (Constant y)) = Constant $ x Prelude.+ y
    reduceValue' (AddValue (Constant 0) x) = x
    reduceValue' (AddValue x (Constant 0)) = x
    reduceValue' (AddValue x y) = AddValue (reduceValue' x) (reduceValue' y)
    reduceValue' (SubValue (Constant x) (Constant y)) = Constant $ x Prelude.- y
    reduceValue' (SubValue x (Constant 0)) = x
    reduceValue' (SubValue (Constant 0) x) = NegValue x
    reduceValue' (SubValue x y) = SubValue (reduceValue' x) (reduceValue' y)
    reduceValue' (MulValue (Constant x) (Constant y)) = Constant $ x Prelude.* y
    reduceValue' (MulValue (DivValue a b) (DivValue x y)) = DivValue (MulValue (reduceValue' a) (reduceValue' x)) (MulValue (reduceValue' b) (reduceValue' y))
    reduceValue' (MulValue (DivValue a b) (Constant x)) = DivValue (MulValue (reduceValue' a) (Constant x)) (reduceValue' b)
    reduceValue' (MulValue x y) = MulValue (reduceValue' x) (reduceValue' y)
    reduceValue' (DivValue (Constant x) (Constant y)) | rem x y == 0 = Constant (x `div` y)
    reduceValue' d@(DivValue (Constant _) (Constant _)) = Constant $ evalVal d
    reduceValue' (DivValue x y) = DivValue (reduceValue' x) (reduceValue' y)
    reduceValue' (NegValue (Constant x)) = Constant $ - x
    reduceValue' (NegValue v) = NegValue (reduceValue' v)
    reduceValue' (Cond (ValueGT (Constant x) (Constant y)) a _) | x > y = reduceValue' a
    reduceValue' (Cond (ValueGT (Constant _) (Constant _)) _ b) = reduceValue' b
    reduceValue' (Cond (ValueLT (Constant x) (Constant y)) a _) | x < y = reduceValue' a
    reduceValue' (Cond (ValueLT (Constant _) (Constant _)) _ b) = reduceValue' b
    reduceValue' (Cond o a b) = Cond (reduceObs o) (reduceValue' a) (reduceValue' b)
    reduceValue' x = x
