{-# LANGUAGE FlexibleInstances #-}
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

import Actus.Domain (CashFlow, ContractState, ContractTerms, RiskFactors, marloweFixedPoint)
import GHC.Real (Ratio (..))
import Language.Marlowe

type CashFlowMarlowe = CashFlow (Value Observation)
type ContractStateMarlowe = ContractState (Value Observation)
type ContractTermsMarlowe = ContractTerms (Value Observation)
type RiskFactorsMarlowe = RiskFactors (Value Observation)

-- In order to have manageble contract sizes, we need to reduce Value as
-- good as possible. Note: this interfers with the semantics - ideally
-- we would have formally verified reduction semantics instead
instance Num (Value Observation) where
  x + y = reduceValue $ AddValue x y
  x - y = reduceValue $ SubValue x y
  x * y = reduceValue $ DivValue (MulValue x y) (Constant marloweFixedPoint)
  abs a = _max a (NegValue a)
    where _max x y = Cond (ValueGT x y) x y
  fromInteger n = Constant $ n * marloweFixedPoint
  negate a = NegValue a
  signum _ = undefined

instance Fractional (Value Observation) where
  x / y = reduceValue $ DivValue (MulValue (Constant marloweFixedPoint) x) y
  fromRational (x:%y) = DivValue (fromInteger (marloweFixedPoint * x)) (fromInteger y)

instance Real (Value Observation) where
  toRational _ = undefined

instance RealFrac (Value Observation) where
  properFraction _ = undefined
  ceiling _ = undefined

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
evalVal (AddValue lhs rhs) = evalVal lhs + evalVal rhs
evalVal (SubValue lhs rhs) = evalVal lhs - evalVal rhs
evalVal (MulValue lhs rhs) = evalVal lhs * evalVal rhs
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
reduceValue v = Constant $ evalVal v
