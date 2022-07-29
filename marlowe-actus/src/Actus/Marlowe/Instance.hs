{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Actus.Marlowe.Instance
  (
  -- * Marlowe types
    CashFlowMarlowe
  , ContractStateMarlowe
  , ContractTermsMarlowe
  , RiskFactorsMarlowe

  , toMarloweFixedPoint
  -- * Contract reduction
  , reduceContract
  )
where

import Actus.Domain (CashFlow, ContractState, ContractTerms, RiskFactors)
import Data.Maybe (fromMaybe)
import GHC.Real (Ratio (..))
import qualified Language.Marlowe.Core.V1.Semantics as Core
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import Language.Marlowe.Extended.V1
import qualified Ledger

type CashFlowMarlowe = CashFlow Value
type ContractStateMarlowe = ContractState Value
type ContractTermsMarlowe = ContractTerms Value
type RiskFactorsMarlowe = RiskFactors Value

marloweFixedPoint :: Integer
marloweFixedPoint = 1_000_000

toMarloweFixedPoint :: Double -> Integer
toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint *)

-- In order to have manageble contract sizes, we need to reduce Value as
-- good as possible. Note: this interfers with the semantics - ideally
-- we would have formally verified reduction semantics instead
instance Num Value where
  x + y = reduceValue $ AddValue x y
  x - y = reduceValue $ SubValue x y
  x * y = reduceValue $ DivValue (MulValue x y) (Constant marloweFixedPoint)
  abs a = _max a (NegValue a)
    where _max x y = Cond (ValueGT x y) x y
  fromInteger n = Constant $ n * marloweFixedPoint
  negate a = NegValue a
  signum _ = error "Num partially implemented"

instance Fractional Value where
  x / y = reduceValue $ DivValue (MulValue (Constant marloweFixedPoint) x) y
  fromRational (x:%y) = DivValue (fromInteger (marloweFixedPoint * x)) (fromInteger y)

instance Eq Value where
  n == m | evalVal n == evalVal m = True
  n == m | evalVal n /= evalVal m = False
  (==) _ _ = error "Eq partially implemented"

instance Ord Value where
  n `compare` m | evalVal n < evalVal m = LT
  n `compare` m | evalVal n > evalVal m = GT
  n `compare` m | evalVal n == evalVal m = EQ
  compare _ _ = error "Ord partially implemented"

instance Real Value where
  toRational _ = error "Real partially implemented"

instance RealFrac Value where
  properFraction _ = error "RealFrac partially implemented"
  ceiling _ = error "RealFrac partially implemented"

-- | Reduce the contract representation in size, the semantics of the
-- contract are not changed. TODO: formal verification
reduceContract :: Contract -> Contract
reduceContract Close = Close
reduceContract (Pay a b c d e) = Pay a b c (reduceValue d) (reduceContract e)
reduceContract (When cs t c) = When (map f cs) t (reduceContract c)
  where
    f (Case a x)           = Case a (reduceContract x)
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

evalVal :: Value -> Integer
evalVal (AvailableMoney _ _) = error "evalVal not implemented for AvailableMoney"
evalVal (Constant n) = n
evalVal (ConstantParam _) = error "evalVal not implemented for ConstantParam"
evalVal (NegValue val) = negate (evalVal val)
evalVal (AddValue lhs rhs) = evalVal lhs + evalVal rhs
evalVal (SubValue lhs rhs) = evalVal lhs - evalVal rhs
evalVal (MulValue lhs rhs) = evalVal lhs * evalVal rhs
evalVal d@(DivValue (Constant _) (Constant _)) = Core.evalValue env state (fromMaybe (error "toCore") $ toCore d)
  where
    env = Core.Environment {Core.timeInterval = (Ledger.POSIXTime 0, Ledger.POSIXTime 0)}
    state = Core.emptyState $ Ledger.POSIXTime 0
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

reduceValue :: Value -> Value
reduceValue v = Constant $ evalVal v
