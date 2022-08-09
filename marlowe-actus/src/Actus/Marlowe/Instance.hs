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
  , fromMarloweFixedPoint
  -- * Contract reduction
  , reduceContract
  )
where

import Actus.Domain (CashFlow, ContractState, ContractTerms, RiskFactors)
import Control.Applicative (liftA2)
import GHC.Real (Ratio (..))
import Language.Marlowe.Extended.V1

type CashFlowMarlowe = CashFlow Value
type ContractStateMarlowe = ContractState Value
type ContractTermsMarlowe = ContractTerms Value
type RiskFactorsMarlowe = RiskFactors Value

marloweFixedPoint :: Integer
marloweFixedPoint = 1_000_000

toMarloweFixedPoint :: Double -> Integer
toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint *)

fromMarloweFixedPoint :: Integer -> Integer
fromMarloweFixedPoint i = i `quot` marloweFixedPoint

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
  signum a | evalVal a > Just 0 = 1
  signum a | evalVal a < Just 0 = -1
  signum a | evalVal a == Just 0 = 0
  signum _ = error "Num partially implemented"

instance Fractional Value where
  x / y = reduceValue $ DivValue (MulValue (Constant marloweFixedPoint) x) y
  fromRational (x:%y) = DivValue (fromInteger (marloweFixedPoint * x)) (fromInteger y)

instance Eq Value where
  m == n = equals (evalVal m) (evalVal n)
    where
      equals :: Maybe Integer -> Maybe Integer -> Bool
      equals (Just i) (Just j) = i == j
      equals _ _               = error "Eq partially implemented"

instance Ord Value where
  m `compare` n | evalVal m < evalVal n = LT
  m `compare` n | evalVal m > evalVal n = GT
  m `compare` n | evalVal m == evalVal n = EQ
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
    f (Case a x) = Case a (reduceContract x)
reduceContract i@(If obs a b) = case evalObs obs of
  Just c  -> if c then reduceContract a else reduceContract b
  Nothing -> i
reduceContract (Let v o c) = Let v (reduceValue o) (reduceContract c)
reduceContract (Assert o c) = Assert (reduceObs o) (reduceContract c)

evalObs :: Observation -> Maybe Bool
evalObs (AndObs lhs rhs)   = liftA2 (&&) (evalObs lhs) (evalObs rhs)
evalObs (OrObs lhs rhs)    = liftA2 (||) (evalObs lhs) (evalObs rhs)
evalObs (NotObs subObs)    = not <$> evalObs subObs
evalObs (ChoseSomething _) = Nothing
evalObs (ValueGT lhs rhs)  = liftA2 (>) (evalVal lhs) (evalVal rhs)
evalObs (ValueLT lhs rhs)  = liftA2 (<) (evalVal lhs) (evalVal rhs)
evalObs (ValueGE lhs rhs)  = liftA2 (>=) (evalVal lhs) (evalVal rhs)
evalObs (ValueLE lhs rhs)  = liftA2 (<=) (evalVal lhs) (evalVal rhs)
evalObs (ValueEQ lhs rhs)  = liftA2 (==) (evalVal lhs) (evalVal rhs)
evalObs TrueObs            = Just True
evalObs FalseObs           = Just False

evalVal :: Value -> Maybe Integer
evalVal (AvailableMoney _ _) = Nothing
evalVal (Constant n)         = Just n
evalVal (ConstantParam _)    = Nothing
evalVal (NegValue val)       = negate <$> evalVal val
evalVal (AddValue lhs rhs)   = liftA2 (+) (evalVal lhs) (evalVal rhs)
evalVal (SubValue lhs rhs)   = liftA2 (-) (evalVal lhs) (evalVal rhs)
evalVal (MulValue lhs rhs)   = liftA2 (*) (evalVal lhs) (evalVal rhs)
evalVal (DivValue lhs rhs)   = liftA2 quot (evalVal lhs) (evalVal rhs)
evalVal (ChoiceValue _)      = Nothing
evalVal TimeIntervalStart    = Nothing
evalVal TimeIntervalEnd      = Nothing
evalVal (UseValue _)         = Nothing
evalVal (Cond o a b)         = evalObs o >>= \obs -> if obs then evalVal a else evalVal b

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
reduceValue v = maybe v Constant (evalVal v)
