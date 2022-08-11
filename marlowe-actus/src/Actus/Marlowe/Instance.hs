{-# LANGUAGE FlexibleContexts   #-}
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

import Actus.Domain (ActusFrac (..), ActusOps (..), CashFlow, ContractState, ContractTerms, RiskFactors)
import Data.Functor ((<&>))
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
    where
      _max x y = Cond (ValueGT x y) x y
  fromInteger n = Constant $ n * marloweFixedPoint
  negate a = NegValue a
  signum a =
    Cond (ValueLT a 0) (-1) $
      Cond (ValueGT a 0) 1 0

instance ActusOps Value where
  _min x y = Cond (ValueLT x y) x y
  _max x y = Cond (ValueGT x y) x y

instance ActusFrac Value where
  _ceiling x = fromMaybe (error "ActusFrac partially implemented") (evalVal x)

instance Fractional Value where
  x / y = reduceValue $ DivValue (MulValue (Constant marloweFixedPoint) x) y
  fromRational (x :% y) = DivValue (fromInteger (marloweFixedPoint * x)) (fromInteger y)

evalVal :: Value -> Maybe Integer
evalVal d = toCore d <&> Core.evalValue env state
  where
    env = Core.Environment {Core.timeInterval = (Ledger.POSIXTime 0, Ledger.POSIXTime 0)}
    state = Core.emptyState $ Ledger.POSIXTime 0

evalObs :: Observation -> Maybe Bool
evalObs d = toCore d <&> Core.evalObservation env state
  where
    env = Core.Environment {Core.timeInterval = (Ledger.POSIXTime 0, Ledger.POSIXTime 0)}
    state = Core.emptyState $ Ledger.POSIXTime 0

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
