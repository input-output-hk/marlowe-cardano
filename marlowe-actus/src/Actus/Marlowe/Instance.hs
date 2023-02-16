{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Actus.Marlowe.Instance
  ( -- * Marlowe types
    CashFlowMarlowe
  , ContractStateMarlowe
  , ContractTermsMarlowe
  , RiskFactorsMarlowe
  , fromMarloweFixedPoint
  , toMarloweFixedPoint
    -- * Contract reduction
  , reduceContract
  ) where

import Actus.Domain (CashFlow, ContractState, ContractTerms, MinMax(..), RiskFactors)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import GHC.Real (Ratio(..))
import qualified Language.Marlowe.Core.V1.Semantics as Core
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import Language.Marlowe.Extended.V1
import qualified Ledger
import qualified PlutusTx.Builtins as Builtins

{-# INLINABLE division #-}

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
-- good as possible.
--
-- Note:
--
-- * This interfers with the semantics - ideally we would have formally
--   verified reduction semantics instead
--
-- * There are partial implementations, as the evaluation of a Value
--   is not always possible
--
-- * The semantics of division, i.e. rounding changed. In order to
--   preserve financial rounding, in ACTUS we always have to use `division`
--   rather than `DivValue`
instance Num Value where
  x + y = reduceValue $ AddValue x y
  x - y = reduceValue $ SubValue x y
  x * y = reduceValue $ division (MulValue x y) (Constant marloweFixedPoint)
  abs a = _max a (NegValue a)
    where
      _max x y = Cond (ValueGT x y) x y
  fromInteger n = Constant $ n * marloweFixedPoint
  negate a = NegValue a
  signum a =
    Cond (ValueLT a 0) (-1) $
      Cond (ValueGT a 0) 1 0

instance MinMax Value where
  _min x y = Cond (ValueLT x y) x y
  _max x y = Cond (ValueGT x y) x y

instance Fractional Value where
  lhs / rhs = MulValue (division lhs rhs) (Constant marloweFixedPoint)
  fromRational (x :% y) = MulValue (division (fromInteger x) (fromInteger y)) (Constant marloweFixedPoint)

-- |Division with financial rounding
division :: Value -> Value -> Value
division lhs rhs = fromMaybe (error "Value cannot be evaluted") $
  do
    n <- evalVal lhs
    d <- evalVal rhs
    pure $ Constant (division' n d)
  where
    division' :: Integer -> Integer -> Integer
    division' 0 _ = 0
    division' _ 0 = 0
    division' n d =
      let (q, r) = n `quotRem` d
          ar = abs r * 2
          ad = abs d
       in if ar < ad
            then q -- reminder < 1/2
            else
              if ar > ad
                then q + signum n * signum d -- reminder > 1/2
                else
                  let -- reminder == 1/2
                      qIsEven = q `Builtins.remainderInteger` 2 == 0
                   in if qIsEven then q else q + signum n * signum d

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
  Just c  -> reduceContract (if c then a else b)
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
