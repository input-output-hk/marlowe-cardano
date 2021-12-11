{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.ACTUS.Domain.Ops where

import           Data.Time                                   (LocalTime)
import           Language.Marlowe                            (Observation (..), Value (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms (CR (..), DCC (..))
import           Language.Marlowe.ACTUS.Utility.YearFraction (yearFraction)

marloweFixedPoint :: Integer
marloweFixedPoint = 1000

class ActusOps a where
    _min  :: a -> a -> a
    _max  :: a -> a -> a
    _abs  :: a -> a
    _zero :: a
    _one  :: a
    _fromInteger :: Integer -> a
    _negate :: a -> a

class Eq a => ActusNum a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a

class YearFractionOps b where
    _y :: DCC -> LocalTime -> LocalTime -> Maybe LocalTime -> b

class ScheduleOps b where
    _ceiling :: b -> Integer

class (ActusNum a, ActusOps a) => RoleSignOps a where
    _r :: CR -> a
    _r CR_RPA = _one
    _r CR_RPL = _negate _one
    _r CR_CLO = _one
    _r CR_CNO = _one
    _r CR_COL = _one
    _r CR_LG  = _one
    _r CR_ST  = _negate _one
    _r CR_BUY = _one
    _r CR_SEL = _negate _one
    _r CR_RFL = _one
    _r CR_PFL = _negate _one
    _r CR_RF  = _one
    _r CR_PF  = _negate _one

instance RoleSignOps Double
instance RoleSignOps (Value Observation)

instance ActusOps Double where
    _min  = min
    _max  = max
    _abs  = abs
    _zero = 0.0
    _one  = 1.0
    _fromInteger = fromInteger
    _negate = negate

instance ActusNum Double where
    a + b       = a Prelude.+ b
    a - b       = a Prelude.- b
    a * b       = a Prelude.* b
    a / b       = a Prelude./ b

instance YearFractionOps Double where
    _y = yearFraction

instance ScheduleOps Double where
    _ceiling = ceiling

instance YearFractionOps (Value Observation) where
    _y a b c d = Constant . toMarloweFixedPoint $ yearFraction a b c d
      where
        toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint Prelude.*)

instance ScheduleOps (Value Observation) where
    _ceiling (Constant a) = a `div` marloweFixedPoint
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

infixl 7  *, /
infixl 6  +, -

instance ActusNum (Value Observation) where
  -- add
  x + (Constant 0)            = x
  (Constant 0) + y            = y
  (Constant x) + (Constant y) = Constant (x Prelude.+ y)
  x + y                       = AddValue x y

  -- sub
  x - (Constant 0)            = x
  (Constant 0) - y            = NegValue y
  (Constant x) - (Constant y) = Constant (x Prelude.- y)
  x - y                       = SubValue x y

  -- mul
  (Constant 0) * _ = Constant 0
  (Constant x) * (Constant y) = Constant (x Prelude.* y `div` marloweFixedPoint)
  (Constant x) * y | x == marloweFixedPoint = y
  (Constant x) * y | rem x marloweFixedPoint == 0 = MulValue (Constant (x `div` marloweFixedPoint)) y
  x * (Constant y) | rem y marloweFixedPoint == 0 = MulValue x (Constant (y `div` marloweFixedPoint))
  x * y = DivValue (MulValue x y) (Constant marloweFixedPoint)

  -- div
  (Constant 0) / _ = Constant 0
  (Constant x) / (Constant y) = Constant $ (x `div` y) Prelude.* marloweFixedPoint
  x / (Constant y) | y == marloweFixedPoint = x
  x / y = MulValue (DivValue x y) (Constant marloweFixedPoint)
