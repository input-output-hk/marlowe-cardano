{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Marlowe.ACTUS.Domain.Ops where

import           Data.Time                                   (LocalTime)
import           Language.Marlowe                            (Observation (ValueGT, ValueLT),
                                                              Value (AddValue, Cond, Constant, DivValue, MulValue, NegValue, SubValue))
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

class ActusNum a where
    (+) :: a -> a -> a
    (-) :: a -> a -> a
    (*) :: a -> a -> a
    (/) :: a -> a -> a

class YearFractionOps a b where
    _y :: DCC -> a -> a -> Maybe a -> b

class DateOps a b where
    _lt :: a -> a -> b --returns pseudo-boolean

_minusOne :: (ActusNum a, ActusOps a) => a
_minusOne = _zero Language.Marlowe.ACTUS.Domain.Ops.- _one

class (ActusNum a, ActusOps a) => RoleSignOps a where
    _r :: CR -> a
    _r CR_RPA = _one
    _r CR_RPL = _minusOne
    _r CR_CLO = _one
    _r CR_CNO = _one
    _r CR_COL = _one
    _r CR_LG  = _one
    _r CR_ST  = _minusOne
    _r CR_BUY = _one
    _r CR_SEL = _minusOne
    _r CR_RFL = _one
    _r CR_PFL = _minusOne
    _r CR_RF  = _one
    _r CR_PF  = _minusOne

instance RoleSignOps Double
instance RoleSignOps (Value Observation)

instance ActusOps Double where
    _min  = min
    _max  = max
    _abs  = abs
    _zero = 0.0
    _one  = 1.0

instance ActusNum Double where
    a + b       = a Prelude.+ b
    a - b       = a Prelude.- b
    a * b       = a Prelude.* b
    a / b       = a Prelude./ b

instance DateOps LocalTime Double where
    _lt a b = if a < b then _one else _zero

instance YearFractionOps LocalTime Double where
    _y = yearFraction

instance ActusOps (Value Observation) where
    _min a b = Cond (ValueLT a b) a b
    _max a b = Cond (ValueGT a b) a b
    _abs a = _max a (SubValue _zero a)
    _zero = Constant 0
    _one  = Constant marloweFixedPoint

instance DateOps (Value Observation) (Value Observation) where
    _lt a b = Cond (ValueLT a b) _one _zero

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
  (Constant x) / (Constant y) = Constant $ x `div` (y Prelude.* marloweFixedPoint)
  (Constant x) / y | rem x marloweFixedPoint == 0 = DivValue (Constant (x `div` marloweFixedPoint)) y
  x / (Constant y) | y == marloweFixedPoint = x
  x / (Constant y) | rem y marloweFixedPoint == 0 = DivValue x (Constant (y `div` marloweFixedPoint))
  x / y = DivValue x y
