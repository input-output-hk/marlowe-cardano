module Actus.Domain.Ops where

import Actus.Domain.ContractTerms (CR (..), DCC (..))
import Data.Time (LocalTime)

marloweFixedPoint :: Integer
marloweFixedPoint = 1000000

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

infixl 7  *, /
infixl 6  +, -

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
