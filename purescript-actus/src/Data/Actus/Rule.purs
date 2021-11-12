module Data.Actus.Applicability
  ( X
  , NN
  , NA
  , X_I_1(..)
  , X_I_2(..)
  , X_I_4(..)
  , NN_I_1(..)
  , NN_I_3(..)
  ) where

import Prelude

import Data.Const (Const)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Some (Some)

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

type CA f a = (a -> a) -> f a

type Rule f a = CA f a -> f a

-- type Rule :: CA Row Type -> Row Type

-- | The X rule represents an optional field
type X :: Rule Row Type
type X f = f Maybe

-- | The NN rule represents a mandatory field
type NN :: Rule Row Type
type NN f = f Identity

-- | The NA rule represents a non-applicable field
type NA :: Rule Row Type
type NA f = f (Const Unit)

data X_I_1 :: CA Row Type -> CA Row Type -> Type
data X_I_1 uncond cond
  = NoneDefined
  | SomeDefined (Some uncond) (Record (X cond))

data X_I_2 :: Type -> CA Row Type -> Type
data X_I_2 uncond cond
  = Undefined
  | Defined uncond (Some cond)

data X_I_4 :: CA Row Type -> Type
data X_I_4 cond
  = X_NT
  | X_NTIED
  | X_NTL (Some cond)

data NN_I_1 :: CA Row Type -> Type
data NN_I_1 group
  = None
  | All (Record (NN group))

data NN_I_3 group
  = NN_NT
  | NN_NTL
  | NN_NTIED (Record (NN group))

