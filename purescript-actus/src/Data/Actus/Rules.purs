module Data.Actus.Rules where

import Prelude

import Data.Actus.Terms (Terms)
import Data.Const (Const)
import Data.Identity (Identity)
import Data.Maybe (Maybe)
import Data.Some (Some)

-- | A rule type produces a concrete type for a set of terms
type Rule f a = Terms f a -> a

-- | The X rule represents an optional field
type X :: Rule Row Type
type X f = Record (f Maybe)

-- | The NN rule represents a mandatory field
type NN :: Rule Row Type
type NN f = Record (f Identity)

-- | The NA rule represents a non-applicable field
type NA :: Rule Row Type
type NA f = Record (f (Const Unit))

-- | The x(a,1,) rule says that a group of conditional terms are applicable only
-- | when at least one in a group of conditional terms is defined.
data X_I_1 :: Terms Row Type -> Rule Row Type
data X_I_1 uncond cond
  = NoneDefined
  | SomeDefined (Some uncond) (X cond)

-- | The x(a,2,) rule says that if the unconditional terms are defined, then at
-- | least one conditional term must be defined
data X_I_2 :: Terms Row Type -> Rule Row Type
data X_I_2 uncond cond
  = Undefined
  | Defined (NN uncond) (Some cond)

-- | The x(a,4,) rule says that if the interestCalculationBase term is equal to
-- | NTL, then at least one conditional term must be defined
data X_I_4 :: Rule Row Type
data X_I_4 cond
  = X_NT
  | X_NTIED
  | X_NTL (Some cond)

-- | The NN(a,1,) rule says that if one term in the group is defined, then all
-- | terms in the group must be defined
data NN_I_1 :: Rule Row Type
data NN_I_1 group
  = None
  | All (NN group)

-- | The x(a,3,) rule says that if the interestCalculationBase term is equal to
-- | NTIED, then all conditional terms must be defined
data NN_I_3 :: Rule Row Type
data NN_I_3 group
  = NN_NT
  | NN_NTL
  | NN_NTIED (NN group)

