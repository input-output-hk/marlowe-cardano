-- These are FFI bindings to the injected API of the Nami wallet. As it's stated in their site, it mostly
-- follows the CIP-0030 which is still in active development. There are some differences with the API and
-- changes are expetect to happen.
module Wallet.Nami.Internal
  ( getNamiAPI
  , enable
  , NamiAPI
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import data NamiAPI :: Type

foreign import getNamiAPI_ :: EffectFn2 (forall x. x -> Maybe x) (forall x. Maybe x) (Maybe NamiAPI)

getNamiAPI :: Effect (Maybe NamiAPI)
getNamiAPI = runEffectFn2 getNamiAPI_ Just Nothing

foreign import enable_ :: EffectFn1 NamiAPI (Promise Boolean)

enable :: NamiAPI -> Aff Boolean
enable = toAffE <<< runEffectFn1 enable_
