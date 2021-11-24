module Wallet.Yoroi.Internal
  ( getDappConector
  , enable
  , DappConector
  , WalletAPI
  ) where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)

foreign import data DappConector :: Type

foreign import data WalletAPI :: Type

foreign import getDappConector_ :: EffectFn3 (forall x. x -> Maybe x) (forall x. Maybe x) String (Maybe DappConector)

getDappConector :: String -> Effect (Maybe DappConector)
getDappConector = runEffectFn3 getDappConector_ Just Nothing

foreign import enable_ :: EffectFn1 DappConector (Promise WalletAPI)

enable :: DappConector -> Aff WalletAPI
enable = toAffE <<< runEffectFn1 enable_
