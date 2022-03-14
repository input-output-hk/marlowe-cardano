module Capability.Toast
  ( class Toast
  , addToast
  ) where

import Prologue

import AppM (AppM)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Monad.Trans.Class (lift)
import Effect.Class (class MonadEffect)
import Halogen (HalogenM)
import Halogen.Store.Monad (updateStore)
import Store (Action(..))
import Toast.Types (ToastMessage)

-- This class allows any component to trigger a toast notification
class Monad m <= Toast m where
  addToast :: ToastMessage -> m Unit

-- There is nothing pertinent to do inside the AppM, but we need to provide this instance to
-- satisfy the compiler
instance MonadEffect m => Toast (AppM m) where
  addToast = updateStore <<< ShowToast

instance Toast m => Toast (HalogenM state action slots msg m) where
  addToast = lift <<< addToast

instance Toast m => Toast (MaybeT m) where
  addToast = lift <<< addToast

instance Toast m => Toast (ReaderT r m) where
  addToast = lift <<< addToast
