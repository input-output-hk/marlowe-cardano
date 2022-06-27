module Contrib.Halogen.Store.Monad where

import Prelude

import Data.Lens (Lens', over, view)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)

getsStore
  :: forall a m s v
   . Functor m
  => MonadStore a s m
  => (s -> v)
  -> m v
getsStore f = map f getStore

useStore
  :: forall a s m v
   . Functor m
  => MonadStore a s m
  => Lens' s v
  -> m v
useStore l = getsStore (view l)

overStore
  :: forall t17 t18 t19 t20 t21 t22
   . MonadStore (t19 -> t20) t18 t17
  => ((t21 -> t22) -> t19 -> t20)
  -> (t21 -> t22)
  -> t17 Unit
overStore l v = updateStore (over l v)
