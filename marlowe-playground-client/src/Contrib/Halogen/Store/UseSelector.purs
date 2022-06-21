module Contrib.Halogen.Store.UseSelector where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Halogen.Hooks (class HookNewtype, type (<>), Hook, UseEffect, UseState)
import Halogen.Hooks as Hooks
import Halogen.Store.Monad (class MonadStore, emitSelected, getStore)
import Halogen.Store.Select (Selector(..))

foreign import data UseSelector :: Type -> Type -> Type -> Hooks.HookType

type UseSelector' :: Type -> Type -> Type -> Hooks.HookType
type UseSelector' act store ctx = UseState (Maybe ctx) <> UseEffect <>
  Hooks.Pure

instance HookNewtype (UseSelector act store ctx) (UseSelector' act store ctx)

useSelector
  :: forall m act store ctx
   . MonadStore act store m
  => Selector store ctx
  -> Hook m (UseSelector act store ctx) (Maybe ctx)
useSelector (Selector selector) = Hooks.wrap hook
  where
  hook :: Hook m (UseSelector' act store ctx) (Maybe ctx)
  hook = Hooks.do
    ctx /\ ctxId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      -- Read the initial value from the store because the emitter
      -- won't notify us about it.
      store <- getStore
      Hooks.put ctxId (Just $ selector.select store)

      emitter <- emitSelected (Selector selector)
      subscriptionId <- Hooks.subscribe $ map (Hooks.put ctxId <<< Just) emitter
      pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.pure ctx
