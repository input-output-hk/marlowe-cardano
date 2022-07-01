module Store.AuthState.Hooks where

import Prelude

import Contrib.Halogen.Store.UseSelector (UseSelector) as H
import Contrib.Halogen.Store.UseSelector (UseSelector, useSelector)
import Data.DateTime (DateTime)
import Data.Function (on)
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Now (nowDateTime)
import Halogen.Hooks (type (<>), Hook, useTickEffect)
import Halogen.Hooks (HookType, UseEffect, captures, wrap) as H
import Halogen.Hooks.Extra.Hooks (UseStateFn) as H
import Halogen.Hooks.Extra.Hooks (usePutState)
import Halogen.Hooks.Hook (class HookNewtype, Pure, bind, discard, pure) as H
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (select)
import Halogen.Store.Select (selectEq) as H
import Network.RemoteData as RemoteData
import Session (AuthResponse)
import Session as A
import Store.AuthState (State, _authState)

usePossiblyAuthenticated
  :: forall t2 t3 t7 r state
   . Newtype state (State r)
  => MonadStore t7 state t3
  => Hook t3 (UseSelector t7 state AuthResponse <> t2) (Maybe Boolean)
usePossiblyAuthenticated = H.do
  let
    selectorLens :: Lens' state AuthResponse
    selectorLens = _authState >>> _Newtype
  authResponse <- useSelector
    (select (on eq RemoteData.toMaybe) $ view selectorLens)
  H.pure $ A.possiblyAuthenticated <$> authResponse

foreign import data UseIsAuthenticated :: Type -> Type -> H.HookType

instance
  H.HookNewtype
    (UseIsAuthenticated action state)
    ( H.UseStateFn (Maybe Boolean)
        <> H.UseSelector action state (Maybe DateTime)
        <> H.UseEffect
        <> H.Pure
    )

useIsAuthenticated
  :: forall action m r state
   . Newtype state (State r)
  => MonadStore action state m
  => Hook m (UseIsAuthenticated action state) (Maybe Boolean)
useIsAuthenticated = H.wrap H.do
  let
    selectorLens :: Lens' state AuthResponse
    selectorLens = _authState >>> _Newtype
  state /\ putState <- usePutState Nothing
  sessionTimeout <- useSelector
    (H.selectEq $ A.sessionTimeout <<< view selectorLens)

  H.captures { sessionTimeout } useTickEffect do
    case join sessionTimeout of
      Just t -> do
        n <- liftEffect nowDateTime
        putState $ Just (n < t)
      Nothing -> pure unit
    pure Nothing
  H.pure state
