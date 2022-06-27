module Store.Hooks where

import Prelude

import Contrib.Halogen.Store.UseSelector (UseSelector) as H
import Contrib.Halogen.Store.UseSelector (UseSelector, useSelector)
import Data.DateTime (DateTime)
import Data.Function (on)
import Data.Lens (view)
import Data.Maybe (Maybe(..))
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
import Store (Action, State, _authResponse)

usePossiblyAuthenticated
  :: forall t2 t3 t7
   . MonadStore t7 State t3
  => Hook t3 (UseSelector t7 State AuthResponse <> t2) (Maybe Boolean)
usePossiblyAuthenticated = H.do
  authResponse <- useSelector
    (select (on eq RemoteData.toMaybe) $ view _authResponse)
  H.pure $ A.possiblyAuthenticated <$> authResponse

foreign import data UseIsAuthenticated :: H.HookType

instance
  H.HookNewtype UseIsAuthenticated
    ( H.UseStateFn (Maybe Boolean)
        <> H.UseSelector Action State (Maybe DateTime)
        <> H.UseEffect
        <> H.Pure
    )

useIsAuthenticated
  :: forall m
   . MonadStore Action State m
  => Hook m UseIsAuthenticated (Maybe Boolean)
useIsAuthenticated = H.wrap H.do
  state /\ putState <- usePutState Nothing
  sessionTimeout <- useSelector
    (H.selectEq $ A.sessionTimeout <<< view _authResponse)

  H.captures { sessionTimeout } useTickEffect do
    case join sessionTimeout of
      Just t -> do
        n <- liftEffect nowDateTime
        putState $ Just (n < t)
      Nothing -> pure unit
    pure Nothing
  H.pure state
