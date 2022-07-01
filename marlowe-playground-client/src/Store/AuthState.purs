module Store.AuthState where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Variant (Variant)
import Data.Variant as Variant
import Network.RemoteData (RemoteData(..))
import Prim.Row as Row
import Record as Record
import Session (AuthResponse)
import Type.Prelude (Proxy(..))

_authStateP = Proxy :: Proxy "authState"

_authState :: forall r. Lens' (State r) AuthState
_authState = prop _authStateP

type AuthState = AuthResponse

insertInitialAuthState
  :: forall r. Row.Lacks "authState" r => { | r } -> { | StateRow r }
insertInitialAuthState = Record.insert _authStateP NotAsked

type StateRow r = (authState :: AuthState | r)
type State r = { | StateRow r }

type ActionRow r = (authState :: AuthState | r)
type Action r = Variant (ActionRow r)

action :: forall acc. AuthState -> Action acc
action = Variant.inj _authStateP

reduce
  :: forall acc st
   . (Variant acc -> State st -> State st)
  -> Action acc
  -> State st
  -> State st
reduce = Variant.on _authStateP $ Record.set _authStateP
