module Store where

import Marlowe (class HasSPSettings, SPSettings_)
import Type.Row (type (+))

type EnvRow :: Row Type -> Row Type
type EnvRow r
  = ( ajaxSettings :: SPSettings_ | r )

-- | Immutable part of the global state. We wrap it in a `newtype`
-- | because we need an instance of `HasSPSettings`.
newtype Env
  = Env { | EnvRow + () }

type StateRow :: Row Type -> Row Type
type StateRow r
  = ( | r )

type Store
  = { | StateRow (EnvRow ()) }

instance hasSPSettingsEnv :: HasSPSettings Env where
  spSettings (Env { ajaxSettings }) = ajaxSettings

toEnv :: Store -> Env
toEnv { ajaxSettings } = Env { ajaxSettings }

data Action

reduce :: forall env. { | StateRow env } -> Action -> { | StateRow env }
reduce s _ = s
