module AppM (AppM, passphrase, runAppM) where

import Prologue

import Clipboard (class MonadClipboard, copy)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Effect.Class (log') as Control.Monad.Effect.Class
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (class MonadError)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader
  ( class MonadReader
  , ReaderT
  , runReaderT
  , withReaderT
  )
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Array as A
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (fromJust)
import Data.Newtype (un)
import Data.Passphrase (Passphrase)
import Data.Passphrase as Passphrase
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Env (Env(..))
import Halogen (Component)
import Halogen.Component (hoist)
import Halogen.Store.Monad (class MonadStore, StoreT, runAndEmitStoreT)
import Halogen.Subscription (Emitter)
import Marlowe.Run.Server as MarloweRun
import Partial.Unsafe (unsafePartial)
import Plutus.PAB.Webserver as PAB
import Servant.PureScript (class MonadAjax, Request, request)
import Store as Store
import Type.Proxy (Proxy(..))
import URI (Fragment, Host, Path, RelativeRef, UserInfo)
import URI.Extra.QueryPairs (Key, QueryPairs, Value)
import URI.RelativePart (_relPath)
import URI.RelativeRef (_relPart)

-- | We want to have this constant on the module level so
-- | it just blows up if we change passphrase validation.
passphrase :: Passphrase
passphrase = unsafePartial $ fromJust $ Passphrase.fromString
  "fixme-allow-pass-per-wallet"

newtype AppM m a = AppM (ReaderT Env (StoreT Store.Action Store.Store m) a)

runAppM
  :: forall q i o m
   . Monad m
  => Env
  -> Store.Store
  -> Component q i o (AppM m)
  -> Aff ({ component :: Component q i o m, emitter :: Emitter Store.Store })
runAppM env store =
  runAndEmitStoreT store Store.reduce <<< hoist \(AppM r) -> runReaderT r env

derive newtype instance Functor m => Functor (AppM m)

derive newtype instance Apply m => Apply (AppM m)

derive newtype instance Applicative m => Applicative (AppM m)

derive newtype instance Bind m => Bind (AppM m)

derive newtype instance Monad m => Monad (AppM m)

derive newtype instance MonadThrow e m => MonadThrow e (AppM m)

derive newtype instance MonadError e m => MonadError e (AppM m)

derive newtype instance MonadEffect m => MonadEffect (AppM m)

derive newtype instance MonadAff m => MonadAff (AppM m)

derive newtype instance MonadRec m => MonadRec (AppM m)

instance MonadTrans AppM where
  lift = AppM <<< lift <<< lift

derive newtype instance Monad m => MonadAsk Env (AppM m)

derive newtype instance Monad m => MonadReader Env (AppM m)

derive newtype instance
  MonadEffect m =>
  MonadStore Store.Action Store.Store (AppM m)

derive newtype instance MonadTime m => MonadTime (AppM m)

-- TODO move to servant-support and add more lenses
_uri
  :: forall reqContent resContent decodeError req res
   . Lens'
       (Request reqContent resContent decodeError req res)
       ( RelativeRef
           UserInfo
           Host
           Path
           (Array String)
           (QueryPairs Key Value)
           Fragment
       )
_uri = prop (Proxy :: _ "uri")

prependPath
  :: forall reqContent resContent decodeError req res
   . Array String
  -> Request reqContent resContent decodeError req res
  -> Request reqContent resContent decodeError req res
prependPath prefix =
  over
    (_uri <<< _relPart <<< _relPath)
    (Just <<< append prefix <<< join <<< A.fromFoldable)

instance MonadAjax PAB.Api m => MonadAjax PAB.Api (AppM m) where
  request api = lift <<< request api <<< prependPath [ "pab" ]

instance MonadAjax MarloweRun.Api m => MonadAjax MarloweRun.Api (AppM m) where
  request api = lift <<< request api

instance MonadEffect m => MonadLogger StructuredLog (AppM m) where
  -- | All other helper functions (debug, error, info, warn) are in `Control.Logger.Capability`
  log m = AppM $ withReaderT (un Env) (Control.Monad.Effect.Class.log' m)

instance MonadEffect m => MonadClipboard (AppM m) where
  copy = liftEffect <<< copy
