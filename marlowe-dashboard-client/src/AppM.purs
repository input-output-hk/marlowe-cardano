module AppM (AppM, passphrase, runAppM) where

import Prologue

import Clipboard (class MonadClipboard, copy)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Effect.Class (log') as Control.Monad.Effect.Class
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Base (class MonadBase, liftBase)
import Control.Monad.Error.Class (class MonadThrow, try)
import Control.Monad.Error.Extra (toMonadThrow)
import Control.Monad.Except (class MonadError)
import Control.Monad.Fork.Class
  ( class MonadBracket
  , class MonadFork
  , class MonadKill
  , bracket
  , kill
  , never
  , uninterruptible
  )
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader
  ( class MonadReader
  , ReaderT(..)
  , runReaderT
  , withReaderT
  )
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Unlift (class MonadUnlift, withRunInBase)
import Data.Array as A
import Data.Lens (Lens', over, view)
import Data.Lens.Record (prop)
import Data.Maybe (fromJust)
import Data.Passphrase (Passphrase)
import Data.Passphrase as Passphrase
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff, withRunInAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unlift (class MonadUnliftEffect, withRunInEffect)
import Env (Env, _sinks)
import Halogen (Component)
import Halogen.Component (hoist)
import Halogen.Store.Monad
  ( class MonadStore
  , HalogenStore
  , StoreT(..)
  , runAndEmitStoreT
  )
import Halogen.Subscription (Emitter)
import Marlowe.Run.Server as MarloweRun
import Partial.Unsafe (unsafePartial)
import Plutus.PAB.Webserver as PAB
import Safe.Coerce (coerce)
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

derive newtype instance MonadFork f m => MonadFork f (AppM m)

instance MonadKill f e m => MonadKill f e (AppM m) where
  kill = map (map lift) kill

instance MonadBracket f e m => MonadBracket f e (AppM m) where
  bracket acquire release =
    AppM
      <<< bracket (coerce acquire) (map (map coerce) release)
      <<< map coerce
  uninterruptible = AppM <<< uninterruptible <<< coerce
  never = AppM never

-- TODO add instances of these classes to StoreT so we can use newtype
-- deriving.
instance MonadUnliftEffect m => MonadUnliftEffect (AppM m) where
  withRunInEffect action = AppM $ ReaderT \env ->
    StoreT $ withRunInEffect \run -> action (run <<< runAppM' env)
    where
    runAppM'
      :: forall a n
       . Env
      -> AppM n a
      -> ReaderT (HalogenStore Store.Action Store.Store) n a
    runAppM' env (AppM r) = case runReaderT r env of
      StoreT storeReader -> storeReader

instance MonadUnliftAff m => MonadUnliftAff (AppM m) where
  withRunInAff action = AppM $ ReaderT \env ->
    StoreT $ withRunInAff \run -> action (run <<< runAppM' env)
    where
    runAppM'
      :: forall a n
       . Env
      -> AppM n a
      -> ReaderT (HalogenStore Store.Action Store.Store) n a
    runAppM' env (AppM r) = case runReaderT r env of
      StoreT storeReader -> storeReader

instance MonadBase b m => MonadBase b (AppM m) where
  liftBase = lift <<< liftBase

instance MonadUnlift b m => MonadUnlift b (AppM m) where
  withRunInBase action = AppM $ ReaderT \env ->
    StoreT $ withRunInBase \run -> action (run <<< runAppM' env)
    where
    runAppM'
      :: forall a n
       . Env
      -> AppM n a
      -> ReaderT (HalogenStore Store.Action Store.Store) n a
    runAppM' env (AppM r) = case runReaderT r env of
      StoreT storeReader -> storeReader

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

-- Errors thrown from base monads can behave in unexpected way if the
-- transformed monad shares an error type. This fixes that behaviour by
-- catching errors in the base monad, lifting the result, and rethrowing in the
-- parent monad.
liftAndRethrow
  :: forall t m e a
   . MonadError e m
  => MonadThrow e (t m)
  => MonadTrans t
  => m a
  -> t m a
liftAndRethrow m = toMonadThrow =<< lift (try m)

instance (MonadError e m, MonadAjax PAB.Api m) => MonadAjax PAB.Api (AppM m) where
  request api = liftAndRethrow <<< request api <<< prependPath [ "pab" ]

instance
  ( MonadError e m
  , MonadAjax MarloweRun.Api m
  ) =>
  MonadAjax MarloweRun.Api (AppM m) where
  request api = liftAndRethrow <<< request api

instance MonadEffect m => MonadLogger StructuredLog (AppM m) where
  -- | All other helper functions (debug, error, info, warn) are in `Control.Logger.Capability`
  log m = AppM $ withReaderT (view _sinks) (Control.Monad.Effect.Class.log' m)

instance MonadEffect m => MonadClipboard (AppM m) where
  copy = liftEffect <<< copy
