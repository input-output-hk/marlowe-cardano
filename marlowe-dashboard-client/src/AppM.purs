module AppM (AppM, passphrase, runAppM) where

import Prologue

import Clipboard (class MonadClipboard, copy)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Effect.Class (log') as Control.Monad.Effect.Class
import Control.Monad.Reader
  ( class MonadReader
  , ReaderT
  , mapReaderT
  , runReaderT
  , withReaderT
  )
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec, tailRecM)
import Control.Monad.Trans.Class (lift)
import Data.Array as A
import Data.Lens (Lens', over)
import Data.Lens.Record (prop)
import Data.Maybe (fromJust)
import Data.Newtype (un)
import Data.Passphrase (Passphrase)
import Data.Passphrase as Passphrase
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Env (Env(..))
import Halogen (Component)
import Halogen.Component (hoist)
import Halogen.Store.Monad
  ( class MonadStore
  , StoreT(..)
  , emitSelected
  , getStore
  , runStoreT
  , updateStore
  )
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

newtype AppM a = AppM (ReaderT Env (StoreT Store.Action Store.Store Aff) a)

runAppM
  :: forall q i o
   . Env
  -> Store.Store
  -> Component q i o AppM
  -> Aff (Component q i o Aff)
runAppM env store =
  runStoreT store Store.reduce <<< hoist \(AppM r) -> runReaderT r env

derive newtype instance Functor AppM

derive newtype instance Apply AppM

derive newtype instance Applicative AppM

derive newtype instance Bind AppM

derive newtype instance Monad AppM

derive newtype instance MonadEffect AppM

derive newtype instance MonadAff AppM

-- TODO use newtype deriving when MonadRec instance is added to StoreT
instance MonadRec AppM where
  tailRecM rec a = AppM $ mapReaderT StoreT $ tailRecM rec' a
    where
    rec' = map (mapReaderT unStoreT <<< unAppM) rec
    unAppM (AppM m) = m
    unStoreT (StoreT m) = m

instance MonadStore Store.Action Store.Store AppM where
  getStore = AppM (lift getStore)
  updateStore = AppM <<< lift <<< updateStore
  emitSelected = AppM <<< lift <<< emitSelected

derive newtype instance MonadAsk Env AppM

derive newtype instance MonadReader Env AppM

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

instance MonadAjax PAB.Api AppM where
  request api = liftAff <<< request api <<< handleRequest
    where
    handleRequest = prependPath [ "pab" ]

instance MonadAjax MarloweRun.Api AppM where
  request api = liftAff <<< request api

instance MonadLogger String AppM where
  -- | All other helper functions (debug, error, info, warn) are in `Control.Logger.Capability`
  log m = AppM $ withReaderT (un Env) (Control.Monad.Effect.Class.log' m)

instance MonadClipboard AppM where
  copy = liftEffect <<< copy
