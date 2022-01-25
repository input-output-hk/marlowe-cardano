module AppM
  ( AppM
  , passphrase
  , runAppM
  ) where

import Prologue

import Clipboard (class MonadClipboard, copy)
import Control.Monad.Reader (class MonadReader, ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromJust)
import Data.Passpharse (Passphrase)
import Data.Passpharse (fromString) as Passphrase
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Env (Env)
import Halogen (Component)
import Halogen.Component (hoist)
import Halogen.Store.Monad
  ( class MonadStore
  , StoreT
  , emitSelected
  , getStore
  , runStoreT
  , updateStore
  )
import Partial.Unsafe (unsafePartial)
import Store as Store

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

instance MonadStore Store.Action Store.Store AppM where
  getStore = AppM (lift getStore)
  updateStore = AppM <<< lift <<< updateStore
  emitSelected = AppM <<< lift <<< emitSelected

derive newtype instance MonadAsk Env AppM

derive newtype instance MonadReader Env AppM

instance MonadClipboard AppM where
  copy = liftEffect <<< copy

