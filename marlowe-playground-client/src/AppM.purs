module AppM where

import Prologue

import Control.Monad.Reader (class MonadReader, ReaderT, runReaderT)
import Control.Monad.Reader.Trans (class MonadAsk)
import Control.Monad.State.Trans (lift)
import Data.Newtype (class Newtype, un)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Env (Env)
import Halogen as H
import Halogen.Component (hoist)
import Halogen.Store.Monad (class MonadStore, StoreT, emitSelected, getStore, runStoreT, updateStore)
import Store (Store)
import Store as Store

newtype AppM a
  = AppM (ReaderT Env (StoreT Store.Action Store Aff) a)

runAppM :: forall i o q. Env -> Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM env store = runStoreT store Store.reduce <<< hoist (flip runReaderT env) <<< hoist (un AppM)

derive instance Newtype (AppM a) _

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadAskAppM :: MonadAsk Env AppM

derive newtype instance monadReaderAppM :: MonadReader Env AppM

instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM where
  getStore = AppM (lift getStore)
  updateStore = AppM <<< lift <<< updateStore
  emitSelected = AppM <<< lift <<< emitSelected
