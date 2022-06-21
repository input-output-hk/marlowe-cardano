module AppM where

import Prologue

import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (Component)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, runAndEmitStoreT)
import Halogen.Subscription (Emitter)
import Servant.PureScript (class MonadAjax, request)
import Store as Store
import Types (Env)

newtype AppM a = AppM (ReaderT Env (StoreT Store.Action Store.State Aff) a)

runAppM
  :: forall q i o
   . Env
  -> Store.State
  -> Component q i o AppM
  -> Aff ({ component :: Component q i o Aff, emitter :: Emitter Store.State })
runAppM env store =
  runAndEmitStoreT store Store.reduce <<< H.hoist \(AppM r) -> runReaderT r env

derive newtype instance Functor AppM

derive newtype instance Apply AppM

derive newtype instance Applicative AppM

derive newtype instance Bind AppM

derive newtype instance Monad AppM

derive newtype instance MonadEffect AppM

derive newtype instance MonadAff AppM

derive newtype instance MonadStore Store.Action Store.State AppM

instance MonadAjax api AppM where
  request api r = liftAff $ request api r
