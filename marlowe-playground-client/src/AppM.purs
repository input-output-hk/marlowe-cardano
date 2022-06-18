module AppM where

import Prologue

import Control.Monad.Reader (ReaderT, runReaderT)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Servant.PureScript (class MonadAjax)
import Types (Env)

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance Functor AppM

derive newtype instance Apply AppM

derive newtype instance Applicative AppM

derive newtype instance Bind AppM

derive newtype instance Monad AppM

derive newtype instance MonadEffect AppM

derive newtype instance MonadAff AppM

derive newtype instance MonadAjax api AppM
