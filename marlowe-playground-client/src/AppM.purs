module AppM where

import Prologue

import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Servant.PureScript (class MonadAjax)

newtype AppM a = AppM (Aff a)

runAppM :: AppM ~> Aff
runAppM (AppM m) = m

derive newtype instance Functor AppM

derive newtype instance Apply AppM

derive newtype instance Applicative AppM

derive newtype instance Bind AppM

derive newtype instance Monad AppM

derive newtype instance MonadEffect AppM

derive newtype instance MonadAff AppM

derive newtype instance MonadAjax api AppM
