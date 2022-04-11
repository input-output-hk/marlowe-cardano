module Control.Monad.UUID where

import Prologue

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Reader (ReaderT)
import Data.UUID.Argonaut (UUID, genUUID)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen.Store.Monad (StoreT(..))

class Monad m <= MonadUUID m where
  generateUUID :: m UUID

instance MonadUUID Effect where
  generateUUID = genUUID

instance MonadUUID Aff where
  generateUUID = liftEffect generateUUID

instance MonadUUID m => MonadUUID (ReaderT r m) where
  generateUUID = lift generateUUID

instance MonadUUID m => MonadUUID (StoreT action state m) where
  generateUUID = StoreT $ lift generateUUID
