module Test.Control.Monad.UUID where

import Prologue

import Control.Monad.Base (class MonadBase)
import Control.Monad.Cont (class MonadTrans)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Fork.Class (class MonadFork, class MonadKill, kill)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.UUID (class MonadUUID)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (WriterT)
import Data.Enum (succ)
import Data.Maybe (fromMaybe)
import Data.UUID.Argonaut (UUID)
import Data.UniqueIdentifier (UniqueIdentifier, toUUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.Control.Monad.Time (MockTimeM)
import Test.Halogen (class MonadHalogenTest)
import Test.Network.HTTP (class MonadMockHTTP, MockHttpM)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)

class Monad m <= MonadMockUUID m where
  -- Get the last UUID generated without incrementing the counter.
  getLastUUID :: m UUID
  -- Get the new UUID to be generated without incrementing the counter.
  getNextUUID :: m UUID

instance MonadMockUUID m => MonadMockUUID (ReaderT r m) where
  getLastUUID = lift getLastUUID
  getNextUUID = lift getNextUUID

instance (Monoid w, MonadMockUUID m) => MonadMockUUID (WriterT w m) where
  getLastUUID = lift getLastUUID
  getNextUUID = lift getNextUUID

instance MonadMockUUID m => MonadMockUUID (ExceptT e m) where
  getLastUUID = lift getLastUUID
  getNextUUID = lift getNextUUID

instance MonadMockUUID m => MonadMockUUID (MockHttpM m) where
  getLastUUID = lift getLastUUID
  getNextUUID = lift getNextUUID

instance MonadMockUUID m => MonadMockUUID (MockTimeM m) where
  getLastUUID = lift getLastUUID
  getNextUUID = lift getNextUUID

instance MonadEffect m => MonadMockUUID (MockUuidM m) where
  getNextUUID = do
    uuidRef <- MockUuidM ask
    liftEffect $ toUUID <$> Ref.read uuidRef
  getLastUUID = do
    uuidRef <- MockUuidM ask
    uuid <- liftEffect $ Ref.read uuidRef
    pure $ toUUID $ fromMaybe bottom $ succ uuid

newtype MockUuidM (m :: Type -> Type) a =
  MockUuidM (ReaderT (Ref UniqueIdentifier) m a)

runMockUuidM :: forall m a. MockUuidM m a -> Ref UniqueIdentifier -> m a
runMockUuidM (MockUuidM r) = runReaderT r

derive newtype instance Functor m => Functor (MockUuidM m)
derive newtype instance Apply m => Apply (MockUuidM m)
derive newtype instance Applicative m => Applicative (MockUuidM m)
derive newtype instance Bind m => Bind (MockUuidM m)
derive newtype instance Monad m => Monad (MockUuidM m)
derive newtype instance MonadEffect m => MonadEffect (MockUuidM m)
derive newtype instance MonadAff m => MonadAff (MockUuidM m)
derive newtype instance MonadUnliftAff m => MonadUnliftAff (MockUuidM m)
derive newtype instance MonadBase b m => MonadBase b (MockUuidM m)
derive newtype instance MonadUnlift b m => MonadUnlift b (MockUuidM m)
derive newtype instance MonadThrow e m => MonadThrow e (MockUuidM m)
derive newtype instance MonadError e m => MonadError e (MockUuidM m)
derive newtype instance MonadRec m => MonadRec (MockUuidM m)
derive newtype instance MonadFork f m => MonadFork f (MockUuidM m)
derive newtype instance MonadTest m => MonadTest (MockUuidM m)
derive newtype instance MonadMockHTTP m => MonadMockHTTP (MockUuidM m)
derive newtype instance MonadTime m => MonadTime (MockUuidM m)

instance MonadTrans (MockUuidM) where
  lift m = MockUuidM $ lift m

instance MonadKill e f m => MonadKill e f (MockUuidM m) where
  kill e = lift <<< kill e

derive newtype instance
  MonadHalogenTest q i o m =>
  MonadHalogenTest q i o (MockUuidM m)

derive newtype instance MonadUser m => MonadUser (MockUuidM m)

instance (MonadEffect m, MonadError Error m) => MonadUUID (MockUuidM m) where
  generateUUID = do
    uuidRef <- MockUuidM ask
    liftEffect do
      uuid <- liftEffect $ Ref.read uuidRef
      liftEffect $ Ref.write (fromMaybe bottom $ succ uuid) uuidRef
      pure $ toUUID uuid
