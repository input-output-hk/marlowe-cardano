module Test.Control.Monad.UUID where

import Prologue

import Control.Monad.Base (class MonadBase)
import Control.Monad.Cont (class MonadTrans)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Except (ExceptT)
import Control.Monad.Fork.Class (class MonadFork, class MonadKill, kill)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.UUID (class MonadUUID)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (WriterT)
import Data.UUID.Argonaut (UUID, parseUUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.Control.Monad.Time (MockTimeM)
import Test.Halogen (class MonadHalogenTest)
import Test.Network.HTTP (class MonadMockHTTP, MockHttpM)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)

class Monad m <= MonadMockUUID m where
  -- We are only storing a single UUID, if an action fires multiple requests, we
  -- might want to change this to receive an array.
  setNextUUID :: UUID -> m Unit

instance MonadMockUUID m => MonadMockUUID (ReaderT r m) where
  setNextUUID = lift <<< setNextUUID

instance (Monoid w, MonadMockUUID m) => MonadMockUUID (WriterT w m) where
  setNextUUID = lift <<< setNextUUID

instance MonadMockUUID m => MonadMockUUID (ExceptT e m) where
  setNextUUID = lift <<< setNextUUID

instance MonadEffect m => MonadMockUUID (MockUuidM m) where
  setNextUUID uuid = do
    uuidRef <- MockUuidM $ asks _.uuidRef
    liftEffect $ Ref.write (Just uuid) uuidRef

instance MonadMockUUID m => MonadMockUUID (MockHttpM m) where
  setNextUUID = lift <<< setNextUUID

instance MonadMockUUID m => MonadMockUUID (MockTimeM m) where
  setNextUUID = lift <<< setNextUUID

type MockUuidEnv =
  { uuidRef :: Ref (Maybe UUID)
  }

newtype MockUuidM (m :: Type -> Type) a = MockUuidM (ReaderT MockUuidEnv m a)

runMockUuidM :: forall m a. MockUuidM m a -> MockUuidEnv -> m a
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
    uuidRef <- MockUuidM $ asks _.uuidRef
    mReqId <- liftEffect $ Ref.read uuidRef
    case mReqId of
      Nothing -> throwError $ error
        "A UUID was asked to be created but no mock was found"
      Just reqId -> do
        liftEffect $ Ref.write Nothing uuidRef
        pure reqId

mkTestUUID
  :: forall m. MonadThrow Error m => MonadMockUUID m => String -> m UUID
mkTestUUID str =
  case parseUUID str of
    Just uuid -> do
      setNextUUID uuid
      pure uuid
    Nothing -> throwError $ error $ "Can't parse <" <> str <> "> as a UUID"

