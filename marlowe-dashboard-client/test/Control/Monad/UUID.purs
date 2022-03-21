module Test.Control.Monad.UUID where

import Prelude

import Control.Monad.Base (class MonadBase)
import Control.Monad.Cont (class MonadTrans)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class (class MonadFork, class MonadKill, kill)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (ReaderT, asks, runReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.UUID (class MonadUUID)
import Control.Monad.Unlift (class MonadUnlift)
import Data.Formatter.Internal (repeat)
import Data.String.CodeUnits (length)
import Data.UUID.Argonaut (UUID(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Test.Halogen (class MonadHalogenTest)
import Test.Network.HTTP (class MonadMockHTTP)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Unsafe.Coerce (unsafeCoerce)

type MockUuidEnv =
  { uuidRef :: Ref Int
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

instance MonadEffect m => MonadUUID (MockUuidM m) where
  generateUUID = do
    uuidRef <- MockUuidM $ asks _.uuidRef
    reqId <- liftEffect $ Ref.read uuidRef
    liftEffect $ Ref.modify_ (_ + 1) uuidRef
    pure $ mockUUID reqId

padStart :: Int -> Int -> String
padStart size n = prefix <> show n
  where
  nSize = length $ show n
  prefix = repeat "0" (size - nSize)

mockUUID :: Int -> UUID
mockUUID n = UUID $ unsafeCoerce $ "00000000-0000-0000-0000-" <> padStart 12 n
