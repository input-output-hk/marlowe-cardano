module Test.Web where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Reader (ReaderT, asks, mapReaderT, runReaderT)
import Control.Monad.Reader.Class
  ( class MonadAsk
  , class MonadReader
  , ask
  , local
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Distributive (class Distributive)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Test.Web.Monad (class MonadTest)
import Web.DOM (Element)

type TestEnv =
  { container :: Element
  }

newtype TestM (m :: Type -> Type) a = TestM (ReaderT TestEnv m a)

derive instance Functor m => Functor (TestM m)
derive newtype instance Apply m => Apply (TestM m)
derive newtype instance Applicative m => Applicative (TestM m)
derive newtype instance Bind m => Bind (TestM m)
derive newtype instance Alt m => Alt (TestM m)
derive newtype instance Plus m => Plus (TestM m)
derive newtype instance Alternative m => Alternative (TestM m)
derive newtype instance Monad m => Monad (TestM m)
derive newtype instance MonadZero m => MonadZero (TestM m)
derive newtype instance (Apply m, Semigroup a) => Semigroup (TestM m a)
derive newtype instance (Applicative m, Monoid a) => Monoid (TestM m a)
derive newtype instance MonadPlus m => MonadPlus (TestM m)
derive newtype instance MonadTrans (TestM)
derive newtype instance MonadAff m => MonadAff (TestM m)
derive newtype instance MonadEffect m => MonadEffect (TestM m)
derive newtype instance MonadCont m => MonadCont (TestM m)
derive newtype instance MonadThrow e m => MonadThrow e (TestM m)
derive newtype instance MonadError e m => MonadError e (TestM m)
instance MonadAsk r m => MonadAsk r (TestM m) where
  ask = TestM $ lift ask

instance MonadReader r m => MonadReader r (TestM m) where
  local f (TestM r) = TestM $ mapReaderT (local f) r

derive newtype instance MonadState s m => MonadState s (TestM m)
derive newtype instance MonadTell w m => MonadTell w (TestM m)
derive newtype instance MonadWriter w m => MonadWriter w (TestM m)
derive newtype instance Distributive g => Distributive (TestM g)
derive newtype instance MonadRec m => MonadRec (TestM m)

instance MonadAff m => MonadTest (TestM m) where
  getContainer = TestM $ asks _.container
  withContainer container (TestM r) =
    TestM $ local _ { container = container } r

runTestM :: forall m a. Element -> TestM m a -> m a
runTestM container (TestM r) = runReaderT r { container }
