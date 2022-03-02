module Control.Monad.Now where

import Prologue

import Control.Alt (class Alt)
import Control.Alternative (class Alternative, empty)
import Control.Lazy (class Lazy)
import Control.Monad.Cont (ContT)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader)
import Control.Monad.Rec.Class (class MonadRec, forever)
import Control.Monad.State (StateT, gets)
import Control.Monad.State.Class (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.Plus (class Plus)
import Data.Date (Date)
import Data.DateTime (DateTime, date, time)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Newtype (class Newtype)
import Data.Time (Time)
import Data.Time.Duration (class Duration, Minutes, fromDuration)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Now as EN
import Halogen (HalogenM)
import Halogen.Store.Monad (StoreT(..))
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS

-- | A class for monads that can read information about the current time.
class Monad m <= MonadTime m where
  -- | Gets an `Instant` value for the date and time according to the current
  -- | machine’s clock.
  now :: m Instant
  -- | Gets the time zone difference, in minutes,
  -- | from current local time (host system settings) to UTC using `now`.
  timezoneOffset :: m Minutes
  -- | Make an emitter that will emit the current time on an interval specified
  -- | by the given duration.
  makeClock :: forall d. Duration d => d -> m (Emitter Instant)

-- | Gets a `DateTime` value for the date and time according to the current
-- | machine’s clock.
nowDateTime :: forall m. MonadTime m => m DateTime
nowDateTime = toDateTime <$> now

-- | Gets the date according to the current machine’s clock.
nowDate :: forall m. MonadTime m => m Date
nowDate = date <<< toDateTime <$> now

-- | Gets the time according to the current machine’s clock.
nowTime :: forall m. MonadTime m => m Time
nowTime = time <<< toDateTime <$> now

instance MonadTime Effect where
  now = EN.now
  timezoneOffset = EN.getTimezoneOffset
  makeClock d = pure $ HS.makeEmitter \push -> do
    launchAff_ $ forever do
      liftEffect $ push =<< now
      delay $ fromDuration d
    pure $ pure unit

instance MonadTime Aff where
  now = liftEffect now
  timezoneOffset = liftEffect timezoneOffset
  makeClock = liftEffect <<< makeClock

instance MonadTime m => MonadTime (HalogenM state action slots msg m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance MonadTime m => MonadTime (StoreT action state m) where
  now = StoreT $ lift now
  timezoneOffset = StoreT $ lift timezoneOffset
  makeClock = StoreT <<< lift <<< makeClock

instance MonadTime m => MonadTime (ReaderT r m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance (Monoid w, MonadTime m) => MonadTime (WriterT w m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance MonadTime m => MonadTime (StateT s m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance MonadTime m => MonadTime (ContT r m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance MonadTime m => MonadTime (ExceptT e m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance MonadTime m => MonadTime (MaybeT m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

instance (Monoid w, MonadTime m) => MonadTime (RWST r w s m) where
  now = lift now
  timezoneOffset = lift timezoneOffset
  makeClock = lift <<< makeClock

-- | A type for faking the current time.
newtype FakeTime m a = FakeTime
  (StateT { now :: Instant, timezoneOffset :: Minutes } m a)

derive instance Newtype (FakeTime m a) _
derive newtype instance Functor m => Functor (FakeTime m)
derive newtype instance Monad m => Apply (FakeTime m)
derive newtype instance Monad m => Applicative (FakeTime m)
derive newtype instance (Monad m, Alt m) => Alt (FakeTime m)
derive newtype instance (Monad m, Plus m) => Plus (FakeTime m)
instance (Monad m, Alternative m) => Alternative (FakeTime m)
derive newtype instance Monad m => Bind (FakeTime m)
instance Monad m => Monad (FakeTime m)
instance MonadPlus m => MonadPlus (FakeTime m)
derive newtype instance MonadRec m => MonadRec (FakeTime m)
derive newtype instance MonadTrans (FakeTime)
derive newtype instance Lazy (FakeTime m a)
derive newtype instance MonadEffect m => MonadEffect (FakeTime m)
derive newtype instance MonadCont m => MonadCont (FakeTime m)
derive newtype instance MonadThrow e m => MonadThrow e (FakeTime m)
derive newtype instance MonadError e m => MonadError e (FakeTime m)
derive newtype instance MonadAsk r m => MonadAsk r (FakeTime m)
derive newtype instance MonadReader r m => MonadReader r (FakeTime m)
derive newtype instance MonadTell w m => MonadTell w (FakeTime m)
derive newtype instance MonadWriter w m => MonadWriter w (FakeTime m)
derive newtype instance (Monad m, Semigroup a) => Semigroup (FakeTime m a)
derive newtype instance (Monad m, Monoid a) => Monoid (FakeTime m a)

instance MonadState s m => MonadState s (FakeTime m) where
  state = lift <<< state

instance Monad m => MonadTime (FakeTime m) where
  now = FakeTime $ gets _.now
  timezoneOffset = FakeTime $ gets _.timezoneOffset
  makeClock _ = pure empty
