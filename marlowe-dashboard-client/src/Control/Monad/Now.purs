module Control.Monad.Now where

import Prologue

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Date (Date)
import Data.DateTime (DateTime, date, time)
import Data.DateTime.Instant (Instant, toDateTime)
import Data.Time (Time)
import Data.Time.Duration (class Duration, Minutes, fromDuration)
import Effect (Effect)
import Effect.Aff (Aff, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now as EN
import Effect.Ref as Ref
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
  -- | Make an emitter that will emit a signal on the specified interval.
  -- | The clock will emit a signal as soon as a new subscription is created,
  -- | and every d time thereafter, until it is unsubscribed.
  makeClock :: forall d. Duration d => d -> m (Emitter Unit)

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
    cancelled <- Ref.new false
    let
      loop = unlessM (liftEffect $ Ref.read cancelled) do
        liftEffect $ push unit
        delay $ fromDuration d
        loop
    launchAff_ loop
    pure do
      Ref.write true cancelled

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
