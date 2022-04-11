module Test.Control.Monad.Time
  ( class MonadMockTime
  , MockTimeM
  , MockTimeEnv
  , TestClock
  , runMockTimeM
  , advanceTime
  ) where

import Prelude

import Capability.Marlowe (class ManageMarlowe)
import Capability.PAB (class ManagePAB)
import Capability.PlutusApps.FollowerApp (class FollowerApp)
import Capability.Toast (class Toast)
import Capability.Wallet (class ManageWallet)
import Clipboard (class MonadClipboard)
import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Cont (class MonadCont, class MonadTrans, ContT)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Except (ExceptT)
import Control.Monad.Fork.Class
  ( class MonadBracket
  , class MonadFork
  , class MonadKill
  , bracket
  , kill
  , never
  , uninterruptible
  )
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Now (class MonadTime)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT(..)
  , ask
  , asks
  , local
  , runReaderT
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.UUID (class MonadUUID)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT)
import Data.Array (sortWith)
import Data.Bifunctor (lmap)
import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.Distributive (class Distributive)
import Data.Foldable (for_)
import Data.Lazy (force)
import Data.List as L
import Data.List.Lazy (List)
import Data.List.Lazy as LL
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (class Duration, Minutes)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Aff (Error, error)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Unlift (class MonadUnliftEffect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Subscription as HS
import Servant.PureScript (class MonadAjax)
import Test.Halogen (class MonadHalogenTest)
import Test.Network.HTTP (class MonadMockHTTP, MockHttpM)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)

-- Invariant: The instants are in ascending order
type TestClock = (List (Tuple Instant (Effect Unit)))

type MockTimeEnv =
  { tzOffset :: Minutes
  , nowRef :: Ref Instant
  , clocks :: Ref (Map Int TestClock)
  , nextClockId :: Ref Int
  }

newtype MockTimeM (m :: Type -> Type) a = MockTimeM (ReaderT MockTimeEnv m a)

runMockTimeM :: forall m a. MonadAff m => MockTimeM m a -> MockTimeEnv -> m a
runMockTimeM (MockTimeM r) = runReaderT r

derive newtype instance Functor m => Functor (MockTimeM m)
derive newtype instance Apply m => Apply (MockTimeM m)
derive newtype instance Applicative m => Applicative (MockTimeM m)
derive newtype instance Bind m => Bind (MockTimeM m)
derive newtype instance Monad m => Monad (MockTimeM m)
derive newtype instance MonadEffect m => MonadEffect (MockTimeM m)
derive newtype instance MonadUnliftEffect m => MonadUnliftEffect (MockTimeM m)
derive newtype instance MonadAff m => MonadAff (MockTimeM m)
derive newtype instance MonadUnliftAff m => MonadUnliftAff (MockTimeM m)
derive newtype instance MonadBase b m => MonadBase b (MockTimeM m)
derive newtype instance MonadUnlift b m => MonadUnlift b (MockTimeM m)
derive newtype instance MonadThrow e m => MonadThrow e (MockTimeM m)
derive newtype instance MonadError e m => MonadError e (MockTimeM m)
derive newtype instance MonadTell s m => MonadTell s (MockTimeM m)
derive newtype instance MonadWriter s m => MonadWriter s (MockTimeM m)
derive newtype instance MonadState s m => MonadState s (MockTimeM m)
derive newtype instance MonadCont m => MonadCont (MockTimeM m)
derive newtype instance MonadRec m => MonadRec (MockTimeM m)
derive newtype instance MonadFork f m => MonadFork f (MockTimeM m)
derive newtype instance MonadAjax api m => MonadAjax api (MockTimeM m)
derive newtype instance MonadTest m => MonadTest (MockTimeM m)
derive newtype instance MonadMockHTTP m => MonadMockHTTP (MockTimeM m)
derive newtype instance MonadUUID m => MonadUUID (MockTimeM m)
derive newtype instance MonadLogger l m => MonadLogger l (MockTimeM m)
derive newtype instance MonadStore a s m => MonadStore a s (MockTimeM m)
derive newtype instance ManageWallet m => ManageWallet (MockTimeM m)
derive newtype instance ManagePAB m => ManagePAB (MockTimeM m)
derive newtype instance FollowerApp m => FollowerApp (MockTimeM m)
derive newtype instance Toast m => Toast (MockTimeM m)
derive newtype instance
  ( Monad m
  , MonadClipboard m
  ) =>
  MonadClipboard (MockTimeM m)

derive newtype instance ManageMarlowe m => ManageMarlowe (MockTimeM m)
derive newtype instance
  MonadHalogenTest q i o m =>
  MonadHalogenTest q i o (MockTimeM m)

derive newtype instance MonadUser m => MonadUser (MockTimeM m)

instance (MonadEffect m, MonadThrow Error m) => MonadMockTime (MockTimeM m) where
  advanceTime delta = do
    { nowRef, clocks } <- MockTimeM ask
    now <- liftEffect $ Ref.read nowRef
    targetTime <- case adjust delta $ toDateTime now of
      Nothing -> throwError $ error "Invalid datetime"
      Just targetDateTime -> pure $ fromDateTime targetDateTime
    ticksToRun <- liftEffect $ flip Ref.modify' clocks \clockTable ->
      let
        clockTableSplit = breakClock targetTime <$> clockTable
      in
        { state: snd <$> clockTableSplit
        , value: sortWith fst
            $ L.toUnfoldable
            $ L.mapMaybe (LL.last <<< fst)
            $ Map.values clockTableSplit
        }

    for_ ticksToRun \(Tuple instant tick) -> liftEffect do
      Ref.write instant nowRef
      tick

instance MonadEffect m => MonadTime (MockTimeM m) where
  now = liftEffect <<< Ref.read =<< MockTimeM (asks _.nowRef)
  timezoneOffset = MockTimeM $ asks _.tzOffset
  makeClock interval = do
    { nowRef, clocks, nextClockId } <- MockTimeM $ ask
    liftEffect do
      start <- Ref.read nowRef
      clockId <- Ref.read nextClockId
      clockTable <- Ref.read clocks
      { emitter, listener } <- HS.create
      let
        tick = HS.notify listener unit
        nextTick = map (pair <<< flip Tuple tick <<< fromDateTime)
          <<< adjust interval
          <<< toDateTime
          <<< fst
        pair x = Tuple x x
        clock = unfoldr nextTick (Tuple start tick)
        clockTable' = Map.insert clockId clock clockTable
      Ref.write clockTable' clocks
      Ref.write (clockId + 1) nextClockId
      pure $ HS.makeEmitter \push -> do
        _ <- HS.subscribe emitter push
        pure $ Ref.modify_ (Map.delete clockId) clocks

instance MonadKill e f m => MonadKill e f (MockTimeM m) where
  kill e = lift <<< kill e

instance MonadBracket e f m => MonadBracket e f (MockTimeM m) where
  bracket (MockTimeM acquire) release run = MockTimeM $ bracket
    acquire
    (\c a -> case release c a of MockTimeM r -> r)
    (\a -> case run a of MockTimeM r -> r)
  uninterruptible (MockTimeM r) = MockTimeM $ uninterruptible r
  never = lift never

derive newtype instance Distributive g => Distributive (MockTimeM g)
instance MonadTrans (MockTimeM) where
  lift m = MockTimeM $ lift m

instance MonadAsk r m => MonadAsk r (MockTimeM m) where
  ask = lift ask

instance MonadReader r m => MonadReader r (MockTimeM m) where
  local f (MockTimeM (ReaderT r)) = MockTimeM $ ReaderT $ local f <<< r

class Monad m <= MonadMockTime m where
  advanceTime :: forall d. Duration d => d -> m Unit

instance MonadMockTime m => MonadMockTime (ReaderT r m) where
  advanceTime = lift <<< advanceTime

instance (Monoid w, MonadMockTime m) => MonadMockTime (WriterT w m) where
  advanceTime = lift <<< advanceTime

instance MonadMockTime m => MonadMockTime (StateT s m) where
  advanceTime = lift <<< advanceTime

instance MonadMockTime m => MonadMockTime (ContT r m) where
  advanceTime = lift <<< advanceTime

instance MonadMockTime m => MonadMockTime (ExceptT e m) where
  advanceTime = lift <<< advanceTime

instance MonadMockTime m => MonadMockTime (MaybeT m) where
  advanceTime = lift <<< advanceTime

instance MonadMockTime m => MonadMockTime (MockHttpM m) where
  advanceTime = lift <<< advanceTime

instance (Monoid w, MonadMockTime m) => MonadMockTime (RWST r w s m) where
  advanceTime = lift <<< advanceTime

breakClock :: Instant -> TestClock -> Tuple TestClock TestClock
breakClock pivot (LL.List clock) = case force clock of
  LL.Nil -> Tuple mempty mempty
  LL.Cons (Tuple instant run) clock'
    | instant > pivot -> Tuple mempty (LL.List clock)
    | otherwise -> lmap (LL.cons $ Tuple instant run) $ breakClock pivot clock'
