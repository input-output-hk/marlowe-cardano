module Test.Marlowe.Run where

import Prologue

import AppM (runAppM)
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Bind (bindFlipped)
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Effect.Console as Console
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , catchError
  , throwError
  )
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT
  , ask
  , asks
  , runReaderT
  )
import Control.Parallel (parallel, sequential)
import Data.AddressBook as AddressBook
import Data.Array (sortWith)
import Data.Bifunctor (lmap)
import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.Foldable (for_)
import Data.Lazy (force)
import Data.List as L
import Data.List.Lazy (List)
import Data.List.Lazy as LL
import Data.LocalContractNicknames (emptyLocalContractNicknames)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds, Minutes(..), Seconds(..), fromDuration)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket, error, finally, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Env (Env(..), HandleRequest(..), MakeClock(..))
import Halogen.Subscription (Emitter, Listener, makeEmitter)
import Halogen.Subscription as HS
import LocalStorage (Key(..))
import MainFrame.State (mkMainFrame)
import MainFrame.Types as MF
import Marlowe.Time (unixEpoch)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , CombinedWSStreamToServer
  )
import Store (mkStore)
import Test.Control.Monad.Time (class MonadMockTime)
import Test.Halogen (class MonadHalogenTest, runUITest)
import Test.Marlowe.Run.Action.Types (WalletName)
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError
  , RequestMatcherBox
  , boxRequestMatcher
  , unboxRequestMatcher
  )
import Test.Network.HTTP
  ( class MonadMockHTTP
  , RequestBox(..)
  , runRequestMatcher
  )
import Test.Spec (Spec, it)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import WebSocket.Support (FromSocket)

marloweRunTest
  :: String
  -> ( forall m
        . MonadReader Coenv m
       => MonadError Error m
       => MonadHalogenTest MF.Query Unit MF.Msg m
       => MonadMockHTTP m
       => MonadMockTime m
       => m Unit
     )
  -> Spec Unit
marloweRunTest name test = it name $ bracket
  (liftAff mkTestEnv)
  (\(_ /\ coenv /\ _) -> liftEffect $ coenv.dispose)
  \(env /\ coenv /\ errors) -> do
    currentTime <- liftEffect $ Ref.read coenv.currentTime
    let
      store = mkStore currentTime AddressBook.empty emptyLocalContractNicknames
    rootComponent <- runAppM env store mkMainFrame
    errorAVar <- AVar.empty
    sub <- liftEffect
      $ HS.subscribe errors
      $ launchAff_ <<< flip AVar.kill errorAVar
    finally (liftEffect $ HS.unsubscribe sub) do
      sequential ado
        parallel do
          runUITest rootComponent unit (runMarloweTestM test coenv)
          AVar.put unit errorAVar
        parallel $ AVar.take errorAVar
        in unit

newtype MarloweTestM (m :: Type -> Type) a = MarloweTestM (ReaderT Coenv m a)

derive newtype instance Functor m => Functor (MarloweTestM m)
derive newtype instance Apply m => Apply (MarloweTestM m)
derive newtype instance Applicative m => Applicative (MarloweTestM m)
derive newtype instance Bind m => Bind (MarloweTestM m)
instance Monad m => Monad (MarloweTestM m)
derive newtype instance MonadEffect m => MonadEffect (MarloweTestM m)
derive newtype instance MonadAff m => MonadAff (MarloweTestM m)
derive newtype instance MonadThrow e m => MonadThrow e (MarloweTestM m)
derive newtype instance MonadError e m => MonadError e (MarloweTestM m)
derive newtype instance Monad m => MonadAsk Coenv (MarloweTestM m)
derive newtype instance Monad m => MonadReader Coenv (MarloweTestM m)
derive newtype instance MonadTest m => MonadTest (MarloweTestM m)
derive newtype instance MonadUser m => MonadUser (MarloweTestM m)
derive newtype instance
  MonadHalogenTest q i o m =>
  MonadHalogenTest q i o (MarloweTestM m)

instance MonadAff m => MonadMockHTTP (MarloweTestM m) where
  expectRequest responseFormat matcher = do
    fallbackMatcher <- asks _.fallbackMatcher
    liftAff do
      fallback <- AVar.take fallbackMatcher
      AVar.put
        (fallback <> boxRequestMatcher responseFormat matcher)
        fallbackMatcher
  expectNextRequest responseFormat matcher = do
    matchers <- asks _.nextMatchers
    liftAff $ Queue.write matchers $ boxRequestMatcher responseFormat matcher

instance (MonadEffect m, MonadThrow Error m) => MonadMockTime (MarloweTestM m) where
  advanceTime delta = do
    { currentTime, clocks } <- ask
    now <- liftEffect $ Ref.read currentTime
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
            $ bindFlipped (LL.toUnfoldable <<< fst)
            $ Map.values clockTableSplit
        }

    for_ ticksToRun \(Tuple instant tick) -> liftEffect do
      Ref.write instant currentTime
      tick

runMarloweTestM :: forall m a. MarloweTestM m a -> Coenv -> m a
runMarloweTestM (MarloweTestM m) = runReaderT m

mkTestEnv :: Aff (Env /\ Coenv /\ Emitter Error)
mkTestEnv = do
  fallbackMatcher <- liftAff $ AVar.new mempty
  contractStepCarouselSubscription <- liftAff AVar.empty
  endpointSemaphores <- liftAff $ AVar.new Map.empty
  createListeners <- liftAff $ AVar.new Map.empty
  applyInputListeners <- liftAff $ AVar.new Map.empty
  redeemListeners <- liftAff $ AVar.new Map.empty
  followerBus <- liftEffect $ EventBus.create
  pabWebsocketIn <- liftEffect $ HS.create
  pabWebsocketOut <- liftEffect $ HS.create
  errors <- liftEffect $ HS.create
  pabWebsocketOutQueue <- Queue.new
  nextMatchers <- Queue.new
  sub <- liftEffect $ HS.subscribe pabWebsocketOut.emitter \msg ->
    launchAff_ $ Queue.write pabWebsocketOutQueue msg
  localStorage <- liftEffect $ Ref.new Map.empty
  currentTime <- liftEffect $ Ref.new unixEpoch
  nextClockId <- liftEffect $ Ref.new 0
  clocks <- liftEffect $ Ref.new Map.empty
  let
    sources =
      { pabWebsocket: pabWebsocketIn.emitter
      , currentTime: Ref.read currentTime
      }
    sinks = { pabWebsocket: pabWebsocketOut.listener }

    marshallErrors
      :: forall m a. MonadError Error m => MonadEffect m => m a -> m a
    marshallErrors m = catchError m \e -> do
      liftEffect $ HS.notify errors.listener e
      throwError e
    env = Env
      { contractStepCarouselSubscription
      , logger: Console.structuredLogger
      , endpointSemaphores
      , createListeners
      , applyInputListeners
      , redeemListeners
      , followerBus
      , sinks
      , sources
      , handleRequest: HandleRequest \request -> marshallErrors do
          nextMatcher <- Queue.tryRead nextMatchers
          fallbackMatcher' <- AVar.read fallbackMatcher
          let
            matcher = fromMaybe fallbackMatcher' nextMatcher

          case unboxRequestMatcher matcher request of
            Left error -> failMultiline error
            Right response -> pure response
      , localStorage:
          { getItem: \(Key key) -> Map.lookup key <$> Ref.read localStorage
          , setItem: \(Key key) item ->
              Ref.modify_ (Map.insert key item) localStorage
          , removeItem: \(Key key) -> Ref.modify_ (Map.delete key) localStorage
          }
      , timezoneOffset: Minutes zero
      , makeClock:
          MakeClock (makeClock currentTime nextClockId clocks <<< fromDuration)
      , regularPollInterval: fromDuration $ Seconds 5.0
      , syncPollInterval: fromDuration $ Seconds 0.5
      }
    coenv =
      { localStorage
      , currentTime
      , clocks
      , pabWebsocketIn: pabWebsocketIn.listener
      , pabWebsocketOut: pabWebsocketOutQueue
      , nextMatchers
      , fallbackMatcher
      , dispose: do
          HS.unsubscribe sub
      }
  pure $ env /\ coenv /\ errors.emitter

-- Like `Env` but we invert the control of everything in it (i.e. all sources
-- become sinks, sinks sources).
type Coenv =
  { pabWebsocketOut :: Queue CombinedWSStreamToServer
  , fallbackMatcher :: AVar RequestMatcherBox
  , nextMatchers :: Queue RequestMatcherBox
  , pabWebsocketIn :: Listener (FromSocket CombinedWSStreamToClient)
  , currentTime :: Ref Instant
  , clocks :: Ref (Map Int TestClock)
  , localStorage :: Ref (Map String String)
  , dispose :: Effect Unit
  }

-- Invariant: The instants are in ascending order
type TestClock = (List (Tuple Instant (Effect Unit)))

failMultiline :: forall m a. MonadThrow Error m => MatcherError -> m a
failMultiline = throwError <<< error <<< show

makeClock
  :: Ref Instant
  -> Ref Int
  -> Ref (Map Int TestClock)
  -> Milliseconds
  -> Effect (Emitter Unit)
makeClock currentTime nextClockId clocks interval = do
  start <- Ref.read currentTime
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
  pure $ makeEmitter \push -> do
    _ <- HS.subscribe emitter push
    pure $ Ref.modify_ (Map.delete clockId) clocks

breakClock :: Instant -> TestClock -> Tuple TestClock TestClock
breakClock pivot (LL.List clock) = case force clock of
  LL.Nil -> Tuple mempty mempty
  LL.Cons (Tuple instant run) clock'
    | instant > pivot -> Tuple mempty (LL.List clock)
    | otherwise -> lmap (LL.cons $ Tuple instant run) $ breakClock pivot clock'
