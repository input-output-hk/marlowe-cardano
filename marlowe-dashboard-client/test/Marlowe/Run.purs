module Test.Marlowe.Run where

import Prologue

import AppM (runAppM)
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
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
  , asks
  , runReaderT
  )
import Control.Parallel (parallel, sequential)
import Data.AddressBook as AddressBook
import Data.DateTime.Instant (Instant)
import Data.LocalContractNicknames (emptyLocalContractNicknames)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket, error, finally, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Env (Env(..), HandleRequest(..))
import Halogen.Subscription (Emitter, Listener)
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
import Test.Halogen (class MonadHalogenTest, runUITest)
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError
  , RequestMatcherBox
  , boxRequestMatcher
  , fallbackMatcher
  , unboxRequestMatcher
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
    matchers <- asks _.requestMatchers
    liftAff $ Queue.write matchers $ boxRequestMatcher responseFormat matcher

runMarloweTestM :: forall m a. MarloweTestM m a -> Coenv -> m a
runMarloweTestM (MarloweTestM m) = runReaderT m

mkTestEnv :: Aff (Env /\ Coenv /\ Emitter Error)
mkTestEnv = do
  contractStepCarouselSubscription <- liftAff AVar.empty
  endpointSemaphores <- liftAff $ AVar.new Map.empty
  createListeners <- liftAff $ AVar.new Map.empty
  applyInputListeners <- liftAff $ AVar.new Map.empty
  redeemListeners <- liftAff $ AVar.new Map.empty
  pabWebsocketIn <- liftEffect $ HS.create
  pabWebsocketOut <- liftEffect $ HS.create
  errors <- liftEffect $ HS.create
  pabWebsocketOutQueue <- Queue.new
  requestMatchers <- Queue.new
  sub <- liftEffect $ HS.subscribe pabWebsocketOut.emitter \msg ->
    launchAff_ $ Queue.write pabWebsocketOutQueue msg
  clock <- liftEffect $ HS.create
  localStorage <- liftEffect $ Ref.new Map.empty
  currentTime <- liftEffect $ Ref.new unixEpoch
  let
    sources =
      { pabWebsocket: pabWebsocketIn.emitter
      , clock: clock.emitter
      , polling: { walletRegular: clock.emitter, walletSync: clock.emitter }
      }
    sinks = { pabWebsocket: pabWebsocketOut.listener }

    marshallErrors
      :: forall m a. MonadError Error m => MonadEffect m => m a -> m a
    marshallErrors m = catchError m \e -> do
      liftEffect $ HS.notify errors.listener e
      throwError e
    env = Env
      { contractStepCarouselSubscription
      , logger: Console.logger identity
      , endpointSemaphores
      , createListeners
      , applyInputListeners
      , redeemListeners
      , sinks
      , sources
      , handleRequest: HandleRequest \request -> marshallErrors
          do
            queuedMatcher <- Queue.tryRead requestMatchers
            let
              matcher = fromMaybe
                (boxRequestMatcher request.responseFormat fallbackMatcher)
                queuedMatcher

            case unboxRequestMatcher matcher request of
              Left error -> failMultiline error
              Right response -> pure response
      , localStorage:
          { getItem: \(Key key) -> Map.lookup key <$> Ref.read localStorage
          , setItem: \(Key key) item ->
              Ref.modify_ (Map.insert key item) localStorage
          , removeItem: \(Key key) -> Ref.modify_ (Map.delete key) localStorage
          }
      }
    coenv =
      { localStorage
      , currentTime
      , clock: clock.listener
      , pabWebsocketIn: pabWebsocketIn.listener
      , pabWebsocketOut: pabWebsocketOutQueue
      , requestMatchers
      , dispose: do
          HS.unsubscribe sub
      }
  pure $ env /\ coenv /\ errors.emitter

-- Like `Env` but we invert the control of everything in it (i.e. all sources
-- become sinks, sinks sources).
type Coenv =
  { pabWebsocketOut :: Queue CombinedWSStreamToServer
  , requestMatchers :: Queue RequestMatcherBox
  , pabWebsocketIn :: Listener (FromSocket CombinedWSStreamToClient)
  , clock :: Listener Unit
  , currentTime :: Ref Instant
  , localStorage :: Ref (Map String String)
  , dispose :: Effect Unit
  }

failMultiline :: forall m a. MonadThrow Error m => MatcherError -> m a
failMultiline = throwError <<< error <<< show
