module Test.Marlowe.Run where

import Prologue

import Affjax as Affjax
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as AR
import Affjax.StatusCode (StatusCode(..))
import AppM (runAppM)
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Alt (class Alt, alt)
import Control.Apply (lift2)
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
import Data.Argonaut (class EncodeJson, encodeJson, stringifyWithIndent)
import Data.Array (fromFoldable)
import Data.DateTime.Instant (Instant)
import Data.Either (either)
import Data.HTTP.Method (CustomMethod, Method, unCustomMethod)
import Data.LocalContractNicknames (emptyLocalContractNicknames)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, over2, unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Extra (repeat)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Validation.Semigroup (V(..))
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
import Servant.PureScript
  ( class ContentType
  , fromRequestBody
  , responseFormat
  , serializeContent
  )
import Store (mkStore)
import Test.Halogen (class MonadHalogenTest, runUITest)
import Test.Spec (Spec, it)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import WebSocket.Support (FromSocket)

marloweRunTest
  :: String
  -> ( forall m
        . MonadReader Coenv m
       => MonadError Error m
       => MonadHalogenTest MF.Query Unit MF.Msg m
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
      , handleRequest: HandleRequest \request -> marshallErrors do
          queuedMatcher <- Queue.tryRead requestMatchers
          let
            RequestMatcherWithResponse matcher = fromMaybe fallbackMatcher
              queuedMatcher
          case matcher request of
            Left error -> failMultiline $ error <> MatcherError
              ( join
                  [ pure "Request:"
                  , indent 2 $ join
                      [ pure $ joinWith " "
                          [ either show unCustomMethod request.method
                          , request.url
                          ]
                      , map show request.headers
                      , fromFoldable request.content >>=
                          split (Pattern "\n") <<< case _ of
                            ArrayView _ -> "<array view>"
                            Blob _ -> "<blob>"
                            Document _ -> "<document>"
                            String s -> s
                            FormData _ -> "<form data>"
                            FormURLEncoded x -> show x
                            Json json -> stringifyWithIndent 2 json
                      ]
                  ]
              )
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

newtype MatcherError = MatcherError (Array String)

derive instance Newtype MatcherError _
derive instance Eq MatcherError
derive instance Ord MatcherError

instance Show MatcherError where
  show = joinWith "\n  " <<< unwrap

instance Semigroup MatcherError where
  append = over2 MatcherError \a b -> a <> [ repeat 50 "=" ] <> b

instance Monoid MatcherError where
  mempty = MatcherError mempty

newtype RequestMatcherWithResponse = RequestMatcherWithResponse
  ( forall decodeError content
     . ContentType decodeError content
    => Affjax.Request content
    -> Either (MatcherError) (Either Affjax.Error (Affjax.Response content))
  )

newtype RequestMatcher a = RequestMatcher
  ( forall decodeError content
     . ContentType decodeError content
    => Affjax.Request content
    -> Either MatcherError a
  )

derive instance Functor RequestMatcher

instance Apply RequestMatcher where
  apply (RequestMatcher f) (RequestMatcher a) =
    RequestMatcher (unwrap <<< lift2 apply (V <<< f) (V <<< a))

instance Applicative RequestMatcher where
  pure a = RequestMatcher (const $ pure a)

instance Alt RequestMatcher where
  alt (RequestMatcher a) (RequestMatcher b) =
    RequestMatcher (lift2 alt a b)

instance Semigroup a => Semigroup (RequestMatcher a) where
  append = lift2 append

instance Monoid a => Monoid (RequestMatcher a) where
  mempty = pure mempty

fallbackMatcher :: RequestMatcherWithResponse
fallbackMatcher = RequestMatcherWithResponse
  (const $ Left $ MatcherError [ "Unexpected HTTP Request" ])

-- Like `Env` but we invert the control of everything in it (i.e. all sources
-- become sinks, sinks sources).
type Coenv =
  { pabWebsocketOut :: Queue CombinedWSStreamToServer
  , requestMatchers :: Queue RequestMatcherWithResponse
  , pabWebsocketIn :: Listener (FromSocket CombinedWSStreamToClient)
  , clock :: Listener Unit
  , currentTime :: Ref Instant
  , localStorage :: Ref (Map String String)
  , dispose :: Effect Unit
  }

expectRequest'
  :: forall decodeError content m
   . ContentType decodeError content
  => MonadAsk Coenv m
  => MonadAff m
  => RequestMatcher (Either Affjax.Error (Affjax.Response content))
  -> m Unit
expectRequest' matcher = do
  matchers <- asks _.requestMatchers
  liftAff $ Queue.write matchers $ requestMatcherWithResponse matcher

expectRequest
  :: forall decodeError content m
   . ContentType decodeError content
  => MonadAsk Coenv m
  => MonadAff m
  => RequestMatcher content
  -> m Unit
expectRequest = expectRequest' <<< map
  ( Right <<<
      { status: StatusCode 200
      , statusText: "OK"
      , headers: []
      , body: _
      }
  )

expectJsonRequest
  :: forall a m
   . EncodeJson a
  => MonadAsk Coenv m
  => MonadAff m
  => RequestMatcher a
  -> m Unit
expectJsonRequest = expectRequest <<< map encodeJson

expectMethod :: Method -> RequestMatcher Unit
expectMethod method = conditionMatcher (eq (Left method) <<< _.method) $
  MatcherError
    [ "Actual method does not match the following expected Method: " <> show
        method
    ]

expectCustomMethod :: CustomMethod -> RequestMatcher Unit
expectCustomMethod method = conditionMatcher (eq (Right method) <<< _.method) $
  MatcherError
    [ "Actual method does not match the following expected Method: " <>
        unCustomMethod method
    ]

expectUri :: String -> RequestMatcher Unit
expectUri uri = conditionMatcher ((uri == _) <<< _.url) $
  MatcherError
    [ "Actual URI does not match the following expected URI: " <> uri ]

expectJsonContent :: forall a. EncodeJson a => a -> RequestMatcher Unit
expectJsonContent = expectContent <<< Just <<< encodeJson

expectContent
  :: forall decodeError content
   . ContentType decodeError content
  => Maybe content
  -> RequestMatcher Unit
expectContent mContent =
  conditionMatcher (eq mContent <<< (fromRequestBody <=< _.content))
    $ MatcherError case mContent of
        Nothing -> [ "No content was expected" ]
        Just content -> join
          [ pure "Actual content does not match the following expected content:"
          , indent 2 $ split (Pattern "\n") (serializeContent content)
          ]

conditionMatcher
  :: ( forall decodeError' content'
        . ContentType decodeError' content'
       => Affjax.Request content'
       -> Boolean
     )
  -> MatcherError
  -> RequestMatcher Unit
conditionMatcher condition error = RequestMatcher \request ->
  if condition request then Right unit
  else Left error

requestMatcherWithResponse
  :: forall decodeError content
   . ContentType decodeError content
  => RequestMatcher (Either Affjax.Error (Affjax.Response content))
  -> RequestMatcherWithResponse
requestMatcherWithResponse (RequestMatcher matcher) =
  RequestMatcherWithResponse go
  where
  go
    :: forall decodeError' content'
     . ContentType decodeError' content'
    => Affjax.Request content'
    -> Either MatcherError (Either Affjax.Error (Affjax.Response content'))
  go request = coerceResponse =<< matcher request
    where
    coerceResponse = case _ of
      Left e -> Right $ Left e
      Right response'
        | providedFormat == requiredFormat ->
            Right $ Right $ unsafeCoerceResponse response'
        | otherwise -> Left $ MatcherError
            [ "Provided response format: " <> providedFormat
            , "Does not match required response format: " <> requiredFormat
            ]
      where
      providedFormat = toResponseType $ responseFormat (Proxy :: _ content)
      requiredFormat = toResponseType request.responseFormat

      toResponseType :: forall a. AR.ResponseFormat a -> String
      toResponseType = case _ of
        AR.ArrayBuffer _ -> "arraybuffer"
        AR.Blob _ -> "blob"
        AR.Document _ -> "document"
        -- This case is why we need to re-implement this, even though Affjax
        -- already does. for Json, it returns "text"
        AR.Json _ -> "json"
        AR.String _ -> "text"
        AR.Ignore _ -> ""

      unsafeCoerceResponse
        :: Affjax.Response content -> Affjax.Response content'
      unsafeCoerceResponse = unsafeCoerce

indent :: Int -> Array String -> Array String
indent i = map $ append $ repeat i " "

failMultiline :: forall m a. MonadThrow Error m => MatcherError -> m a
failMultiline = throwError <<< error <<< show
