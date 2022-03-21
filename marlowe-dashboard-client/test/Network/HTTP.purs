module Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , MockHttpM
  , RequestBox(..)
  , RequestMatcher
  , runMockHttpM
  , runRequestMatcher
  , expectCondition
  , expectContent
  , expectCustomMethod
  , expectJsonContent
  , expectJsonRequest
  , expectMaybe
  , expectMethod
  , expectNoContent
  , expectRequest
  , expectRequest'
  , expectTextContent
  , expectTextRequest
  , expectUri
  , renderMatcherError
  , requestMatcher
  ) where

import Prelude

import Affjax (Error, Request, Response)
import Affjax as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Capability.Marlowe (class ManageMarlowe)
import Capability.PAB (class ManagePAB)
import Capability.PlutusApps.FollowerApp (class FollowerApp)
import Capability.Toast (class Toast)
import Capability.Wallet (class ManageWallet)
import Clipboard (class MonadClipboard)
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Alt (class Alt, alt)
import Control.Apply (lift2)
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
  , local
  , runReaderT
  )
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.State (class MonadState, StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (class MonadTell, class MonadWriter, WriterT)
import Data.Argonaut
  ( class EncodeJson
  , encodeJson
  , stringify
  , stringifyWithIndent
  )
import Data.Array (fromFoldable)
import Data.Bifunctor (lmap)
import Data.Distributive (class Distributive)
import Data.Either (Either(..), either, note)
import Data.HTTP.Method (CustomMethod, Method, unCustomMethod)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Extra (repeat)
import Data.Validation.Semigroup (V(..))
import Effect.Aff (Aff, error)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect)
import Effect.Unlift (class MonadUnliftEffect)
import Halogen.Store.Monad (class MonadStore)
import Servant.PureScript
  ( class MonadAjax
  , AjaxError(..)
  , ErrorDescription(..)
  , prepareRequest
  , prepareResponse
  , printAjaxError
  )
import Test.Assertions (jsonDiffString)
import Test.Halogen (class MonadHalogenTest)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Unsafe.Coerce (unsafeCoerce)

newtype MockHttpM (m :: Type -> Type) a = MockHttpM
  (ReaderT (Queue RequestBox) m a)

runMockHttpM :: forall m a. MockHttpM m a -> Queue RequestBox -> m a
runMockHttpM (MockHttpM r) = runReaderT r

derive newtype instance Functor m => Functor (MockHttpM m)
derive newtype instance Apply m => Apply (MockHttpM m)
derive newtype instance Applicative m => Applicative (MockHttpM m)
derive newtype instance Bind m => Bind (MockHttpM m)
derive newtype instance Monad m => Monad (MockHttpM m)
derive newtype instance MonadEffect m => MonadEffect (MockHttpM m)
derive newtype instance MonadUnliftEffect m => MonadUnliftEffect (MockHttpM m)
derive newtype instance MonadAff m => MonadAff (MockHttpM m)
derive newtype instance MonadUnliftAff m => MonadUnliftAff (MockHttpM m)
derive newtype instance MonadBase b m => MonadBase b (MockHttpM m)
derive newtype instance MonadUnlift b m => MonadUnlift b (MockHttpM m)
derive newtype instance MonadThrow e m => MonadThrow e (MockHttpM m)
derive newtype instance MonadError e m => MonadError e (MockHttpM m)
derive newtype instance MonadTell s m => MonadTell s (MockHttpM m)
derive newtype instance MonadWriter s m => MonadWriter s (MockHttpM m)
derive newtype instance MonadState s m => MonadState s (MockHttpM m)
derive newtype instance MonadCont m => MonadCont (MockHttpM m)
derive newtype instance MonadRec m => MonadRec (MockHttpM m)
derive newtype instance MonadFork f m => MonadFork f (MockHttpM m)
derive newtype instance MonadTime m => MonadTime (MockHttpM m)
derive newtype instance MonadTest m => MonadTest (MockHttpM m)
derive newtype instance MonadLogger l m => MonadLogger l (MockHttpM m)
derive newtype instance MonadStore a s m => MonadStore a s (MockHttpM m)
derive newtype instance ManageWallet m => ManageWallet (MockHttpM m)
derive newtype instance ManagePAB m => ManagePAB (MockHttpM m)
derive newtype instance FollowerApp m => FollowerApp (MockHttpM m)
derive newtype instance Toast m => Toast (MockHttpM m)
derive newtype instance
  ( Monad m
  , MonadClipboard m
  ) =>
  MonadClipboard (MockHttpM m)

derive newtype instance ManageMarlowe m => ManageMarlowe (MockHttpM m)
derive newtype instance
  MonadHalogenTest q i o m =>
  MonadHalogenTest q i o (MockHttpM m)

derive newtype instance MonadUser m => MonadUser (MockHttpM m)

instance MonadAff m => MonadMockHTTP (MockHttpM m) where
  expectRequest responseFormat matcher = do
    requests <- MockHttpM ask
    liftAff $ flip tailRecM 0 \tries ->
      if tries > 3 then
        throwError $ error $ "Expected an HTTP request to be made"
      else
        Queue.tryRead requests >>= case _ of
          Nothing -> pure $ Loop $ tries + 1
          Just request ->
            Done <$> runRequestMatcher responseFormat matcher request

instance MonadAff m => MonadAjax api (MockHttpM m) where
  request _ req = do
    requests <- MockHttpM ask
    liftAff do
      responseA <- AVar.empty
      let aReq = prepareRequest req
      Queue.write requests $ RequestBox \next -> next aReq responseA
      response <- AVar.take responseA
      case prepareResponse req.decode aReq response of
        Left e@(AjaxError { description: MalformedContent _ }) ->
          throwError $ error $ joinWith "\n"
            [ "Failed to decode HTTP response:"
            , printAjaxError e
            ]
        x -> pure x

instance MonadKill e f m => MonadKill e f (MockHttpM m) where
  kill e = lift <<< kill e

instance MonadBracket e f m => MonadBracket e f (MockHttpM m) where
  bracket (MockHttpM acquire) release run = MockHttpM $ bracket
    acquire
    (\c a -> case release c a of MockHttpM r -> r)
    (\a -> case run a of MockHttpM r -> r)
  uninterruptible (MockHttpM r) = MockHttpM $ uninterruptible r
  never = lift never

derive newtype instance Distributive g => Distributive (MockHttpM g)
instance MonadTrans (MockHttpM) where
  lift m = MockHttpM $ lift m

instance MonadAsk r m => MonadAsk r (MockHttpM m) where
  ask = lift ask

instance MonadReader r m => MonadReader r (MockHttpM m) where
  local f (MockHttpM (ReaderT r)) = MockHttpM $ ReaderT $ local f <<< r

class Monad m <= MonadMockHTTP m where
  expectRequest
    :: forall a
     . ResponseFormat a
    -> RequestMatcher (Either Error (Response a))
    -> m Unit

instance MonadMockHTTP m => MonadMockHTTP (ReaderT r m) where
  expectRequest = map (map lift) expectRequest

instance (Monoid w, MonadMockHTTP m) => MonadMockHTTP (WriterT w m) where
  expectRequest = map (map lift) expectRequest

instance MonadMockHTTP m => MonadMockHTTP (StateT s m) where
  expectRequest = map (map lift) expectRequest

instance MonadMockHTTP m => MonadMockHTTP (ContT r m) where
  expectRequest = map (map lift) expectRequest

instance MonadMockHTTP m => MonadMockHTTP (ExceptT e m) where
  expectRequest = map (map lift) expectRequest

instance MonadMockHTTP m => MonadMockHTTP (MaybeT m) where
  expectRequest = map (map lift) expectRequest

instance (Monoid w, MonadMockHTTP m) => MonadMockHTTP (RWST r w s m) where
  expectRequest = map (map lift) expectRequest

expectTextRequest
  :: forall m
   . MonadMockHTTP m
  => RequestMatcher String
  -> m Unit
expectTextRequest = expectRequest' Response.string

expectJsonRequest
  :: forall a m
   . EncodeJson a
  => MonadMockHTTP m
  => RequestMatcher a
  -> m Unit
expectJsonRequest = expectRequest' Response.json <<< map encodeJson

expectRequest'
  :: forall a m
   . MonadMockHTTP m
  => ResponseFormat a
  -> RequestMatcher a
  -> m Unit
expectRequest' format = expectRequest format <<< map
  ( Right <<<
      { status: StatusCode 200
      , statusText: "OK"
      , headers: []
      , body: _
      }
  )

newtype RequestMatcher a = RequestMatcher
  (forall content. Request content -> Either MatcherError a)

newtype RequestBox = RequestBox
  ( forall r
     . ( forall a
          . Affjax.Request a
         -> AVar (Either Affjax.Error (Affjax.Response a))
         -> r
       )
    -> r
  )

renderMatcherError :: RequestBox -> MatcherError -> String
renderMatcherError (RequestBox f) (MatcherError lines) = f \request _ ->
  joinWith "\n  " $ join
    [ pure "HTTP request did not meet expectations"
    , pure ""
    , lines
    , pure ""
    , pure "Request:"
    , indent 2 $ join
        [ pure $ joinWith " "
            [ either show unCustomMethod request.method
            , request.url
            ]
        , map show request.headers
        , fromFoldable request.content >>=
            split (Pattern "\n") <<< case _ of
              Request.ArrayView _ -> "<array view>"
              Request.Blob _ -> "<blob>"
              Request.Document _ -> "<document>"
              Request.String s -> s
              Request.FormData _ -> "<form data>"
              Request.FormURLEncoded x -> show x
              Request.Json json -> stringifyWithIndent 2 json
        ]
    ]

runRequestMatcher
  :: forall a
   . ResponseFormat a
  -> RequestMatcher (Either Error (Response a))
  -> RequestBox
  -> Aff Unit
runRequestMatcher providedFormat (RequestMatcher matcher) (RequestBox f) = f go
  where
  go
    :: forall b
     . Request b
    -> AVar (Either Affjax.Error (Affjax.Response b))
    -> Aff Unit
  go request responseA = either
    (throwError <<< error <<< renderMatcherError (RequestBox f))
    (flip AVar.put responseA)
    ( map (map unsafeCoerceResponse)
        case request.responseFormat, providedFormat of
          ArrayBuffer _, ArrayBuffer _ ->
            matcher request
          Blob _, Blob _ ->
            matcher request
          Document _, Document _ ->
            matcher request
          Json _, Json _ ->
            matcher request
          String _, String _ ->
            matcher request
          Ignore _, _ ->
            matcher request
          _, Ignore _ ->
            matcher request
          requiredFormat, _ -> Left $ MatcherError
            [ "Provided response format: " <> toResponseType providedFormat
            , "Does not match required response format: "
                <> toResponseType requiredFormat
            ]
    )
    where
    toResponseType :: forall c. ResponseFormat c -> String
    toResponseType = case _ of
      ArrayBuffer _ -> "arraybuffer"
      Blob _ -> "blob"
      Document _ -> "document"
      -- This case is why we need to re-implement this, even though Affjax
      -- already does. for Json, it returns "text"
      Json _ -> "json"
      String _ -> "text"
      Ignore _ -> ""

    unsafeCoerceResponse :: Response a -> Response b
    unsafeCoerceResponse = unsafeCoerce

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

newtype MatcherError = MatcherError (Array String)

derive instance Newtype MatcherError _
derive instance Eq MatcherError
derive instance Ord MatcherError
derive newtype instance Semigroup MatcherError
derive newtype instance Monoid MatcherError

requestMatcher
  :: forall a
   . (forall content. Request content -> Either MatcherError a)
  -> RequestMatcher a
requestMatcher = RequestMatcher

expectMaybe
  :: forall a
   . (forall content. Request content -> Maybe a)
  -> MatcherError
  -> RequestMatcher a
expectMaybe f error = RequestMatcher (note error <<< f)

expectCondition
  :: (forall content. Request content -> Boolean)
  -> MatcherError
  -> RequestMatcher Unit
expectCondition condition error = RequestMatcher \request ->
  if condition request then Right unit
  else Left error

expectMethod :: Method -> RequestMatcher Unit
expectMethod method = expectCondition (eq (Left method) <<< _.method) $
  MatcherError
    [ "✗ Actual method does not match the following expected Method: " <> show
        method
    ]

expectCustomMethod :: CustomMethod -> RequestMatcher Unit
expectCustomMethod method = expectCondition (eq (Right method) <<< _.method) $
  MatcherError
    [ "✗ Actual method does not match the following expected Method: " <>
        unCustomMethod method
    ]

expectUri :: String -> RequestMatcher Unit
expectUri uri = expectCondition ((uri == _) <<< _.url) $
  MatcherError
    [ "✗ Actual URI does not match the following expected URI: " <> uri ]

expectTextContent :: String -> RequestMatcher Unit
expectTextContent = expectContent <<< Request.string

expectJsonContent :: forall a. EncodeJson a => a -> RequestMatcher Unit
expectJsonContent = expectContent <<< Request.json <<< encodeJson

expectNoContent :: RequestMatcher Unit
expectNoContent = expectCondition (isNothing <<< _.content) $ MatcherError
  [ "✗ Expected no content but saw content" ]

expectContent :: RequestBody -> RequestMatcher Unit
expectContent expected = RequestMatcher \request ->
  lmap MatcherError case request.content, expected of
    Nothing, _ -> Left $ [ "Expected content, but saw none" ]
    Just (Request.Json actualJson), Request.Json expectedJson
      | stringify expectedJson == stringify actualJson -> Right unit
      | otherwise -> Left $ join
          [ pure
              "✗ Actual JSON content does not match expected content:"
          , indent 2
              $
                [ withGraphics (foreground Red) "- Actual"
                , withGraphics (foreground Green) "+ Expected"
                , ""
                , ""
                ] <> split
                  (Pattern "\n")
                  (jsonDiffString actualJson expectedJson)
          ]
    Just (Request.String actualStr), Request.String expectedStr
      | expectedStr == actualStr -> Right unit
      | otherwise -> Left $ join
          [ pure
              "✗ Actual text content does not match the following expected content:"
          , indent 2 $ split (Pattern "\n") expectedStr
          ]
    Just actualBody, expectedBody
      | toContentType actualBody == toContentType expectedBody -> Left
          [ "✗ Cannot compare " <> toContentType expectedBody <> " for equality"
          ]
      | otherwise -> Left
          [ "✗ Actual content type: " <> toContentType actualBody
          , "✗ Does not match expected content type: " <> toContentType
              expectedBody
          ]
  where
  toContentType = case _ of
    Request.ArrayView _ -> "arrayview"
    Request.Blob _ -> "blob"
    Request.Document _ -> "document"
    Request.String _ -> "text"
    Request.FormData _ -> "formdata"
    Request.FormURLEncoded _ -> "formURLEncoded"
    Request.Json _ -> "json"

indent :: Int -> Array String -> Array String
indent i = map $ append $ repeat i " "
