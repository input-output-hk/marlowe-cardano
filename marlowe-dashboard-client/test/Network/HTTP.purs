module Test.Network.HTTP
  ( class MonadMockHTTP
  , RequestMatcher
  , RequestMatcherBox
  , MatcherError(..)
  , boxRequestMatcher
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
  , fallbackMatcher
  , requestMatcher
  , runRequestMatcher
  , unboxRequestMatcher
  ) where

import Prelude

import Affjax (Error, Request, Response)
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat (ResponseFormat(..))
import Affjax.ResponseFormat as Response
import Affjax.StatusCode (StatusCode(..))
import Control.Alt (class Alt, alt)
import Control.Apply (lift2)
import Data.Argonaut (class EncodeJson, encodeJson, stringifyWithIndent)
import Data.Array (fromFoldable)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either(..), either, note)
import Data.HTTP.Method (CustomMethod, Method, unCustomMethod)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (class Newtype, over2, unwrap)
import Data.String (Pattern(..), joinWith, split)
import Data.String.Extra (repeat)
import Data.Validation.Semigroup (V(..))
import Unsafe.Coerce (unsafeCoerce)

class Monad m <= MonadMockHTTP m where
  expectRequest
    :: forall a
     . ResponseFormat a
    -> RequestMatcher (Either Error (Response a))
    -> m Unit

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

newtype RequestMatcherBox = RequestMatcherBox
  ( forall content
     . Request content
    -> Either MatcherError (Either Error (Response content))
  )

boxRequestMatcher
  :: forall a
   . ResponseFormat a
  -> RequestMatcher (Either Error (Response a))
  -> RequestMatcherBox
boxRequestMatcher providedFormat (RequestMatcher matcher) =
  RequestMatcherBox go
  where
  go
    :: forall b
     . Request b
    -> Either MatcherError (Either Error (Response b))
  go request = bimap (_ <> requestInfo) (map unsafeCoerceResponse)
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
      requiredFormat, _ -> Left $ MatcherError
        [ "Provided response format: " <> toResponseType providedFormat
        , "Does not match required response format: "
            <> toResponseType requiredFormat
        ]
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

    requestInfo = MatcherError $ join
      [ pure "Request:"
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

unboxRequestMatcher
  :: RequestMatcherBox
  -> forall a
   . Request a
  -> Either MatcherError (Either Error (Response a))
unboxRequestMatcher (RequestMatcherBox f) = f

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

instance Show MatcherError where
  show = joinWith "\n  " <<< unwrap

instance Semigroup MatcherError where
  append = over2 MatcherError \a b -> a <> [ repeat 80 "=" ] <> b

instance Monoid MatcherError where
  mempty = MatcherError mempty

fallbackMatcher :: forall a. RequestMatcher a
fallbackMatcher = RequestMatcher
  (const $ Left $ MatcherError [ "Unexpected HTTP Request" ])

requestMatcher
  :: forall a
   . (forall content. Request content -> Either MatcherError a)
  -> RequestMatcher a
requestMatcher = RequestMatcher

runRequestMatcher
  :: forall a
   . RequestMatcher a
  -> forall content
   . Request content
  -> Either MatcherError a
runRequestMatcher (RequestMatcher matcher) = matcher

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
    [ "Actual method does not match the following expected Method: " <> show
        method
    ]

expectCustomMethod :: CustomMethod -> RequestMatcher Unit
expectCustomMethod method = expectCondition (eq (Right method) <<< _.method) $
  MatcherError
    [ "Actual method does not match the following expected Method: " <>
        unCustomMethod method
    ]

expectUri :: String -> RequestMatcher Unit
expectUri uri = expectCondition ((uri == _) <<< _.url) $
  MatcherError
    [ "Actual URI does not match the following expected URI: " <> uri ]

expectTextContent :: String -> RequestMatcher Unit
expectTextContent = expectContent <<< Request.string

expectJsonContent :: forall a. EncodeJson a => a -> RequestMatcher Unit
expectJsonContent = expectContent <<< Request.json <<< encodeJson

expectNoContent :: RequestMatcher Unit
expectNoContent = expectCondition (isNothing <<< _.content) $ MatcherError
  [ "Expected no content but saw content" ]

expectContent :: RequestBody -> RequestMatcher Unit
expectContent expected = RequestMatcher \request ->
  lmap MatcherError case request.content, expected of
    Nothing, _ -> Left $ [ "Expected content, but saw none" ]
    Just (Request.Json actualJson), Request.Json expectedJson
      | expectedJson == actualJson -> Right unit
      | otherwise -> Left $ join
          [ pure
              "Actual JSON content does not match the following expected content:"
          , indent 2 $ split (Pattern "\n") (stringifyWithIndent 2 expectedJson)
          ]
    Just (Request.String actualStr), Request.String expectedStr
      | expectedStr == actualStr -> Right unit
      | otherwise -> Left $ join
          [ pure
              "Actual text content does not match the following expected content:"
          , indent 2 $ split (Pattern "\n") expectedStr
          ]
    Just actualBody, expectedBody
      | toContentType actualBody == toContentType expectedBody -> Left
          [ "Cannot compare " <> toContentType expectedBody <> " for equality"
          ]
      | otherwise -> Left
          [ "Actual content type: " <> toContentType actualBody
          , "Does not match expected content type: " <> toContentType
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
