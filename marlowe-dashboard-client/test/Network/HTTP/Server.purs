module Test.Network.HTTP.Server where

import Prologue

import Affjax as Affjax
import Affjax.RequestBody (RequestBody)
import Affjax.RequestBody as Request
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (Json, stringifyWithIndent)
import Data.CodePoint.Unicode (isSpace)
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Function (on)
import Data.HTTP.Method (Method, unCustomMethod)
import Data.String (Pattern(..), dropWhile, length, split, trim)
import Data.String.Extra (repeat)
import Effect.Aff (Error, error)
import Text.Pretty (class Pretty, Doc, hsep, pretty, text, vsep)

-- For now, simplify and assume we are only serving JSON requests. This could
-- change down the road but it makes things a lot easier for now.
type JsonRequest = Affjax.Request Json
type JsonResponse = Affjax.Response Json

type Sink :: forall k. (k -> Type) -> k -> Type -> Type
type Sink m r a = a -> m r

type Sink2 :: forall k. (k -> Type) -> k -> Type -> Type -> Type
type Sink2 m r a b = a -> b -> m r

newtype TestServer :: forall k. (k -> Type) -> Type -> Type
newtype TestServer m a = TestServer
  ( forall r
     . JsonRequest -- The request to serve
    -> Sink2 m r a JsonResponse -- HTTP response + value callback
    -> Sink2 m r a Affjax.Error -- HTTP error + value callback
    -> Sink m r Doc -- Test error callback
    -> Sink2 m r (Array NotHandledReason) JsonRequest -- Next middleware callback
    -> m r
  )

data NotHandledReason
  = UriMismatch Doc
  | WrongMethod Method
  | Other Doc

instance Pretty NotHandledReason where
  pretty = case _ of
    UriMismatch expected ->
      hsep [ text "✗ URI does not match:", expected ]
    WrongMethod expected ->
      hsep [ text "✗ Method does not match:", text $ show expected ]
    Other message ->
      hsep [ text "✗", message ]

instance Semigroup (TestServer m a) where
  append (TestServer server1) (TestServer server2) =
    TestServer \req res resErr failTest next ->
      -- run the first server first
      server1 req res resErr failTest \reasons req' ->
        -- run the second server next
        server2 req' res resErr failTest \reasons' ->
          next (reasons <> reasons')

instance Monoid (TestServer m a) where
  mempty = TestServer \req _ _ _ next -> next [] req

serveRequest
  :: forall a m
   . MonadError Error m
  => TestServer m a
  -> JsonRequest
  -> Sink m Unit (Either Affjax.Error JsonResponse)
  -> m a
serveRequest (TestServer server) req res =
  server
    req
    (\a response -> res (Right response) *> pure a)
    (\a err -> res (Left err) *> pure a)
    errorHandler
    (const <<< errorHandler <<< notHandledReasonsToDoc)
  where
  errorHandler = throwError <<< error <<< show <<< addRequestInfo req

notHandledReasonsToDoc :: Array NotHandledReason -> Doc
notHandledReasonsToDoc reasons = vsep
  [ text "Middleware rejection trace:"
  , vsep $ pretty <$> reasons
  ]

addRequestInfo :: JsonRequest -> Doc -> Doc
addRequestInfo req doc = vsep
  [ doc
  , text ""
  , text "Request:"
  , hsep $ text <$> [ either show unCustomMethod req.method, req.url ]
  , vsep $ text <<< show <$> req.headers
  , foldMap showRequestBody req.content
  ]

showRequestBody :: RequestBody -> Doc
showRequestBody = case _ of
  Request.ArrayView _ -> text "<array view>"
  Request.Blob _ -> text "<blob>"
  Request.Document _ -> text "<document>"
  Request.String s -> stringToDoc s
  Request.FormData _ -> text "<form data>"
  Request.FormURLEncoded x -> stringToDoc $ show x
  Request.Json json -> jsonToDoc json

jsonToDoc :: Json -> Doc
jsonToDoc = stringToDoc <<< stringifyWithIndent 2

stringToDoc :: String -> Doc
stringToDoc = vsep <<< map lineToDoc <<< split (Pattern "\n")

lineToDoc :: String -> Doc
lineToDoc line =
  if indentation > 0 then
    text (repeat indentation " ") <> text (trim trimmed)
  else
    text (trim line)
  where
  trimmed = dropWhile isSpace line
  indentation = on (-) length line trimmed
