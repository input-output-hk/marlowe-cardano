module API.Request
  ( doPostRequest
  , doEmptyPostRequest
  , doPutRequest
  , doGetRequest
  ) where

import Prologue
import Affjax (Request, defaultRequest, request)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.HTTP.Method (fromString)
import Data.Newtype (unwrap)
import Effect.Aff.Class (class MonadAff, liftAff)
import Servant.PureScript (AjaxError, ErrorDescription(..))

doPostRequest
  :: forall m d e
   . MonadError AjaxError m
  => MonadAff m
  => DecodeJson d
  => EncodeJson e
  => String
  -> e
  -> m d
doPostRequest url requestBody =
  perform
    $ defaultRequest
        { method = fromString "POST"
        , url = url
        , headers = defaultRequest.headers
        , content = Just $ Request.json $ encodeJson requestBody
        , responseFormat = Response.json
        }

doEmptyPostRequest
  :: forall m d
   . MonadError AjaxError m
  => MonadAff m
  => DecodeJson d
  => String
  -> m d
doEmptyPostRequest url =
  perform
    $ defaultRequest
        { method = fromString "POST"
        , url = url
        , headers = defaultRequest.headers
        , responseFormat = Response.json
        }

doPutRequest
  :: forall m d
   . MonadError AjaxError m
  => MonadAff m
  => DecodeJson d
  => String
  -> m d
doPutRequest url =
  perform
    $ defaultRequest
        { method = fromString "PUT"
        , url = url
        , headers = defaultRequest.headers
        , responseFormat = Response.json
        }

doGetRequest
  :: forall m d
   . MonadError AjaxError m
  => MonadAff m
  => DecodeJson d
  => String
  -> m d
doGetRequest url =
  perform
    $ defaultRequest
        { method = fromString "GET"
        , url = url
        , headers = defaultRequest.headers
        , responseFormat = Response.json
        }

perform
  :: forall m d
   . MonadError AjaxError m
  => MonadAff m
  => DecodeJson d
  => Request Json
  -> m d
perform req = do
  result <- liftAff $ request req
  response <- case result of
    Left err -> throwError $ { request: req, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: req, description: UnexpectedHTTPStatus response }
  case decodeJson response.body of
    Left err -> throwError $ { request: req, description: DecodingError err }
    Right body -> pure body
