module API.Request
  ( doPostRequest
  , doPostRequestWith
  , doEmptyPostRequest
  , doPutRequest
  , doGetRequest
  ) where

import Prologue
import Affjax (Request, defaultRequest, request)
import Affjax.RequestBody as Request
import Affjax.ResponseFormat as Response
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Argonaut (JsonDecodeError)
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
doPostRequest = doPostRequestWith { encode: encodeJson, decode: decodeJson }

doPostRequestWith
  :: forall m d e
   . MonadError AjaxError m
  => MonadAff m
  => { encode :: e -> Json
     , decode :: (Json -> Either JsonDecodeError d)
     }
  -> String
  -> e
  -> m d
doPostRequestWith { encode, decode } url requestBody =
  performWith decode
    $ defaultRequest
        { method = fromString "POST"
        , url = url
        , headers = defaultRequest.headers
        , content = Just $ Request.json $ encode requestBody
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
perform req = performWith decodeJson req

performWith
  :: forall m d
   . MonadError AjaxError m
  => MonadAff m
  => (Json -> Either JsonDecodeError d)
  -> Request Json
  -> m d
performWith decode req = do
  result <- liftAff $ request req
  response <- case result of
    Left err -> throwError $ { request: req, description: ConnectingError err }
    Right r -> pure r
  when (unwrap response.status < 200 || unwrap response.status >= 299)
    $ throwError
    $ { request: req, description: UnexpectedHTTPStatus response }
  case decode response.body of
    Left err -> throwError $ { request: req, description: DecodingError err }
    Right body -> pure body
