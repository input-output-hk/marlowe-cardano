module JsonStream (createJsonStream, JsonStreamError(..)) where

import Prelude

import Data.Argonaut (Json)
import Effect (Effect)
import Node.Stream (Readable)

data JsonStreamError
  = StreamError
  | InvalidJson

instance Show JsonStreamError where
  show StreamError = "Stream Error"
  show InvalidJson = "Invalid Json"

-- This Function starts reading a stream, discarding characters until
-- it reaches the begin separator, then it reads until it reaches the
-- end separator. If the inner string can be parsed as Json, it fires
-- an onJson event.
foreign import _createJsonStream
  :: { stream :: Readable ()
     , slizeSize :: Int
     , beginSeparator :: String
     , endSeparator :: String
     , onJson :: Json -> Effect Unit
     , onError :: JsonStreamError -> Effect Unit
     , onFinish :: Effect Unit
     , streamError :: JsonStreamError
     , invalidJson :: JsonStreamError
     }
  -> Effect Unit

createJsonStream
  :: { stream :: Readable ()
     , slizeSize :: Int
     , beginSeparator :: String
     , endSeparator :: String
     , onJson :: Json -> Effect Unit
     , onError :: JsonStreamError -> Effect Unit
     , onFinish :: Effect Unit
     }
  -> Effect Unit
createJsonStream
  { stream, slizeSize, beginSeparator, endSeparator, onJson, onError, onFinish } =
  _createJsonStream
    { stream
    , slizeSize
    , beginSeparator
    , endSeparator
    , onJson
    , onError
    , onFinish
    , streamError: StreamError
    , invalidJson: InvalidJson
    }
