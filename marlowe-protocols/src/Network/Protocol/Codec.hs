{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Codec where

import Control.Exception (Exception (..))
import Control.Monad (mfilter)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.TypedProtocol (Message, Protocol)
import Network.TypedProtocol.Codec

class (Protocol ps) => BinaryMessage ps where
  putMessage :: PeerHasAgency pr (st :: ps) -> Message ps st st' -> Put
  getMessage :: PeerHasAgency pr (st :: ps) -> Get (SomeMessage st)

data DeserializeError = DeserializeError
  { message :: !String
  , offset :: !ByteOffset
  , unconsumedInput :: !BS.ByteString
  }
  deriving (Show)

instance Exception DeserializeError where
  displayException (DeserializeError{..}) =
    unlines
      [ "Offset: " <> show offset
      , "Message: " <> message
      , "Unconsumed Input: " <> T.unpack (encodeBase16 unconsumedInput)
      ]

binaryCodec :: (Applicative m, BinaryMessage ps) => Codec ps DeserializeError m LBS.ByteString
binaryCodec = Codec (encodePut . putMessage) (decodeGet . getMessage)

encodePut :: (a -> Put) -> a -> LBS.ByteString
encodePut = fmap runPut

decodeGet :: (Applicative m) => Get a -> m (DecodeStep LBS.ByteString DeserializeError m a)
decodeGet = go . runGetIncremental
  where
    go =
      pure . \case
        Fail unconsumedInput offset message -> DecodeFail DeserializeError{..}
        Partial f -> DecodePartial $ go . f . fmap LBS.toStrict
        Done unconsumedInput _ a -> DecodeDone a $ mfilter (not . LBS.null) $ Just $ LBS.fromStrict unconsumedInput
