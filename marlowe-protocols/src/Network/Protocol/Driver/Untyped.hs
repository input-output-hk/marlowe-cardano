{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Driver.Untyped where

import Control.Exception (Exception)
import Control.Monad (guard)
import Data.Binary
import Data.Binary.Get (ByteOffset, Decoder (..), isEmpty, label, pushChunk, runGetIncremental)
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Channel

-- | An untyped protocol driver. Sits between a @Network.TypedProtocol.Driver@ which
-- is a stateful, typed channel for a specific protocol, and a @Network.Channel@ which is an unstructured,
-- stateless channel for raw bytes.
--
-- An untyped driver is able to send or receive arbitrary data, but it does so in a structured manner. Each payload will
-- be preceded by a status byte which indicates if the message is a normal, expected payload, or if it is an exception.
data Driver m = Driver
  { sendSuccessMessage :: Put -> m ()
  -- ^ Send a normal message encoded as a @Data.Binary.Put@
  , sendFailureMessage :: Text -> m ()
  -- ^ Send an exception message.
  , recvMessageUntyped :: forall a. Maybe BS.ByteString -> Get a -> m (Either RecvError (a, Maybe BS.ByteString))
  -- ^ Receive a message and attempt to decode it using a @Data.Binary.Get@.
  }

-- | What can go wrong during a recv call.
data RecvError
  = -- | The peer disconnected unexpectedly.
    PeerDisconnected
  | -- | The peer crashed and sent an exception message.
    PeerCrashed Text
  | -- | The peer sent unexpected binary data.
    DeserializeError BS.ByteString ByteOffset String
  deriving stock (Show, Read, Eq, Ord)
  deriving anyclass (Exception)

data StatusToken
  = SuccessToken
  | FailureToken
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Binary)

-- | Create a driver which will operate over a channel.
mkDriver :: forall m. (Monad m) => Channel m LBS.ByteString -> Driver m
mkDriver Channel{..} =
  Driver
    { sendSuccessMessage = send . runPut . (put SuccessToken *>)
    , sendFailureMessage = send . runPut . (put FailureToken *>) . put
    , recvMessageUntyped = \trailing getMessage ->
        runDecoder $ maybe id (flip pushChunk) trailing $ runGetIncremental do
          isEmpty >>= \case
            True -> pure Nothing
            False ->
              Just <$> do
                token <- label "StatusToken" get
                case token of
                  SuccessToken -> Right <$> label "Message" getMessage
                  FailureToken -> Left <$> label "Failure" get
    }
  where
    runDecoder :: Decoder (Maybe (Either Text a)) -> m (Either RecvError (a, Maybe BS.ByteString))
    runDecoder = \case
      Fail unconsumed byteOffset msg -> pure $ Left $ DeserializeError unconsumed byteOffset msg
      Partial consumeNext -> do
        next <- fmap LBS.toStrict <$> recv
        runDecoder $ consumeNext next
      Done _ _ Nothing -> pure $ Left PeerDisconnected
      Done _ _ (Just (Left msg)) -> pure $ Left $ PeerCrashed msg
      Done unconsumed _ (Just (Right msg)) -> pure $ Right (msg, unconsumed <$ guard (not $ BS.null unconsumed))
