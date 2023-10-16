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

data Driver m = Driver
  { sendSuccessMessage :: Put -> m ()
  , sendFailureMessage :: Text -> m ()
  , recvMessageUntyped :: forall a. Maybe BS.ByteString -> Get a -> m (Either RecvError (a, Maybe BS.ByteString))
  }

data RecvError
  = PeerDisconnected
  | PeerCrashed Text
  | DeserializeError BS.ByteString ByteOffset String
  deriving stock (Show, Read, Eq, Ord)
  deriving anyclass (Exception)

data StatusToken
  = SuccessToken
  | FailureToken
  deriving stock (Show, Read, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Binary)

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
