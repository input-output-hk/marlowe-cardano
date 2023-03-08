{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Network.Channel
  where

import Control.Concurrent.STM (STM, newTChan, readTChan, writeTChan)
import Control.Monad (mfilter, (>=>))
import Control.Monad.With (MonadWithExceptable)
import Data.Aeson (Value(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (($>))
import Data.Text (Text)
import Data.Text.Internal.Lazy (smallChunkSize)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString.Lazy as Socket
import Observe.Event.Component (GetSelectorConfig, SelectorConfig(..), SomeJSON(SomeJSON), singletonFieldConfigWith)
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import qualified System.IO as IO

data Channel m a = Channel
  { send :: a -> m ()
  , recv :: m (Maybe a)
  }

isoKleisliChannel
  :: forall a b m
   . Monad m
  => (a -> m b)
  -> (b -> m a)
  -> Channel m a
  -> Channel m b
isoKleisliChannel f f' Channel{..} = Channel
  { send = f' >=> send
  , recv = recv >>= traverse f
  }

hoistChannel
  :: (forall x. m x -> n x)
  -> Channel m a
  -> Channel n a
hoistChannel nat Channel{..} = Channel
  { send = nat . send
  , recv = nat recv
  }

handlesAsChannel
  :: IO.Handle -- ^ Read handle
  -> IO.Handle -- ^ Write handle
  -> Channel IO LBS.ByteString
handlesAsChannel hread hwrite = Channel {..}
  where
    send :: LBS.ByteString -> IO ()
    send chunk = do
      LBS.hPut hwrite chunk
      IO.hFlush hwrite

    recv :: IO (Maybe LBS.ByteString)
    recv = do
      eof <- IO.hIsEOF hread
      if eof
        then pure Nothing
        else Just . LBS.fromStrict <$> BS.hGetSome hread smallChunkSize

socketAsChannel :: Socket -> Channel IO LBS.ByteString
socketAsChannel sock = Channel {..}
  where
    send :: LBS.ByteString -> IO ()
    send = Socket.sendAll sock

    recv :: IO (Maybe LBS.ByteString)
    recv = mfilter (not . LBS.null) . pure <$> Socket.recv sock (fromIntegral smallChunkSize)

effectChannel
  :: Monad m
  => (a -> m ())       -- ^ Run effect on send
  -> (Maybe a -> m ()) -- ^ Run effect on recv
  -> Channel m a
  -> Channel m a
effectChannel onSend onRecv Channel{..} = Channel
  { send = \a -> onSend a *> send a
  , recv = recv >>= \ma -> onRecv ma $> ma
  }

data STMChannel a = STMChannel
  { channel :: Channel STM a
  , close :: STM ()
  }

channelPair :: STM (STMChannel a, STMChannel a)
channelPair = do
  ch1 <- newTChan
  ch2 <- newTChan
  pure
    ( STMChannel
      { channel = Channel
          { send = writeTChan ch1 . Just
          , recv = readTChan ch2
          }
      , close = writeTChan ch1 Nothing
      }
    , STMChannel
      { channel = Channel
        { send = writeTChan ch2 . Just
        , recv = readTChan ch1
        }
      , close = writeTChan ch2 Nothing
      }
    )

data ChannelSelector bytes f where
  Send :: ChannelSelector bytes bytes
  Recv :: ChannelSelector bytes (Maybe bytes)

logChannel
  :: MonadWithExceptable m
  => EventBackend m r (ChannelSelector bytes)
  -> Channel m bytes
  -> Channel m bytes
logChannel eventBackend Channel{..} = Channel
  { send = \bytes -> withEvent eventBackend Send \ev -> do
      addField ev bytes
      send bytes
  , recv = withEvent eventBackend Recv \ev -> do
      mBytes <- recv
      addField ev mBytes
      pure mBytes
  }

getChannelSelectorConfig :: (bytes -> Text) -> Bool -> GetSelectorConfig (ChannelSelector bytes)
getChannelSelectorConfig renderBytes defaultEnabled = \case
  Send -> SelectorConfig "send" defaultEnabled $ singletonFieldConfigWith (SomeJSON . String . renderBytes) "bytes" True
  Recv -> SelectorConfig "recv" defaultEnabled $ singletonFieldConfigWith (SomeJSON . fmap (String . renderBytes)) "bytes" True
