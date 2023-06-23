{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Channel where

import Control.Concurrent.STM (STM, newTChan, readTChan, writeTChan)
import Control.Monad (mfilter, (>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (($>))
import Data.Text.Internal.Lazy (smallChunkSize)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString.Lazy as Socket
import qualified System.IO as IO
import UnliftIO (MonadIO, liftIO)

data Channel m a = Channel
  { send :: a -> m ()
  , recv :: m (Maybe a)
  }

isoKleisliChannel
  :: forall a b m
   . (Monad m)
  => (a -> m b)
  -> (b -> m a)
  -> Channel m a
  -> Channel m b
isoKleisliChannel f f' Channel{..} =
  Channel
    { send = f' >=> send
    , recv = recv >>= traverse f
    }

hoistChannel
  :: (forall x. m x -> n x)
  -> Channel m a
  -> Channel n a
hoistChannel nat Channel{..} =
  Channel
    { send = nat . send
    , recv = nat recv
    }

handlesAsChannel
  :: forall m
   . (MonadIO m)
  => IO.Handle
  -- ^ Read handle
  -> IO.Handle
  -- ^ Write handle
  -> Channel m LBS.ByteString
handlesAsChannel hread hwrite = Channel{..}
  where
    send :: LBS.ByteString -> m ()
    send chunk = liftIO do
      LBS.hPut hwrite chunk
      IO.hFlush hwrite

    recv :: m (Maybe LBS.ByteString)
    recv = liftIO do
      eof <- IO.hIsEOF hread
      if eof
        then pure Nothing
        else Just . LBS.fromStrict <$> BS.hGetSome hread smallChunkSize

socketAsChannel :: forall m. (MonadIO m) => Socket -> Channel m LBS.ByteString
socketAsChannel sock = Channel{..}
  where
    send :: LBS.ByteString -> m ()
    send = liftIO . Socket.sendAll sock

    recv :: m (Maybe LBS.ByteString)
    recv = liftIO $ mfilter (not . LBS.null) . pure <$> Socket.recv sock (fromIntegral smallChunkSize)

effectChannel
  :: (Monad m)
  => (a -> m ())
  -- ^ Run effect on send
  -> (Maybe a -> m ())
  -- ^ Run effect on recv
  -> Channel m a
  -> Channel m a
effectChannel onSend onRecv Channel{..} =
  Channel
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
        { channel =
            Channel
              { send = writeTChan ch1 . Just
              , recv = readTChan ch2
              }
        , close = writeTChan ch1 Nothing
        }
    , STMChannel
        { channel =
            Channel
              { send = writeTChan ch2 . Just
              , recv = readTChan ch1
              }
        , close = writeTChan ch2 Nothing
        }
    )
