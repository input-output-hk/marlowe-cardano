{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Channel where

import Control.Concurrent.STM (STM, newTChan, readTChan, writeTChan)
import Control.Monad ((>=>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Binary.Get (getInt64be, runGet)
import Data.Binary.Put (putInt64be, runPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Functor (($>))
import Data.Int (Int64)
import GHC.Generics (Generic)
import GHC.IO (mkUserError)
import Network.Socket (Socket)
import qualified Network.Socket.ByteString.Lazy as Socket
import UnliftIO (MonadIO, liftIO, throwIO)

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

data FrameStatus
  = OkStatus
  | ErrorStatus
  deriving stock (Show, Read, Eq, Ord, Bounded, Enum, Generic)

data FrameHeader = FrameHeader
  { frameHeaderStatus :: FrameStatus
  , frameHeaderLength :: Int64
  }
  deriving stock (Show, Read, Eq, Ord, Generic)

data Frame = Frame
  { frameStatus :: FrameStatus
  , frameContents :: LBS.ByteString
  }

socketAsChannel :: forall m. (MonadIO m) => Socket -> Channel m Frame
socketAsChannel sock = Channel{..}
  where
    send :: Frame -> m ()
    send Frame{..} = liftIO do
      let headerBytes =
            LBS.cons
              ( case frameStatus of
                  OkStatus -> 0
                  ErrorStatus -> 1
              )
              (runPut $ putInt64be $ LBS.length frameContents)
      Socket.sendAll sock $ headerBytes <> frameContents

    recv :: m (Maybe Frame)
    recv = runMaybeT do
      headerBytes <- liftIO $ Socket.recv sock 9
      (statusByte, sizeBytes) <- MaybeT $ pure $ LBS.uncons headerBytes
      frameStatus <- case statusByte of
        0 -> pure OkStatus
        1 -> pure ErrorStatus
        _ -> throwIO $ mkUserError $ "Invalid status byte: " <> show statusByte
      let contentLength = runGet getInt64be sizeBytes
      frameContents <- liftIO $ Socket.recv sock contentLength
      pure Frame{..}

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
