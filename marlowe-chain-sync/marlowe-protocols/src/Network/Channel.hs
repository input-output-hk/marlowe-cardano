{-# LANGUAGE RankNTypes #-}
module Network.Channel where

import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Internal.Lazy (smallChunkSize)
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
handlesAsChannel hread hwrite = Channel {send, recv}
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
