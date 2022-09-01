{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Network.Protocol.Codec (DeserializeError(..), binaryCodec, PutMessage, GetMessage) where

import Control.Exception (Exception)
import Control.Monad (mfilter)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.TypedProtocol (Message)
import Network.TypedProtocol.Codec

-- An error type for deserialization
data DeserializeError = DeserializeError
  { message         :: !String
  , offset          :: !ByteOffset
  , unconsumedInput :: !BS.ByteString
  } deriving (Show)

instance Exception DeserializeError where

type PutMessage ps =
  forall (pr :: PeerRole) (st :: ps) (st' :: ps)
     . PeerHasAgency pr st
    -> Message ps st st'
    -> Put

type GetMessage ps =
  forall (pr :: PeerRole) (st :: ps)
     . PeerHasAgency pr st
    -> Get (SomeMessage st)

binaryCodec
  :: Applicative m
  => PutMessage ps
  -> GetMessage ps
  -> Codec ps DeserializeError m LBS.ByteString
binaryCodec putMessage getMessage = Codec (encodePut . putMessage) (decodeGet . getMessage)

encodePut :: (a -> Put) -> a -> LBS.ByteString
encodePut = fmap runPut

decodeGet :: Applicative m => Get a -> m (DecodeStep LBS.ByteString DeserializeError m a)
decodeGet = go . runGetIncremental
  where
    go = pure . \case
      Fail unconsumedInput offset message -> DecodeFail DeserializeError{..}
      Partial f                           -> DecodePartial $ go . f . fmap LBS.toStrict
      Done unconsumedInput _ a            -> DecodeDone a $ mfilter (not . LBS.null) $ Just $ LBS.fromStrict unconsumedInput
