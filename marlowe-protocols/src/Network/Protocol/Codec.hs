{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Protocol.Codec where

import Control.Exception (Exception (..))
import Control.Monad (mfilter)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (runPut)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Typeable)
import qualified Data.Text as T
import Network.TypedProtocol (Message, Protocol (..))
import Network.TypedProtocol.Codec

class ShowProtocol ps where
  showsPrecMessage :: Int -> PeerHasAgency pr st -> Message ps st st' -> ShowS
  default showsPrecMessage :: (Show (Message ps st st')) => Int -> PeerHasAgency pr st -> Message ps st st' -> ShowS
  showsPrecMessage p _ = showsPrec p

  showsPrecServerHasAgency :: forall (st :: ps). Int -> ServerHasAgency st -> ShowS
  default showsPrecServerHasAgency :: forall (st :: ps). (Show (ServerHasAgency st)) => Int -> ServerHasAgency st -> ShowS
  showsPrecServerHasAgency = showsPrec

  showsPrecClientHasAgency :: forall (st :: ps). Int -> ClientHasAgency st -> ShowS
  default showsPrecClientHasAgency :: forall (st :: ps). (Show (ClientHasAgency st)) => Int -> ClientHasAgency st -> ShowS
  showsPrecClientHasAgency = showsPrec

class (Protocol ps) => BinaryMessage ps where
  putMessage :: PeerHasAgency pr (st :: ps) -> Message ps st st' -> Put
  getMessage :: PeerHasAgency pr (st :: ps) -> Get (SomeMessage st)

newtype ShowPeerHasAgencyViaShowProtocol pr st = ShowPeerHasAgencyViaShowProtocol (PeerHasAgency pr st)

instance (ShowProtocol ps) => Show (ShowPeerHasAgencyViaShowProtocol pr (st :: ps)) where
  showsPrec p (ShowPeerHasAgencyViaShowProtocol pa) = showParen (p > 10) case pa of
    ClientAgency tok -> showString "ClientAgency" . showsPrecClientHasAgency p tok
    ServerAgency tok -> showString "ServerAgency" . showsPrecServerHasAgency p tok

data DeserializeError ps = forall pr (st :: ps).
  DeserializeError
  { message :: !String
  , offset :: !ByteOffset
  , unconsumedInput :: !BS.ByteString
  , state :: ShowPeerHasAgencyViaShowProtocol pr st
  }

deriving instance (ShowProtocol ps) => Show (DeserializeError ps)

instance (Typeable ps, ShowProtocol ps) => Exception (DeserializeError ps) where
  displayException (DeserializeError{..}) =
    unlines
      [ "Offset: " <> show offset
      , "Protocol State: " <> show state
      , "Message: " <> message
      , "Unconsumed Input: " <> T.unpack (encodeBase16 unconsumedInput)
      ]

binaryCodec :: (Applicative m, BinaryMessage ps) => Codec ps (DeserializeError ps) m LBS.ByteString
binaryCodec = Codec (encodePut . putMessage) (decodeGet <*> getMessage)

encodePut :: (a -> Put) -> a -> LBS.ByteString
encodePut = fmap runPut

decodeGet
  :: (Applicative m)
  => PeerHasAgency pr (st :: ps)
  -> Get a
  -> m (DecodeStep LBS.ByteString (DeserializeError ps) m a)
decodeGet (ShowPeerHasAgencyViaShowProtocol -> state) = go . runGetIncremental
  where
    go =
      pure . \case
        Fail unconsumedInput offset message -> DecodeFail DeserializeError{..}
        Partial f -> DecodePartial $ go . f . fmap LBS.toStrict
        Done unconsumedInput _ a -> DecodeDone a $ mfilter (not . LBS.null) $ Just $ LBS.fromStrict unconsumedInput
