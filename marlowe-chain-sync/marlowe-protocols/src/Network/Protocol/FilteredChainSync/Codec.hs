{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

module Network.Protocol.FilteredChainSync.Codec where

import Control.Monad (mfilter)
import Data.Binary (Binary (get), Get, getWord8, put)
import Data.Binary.Get (ByteOffset, Decoder (..), runGetIncremental)
import Data.Binary.Put (putWord8, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.FilteredChainSync.Types (ClientHasAgency (..), FilteredChainSync, Message (..),
                                                 ServerHasAgency (..), TokNextKind (..))
import Network.TypedProtocol (PeerHasAgency (..), PeerRole, SomeMessage (..))
import Network.TypedProtocol.Codec (Codec (..), DecodeStep (..))
import Unsafe.Coerce (unsafeCoerce)

data DeserializeError = DeserializeError
  { message         :: !String
  , offset          :: !ByteOffset
  , unconsumedInput :: !BS.ByteString
  }

data SomeQuery query = forall err result. SomeQuery (query err result)

data SomeResult query = forall err result. SomeResult (query err result) result

data SomeError query = forall err result. SomeError (query err result) err

codecFilteredChainSync
  :: forall query point tip m
   . Applicative m
  => (SomeQuery query -> LBS.ByteString)
  -> Get (SomeQuery query)
  -> (SomeResult query -> LBS.ByteString)
  -> Get (SomeResult query)
  -> (SomeError query -> LBS.ByteString)
  -> Get (SomeError query)
  -> (point -> LBS.ByteString)
  -> Get point
  -> (tip -> LBS.ByteString)
  -> Get tip
  -> Codec (FilteredChainSync query point tip) DeserializeError m LBS.ByteString
codecFilteredChainSync
  encodeQuery
  decodeQuery
  encodeResult
  decodeResult
  encodeError
  decodeError
  encodePoint
  decodePoint
  encodeTip
  decodeTip = Codec encodeMsg $ decodeGet . decodeMsg
  where
    encodeMsg
      :: forall
          (pr :: PeerRole)
          (st :: FilteredChainSync query point tip)
          (st' :: FilteredChainSync query point tip)
       . PeerHasAgency pr st
      -> Message (FilteredChainSync query point tip) st st'
      -> LBS.ByteString
    encodeMsg (ClientAgency TokInit) = \case
      MsgRequestHandshake schemaVersion -> runPut do
        putWord8 0x01
        put schemaVersion

    encodeMsg (ClientAgency TokIdle) = runPut . \case
      MsgQueryNext pos query -> do
        putWord8 0x04
        put $ encodePoint pos
        put $ encodeQuery $ SomeQuery query
      MsgDone            -> putWord8 0x09

    encodeMsg (ServerAgency TokHandshake) = runPut . \case
      MsgConfirmHandshake                  -> putWord8 0x02
      MsgRejectHandshake supportedVersions -> do
       putWord8 0x03
       put supportedVersions

    encodeMsg (ServerAgency TokNext{}) = runPut . \case
      MsgRejectQuery query err tip          -> do
        putWord8 0x05
        put $ encodeError $ SomeError query err
        put $ encodeTip tip
      MsgRollForward query result pos tip -> do
        putWord8 0x06
        put $ encodeResult $ SomeResult query result
        put $ encodePoint pos
        put $ encodeTip tip
      MsgRollBackward pos tip       -> do
        putWord8 0x07
        put $ encodePoint pos
        put $ encodeTip tip
      MsgWait                         -> putWord8 0x08

    decodeMsg
      :: forall (pr :: PeerRole) (st :: FilteredChainSync query point tip)
       . PeerHasAgency pr st
      -> Get (SomeMessage st)
    decodeMsg tok      = do
      tag <- getWord8
      case (tag, tok) of
        (0x01, ClientAgency TokInit) -> SomeMessage . MsgRequestHandshake <$> get

        (0x04, ClientAgency TokIdle) -> do
          pos <- decodePoint
          SomeQuery query <- decodeQuery
          pure $ SomeMessage $ MsgQueryNext pos query

        (0x09, ClientAgency TokIdle) -> pure $ SomeMessage MsgDone

        (0x02, ServerAgency TokHandshake) -> pure $ SomeMessage MsgConfirmHandshake

        (0x03, ServerAgency TokHandshake) -> SomeMessage . MsgRejectHandshake <$> get

        (0x05, ServerAgency (TokNext _)) -> do
          SomeError query err <- decodeError
          tip <- decodeTip
          -- FIXME how to do this without unsafeCoerce?
          pure $ unsafeCoerce $ SomeMessage $ MsgRejectQuery query err tip

        (0x06, ServerAgency (TokNext _)) -> do
          SomeResult query result <- decodeResult
          point <- decodePoint
          tip <- decodeTip
          -- FIXME how to do this without unsafeCoerce?
          pure $ unsafeCoerce $ SomeMessage $ MsgRollForward query result point tip

        (0x07, ServerAgency (TokNext _)) -> do
          point <- decodePoint
          tip <- decodeTip
          pure $ SomeMessage $ MsgRollBackward point tip

        (0x08, ServerAgency (TokNext TokCanAwait)) -> pure $ SomeMessage MsgWait

        _ -> fail $ "Unexpected tag " <> show tag

decodeGet :: Applicative m => Get a -> m (DecodeStep LBS.ByteString DeserializeError m a)
decodeGet = decodeDecoder . runGetIncremental

decodeDecoder :: Applicative m => Decoder a -> m (DecodeStep LBS.ByteString DeserializeError m a)
decodeDecoder = pure . \case
  Fail unconsumedInput offset message -> DecodeFail DeserializeError{..}
  Partial f                           -> DecodePartial $ decodeDecoder . f . fmap LBS.toStrict
  Done unconsumedInput _ a            -> DecodeDone a $ mfilter LBS.null $ Just $ LBS.fromStrict unconsumedInput
