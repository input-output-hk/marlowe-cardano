{-# LANGUAGE GADTs #-}
module Language.Marlowe.Protocol.Query.Codec
  where

import Data.Binary (get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Language.Marlowe.Protocol.Query.Types
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.TypedProtocol.Codec (Codec, PeerHasAgency(..), SomeMessage(SomeMessage))

codecMarloweHeaderSync :: Applicative m => Codec MarloweQuery DeserializeError m LBS.ByteString
codecMarloweHeaderSync = binaryCodec putMessage getMessage
  where
  putMessage :: PutMessage MarloweQuery
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgGetContractHeaders range -> do
        putWord8 0x01
        put range

    ServerAgency TokGetContractHeaders -> \case
      MsgResolve result -> do
        putWord8 0x02
        put result

      MsgReject err -> do
        putWord8 0x03
        put err

  getMessage :: GetMessage MarloweQuery
  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> SomeMessage . MsgGetContractHeaders <$> get
        _ -> fail "Invalid protocol state for MsgGetContractHeaders"

      0x02 -> case tok of
        ServerAgency TokGetContractHeaders -> SomeMessage . MsgResolve <$> get
        ClientAgency _  -> fail "Invalid protocol state for MsgResolve"

      0x03 -> case tok of
        ServerAgency TokGetContractHeaders -> SomeMessage . MsgReject <$> get
        ClientAgency _ -> fail "Invalid protocol state for MsgReject"

      _ -> fail $ "Invalid message tag " <> show tag
