{-# LANGUAGE GADTs #-}
module Language.Marlowe.Protocol.Query.Codec
  where

import Data.Binary (Get, Put, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Language.Marlowe.Protocol.Query.Types
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.TypedProtocol.Codec (Codec, PeerHasAgency(..), SomeMessage(SomeMessage))

codecMarloweQuery :: Applicative m => Codec MarloweQuery DeserializeError m LBS.ByteString
codecMarloweQuery = binaryCodec putMessage getMessage
  where
  putMessage :: PutMessage MarloweQuery
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgRequest req -> do
        putWord8 0x01
        put $ SomeRequest req

    ServerAgency (TokRequest req) -> \case
      MsgRespond a -> do
        putWord8 0x02
        putResult req a

  getMessage :: GetMessage MarloweQuery
  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> do
          SomeRequest req <- get
          pure $ SomeMessage $ MsgRequest req
        _ -> fail "Invalid protocol state for MsgRequest"

      0x02 -> case tok of
        ServerAgency (TokRequest req) -> SomeMessage . MsgRespond <$> getResult req
        ClientAgency _  -> fail "Invalid protocol state for MsgRespond"

      _ -> fail $ "Invalid message tag " <> show tag

  getResult :: StRequest a -> Get a
  getResult = \case
    TokBoth a b -> (,) <$> getResult a <*> getResult b
    TokContractHeaders -> get

  putResult :: StRequest a -> a -> Put
  putResult = \case
    TokBoth ta tb -> \(a, b) -> putResult ta a *> putResult tb b
    TokContractHeaders -> put