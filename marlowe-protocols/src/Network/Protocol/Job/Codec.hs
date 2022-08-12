{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Network.Protocol.Job.Codec where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.Codec (DeserializeError, decodeGet, encodePut)
import Network.Protocol.Job.Types
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

data SomeCommand cmd = forall status err result. SomeCommand (cmd status err result)
data SomeJobId cmd = forall status err result. SomeJobId (JobId cmd status err result)

codecJob
  :: forall cmd m
   . (Applicative m, Command cmd)
  => (SomeCommand cmd -> Put)
  -> Get (SomeCommand cmd)
  -> (SomeJobId cmd -> Put)
  -> Get (SomeJobId cmd)
  -> (forall status err result. TokCommand cmd status err result -> Get (JobId cmd status err result))
  -> (forall status err result. TokCommand cmd status err result -> status -> Put)
  -> (forall status err result. TokCommand cmd status err result -> Get status)
  -> (forall status err result. TokCommand cmd status err result -> err -> Put)
  -> (forall status err result. TokCommand cmd status err result -> Get err)
  -> (forall status err result. TokCommand cmd status err result -> result -> Put)
  -> (forall status err result. TokCommand cmd status err result -> Get result)
  -> Codec (Job cmd) DeserializeError m LBS.ByteString
codecJob putCmd getCmd putCmdId getSomeCmdId getCmdId putStatus getStatus putErr getErr putResult getResult =
  Codec (encodePut . putMsg) $ decodeGet . getMsg
  where
    putMsg
      :: forall (pr :: PeerRole) (st :: Job cmd) (st' :: Job cmd)
       . PeerHasAgency pr st
      -> Message (Job cmd) st st'
      -> Put
    putMsg = \case
      ClientAgency TokInit -> \case
        MsgExec cmd -> do
          putWord8 0x01
          putCmd $ SomeCommand cmd
        MsgAttach cmdId -> do
          putWord8 0x02
          putCmdId $ SomeJobId cmdId
      ServerAgency (TokCmd _) -> \case
        MsgFail tokCmd err -> do
          putWord8 0x03
          putErr tokCmd err
        MsgSucceed tokCmd result -> do
          putWord8 0x04
          putResult tokCmd result
        MsgAwait status cmdId -> do
          putWord8 0x05
          putStatus (tokFromId cmdId) status
          putCmdId $ SomeJobId cmdId
      ClientAgency (TokAwait _) -> \case
        MsgPoll   -> putWord8 0x06
        MsgDetach -> putWord8 0x07

    getMsg
      :: forall (pr :: PeerRole) (st :: Job cmd)
       . PeerHasAgency pr st
      -> Get (SomeMessage st)
    getMsg tok = do
      tag <- getWord8
      case tag of
        0x01 -> case tok of
          ClientAgency TokInit -> do
            SomeCommand cmd <- getCmd
            pure $ SomeMessage $ MsgExec cmd
          _ -> fail "Invalid protocol state for MsgExec"
        0x02 -> case tok of
          ClientAgency TokInit -> do
            SomeJobId cmdId <- getSomeCmdId
            pure $ SomeMessage $ MsgAttach cmdId
          _ -> fail "Invalid protocol state for MsgAttach"
        0x03 -> case tok of
          ServerAgency (TokCmd tokCmd) -> do
            err <- getErr $ unsafeCoerce tokCmd
            pure $ SomeMessage $ unsafeCoerce $ MsgFail tokCmd err
          _ -> fail "Invalid protocol state for MsgFail"
        0x04 -> case tok of
          ServerAgency (TokCmd tokCmd) -> do
            result <- getResult $ unsafeCoerce tokCmd
            pure $ SomeMessage $ unsafeCoerce $ MsgSucceed tokCmd result
          _ -> fail "Invalid protocol state for MsgSucceed"
        0x05 -> case tok of
          ServerAgency (TokCmd tokCmd) -> do
            status <- getStatus $ unsafeCoerce tokCmd
            cmdId <- getCmdId $ unsafeCoerce tokCmd
            pure $ SomeMessage $ unsafeCoerce $ MsgAwait status cmdId
          _ -> fail "Invalid protocol state for MsgAwait"
        0x06 -> case tok of
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgPoll
          _                         -> fail "Invalid protocol state for MsgPoll"
        0x07 -> case tok of
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgDetach
          _                         -> fail "Invalid protocol state for MsgDetach"
        _ -> fail $ "Invalid msg tag " <> show tag
