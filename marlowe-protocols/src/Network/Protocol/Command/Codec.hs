{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Network.Protocol.Command.Codec where

import Control.Monad (mfilter)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.ChainSeek.Codec (DeserializeError (..))
import Network.Protocol.Command.Types
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

data SomeCommand cmd = forall status err result. SomeCommand (cmd status err result)
data SomeCommandId cmd = forall status err result. SomeCommandId (CommandId cmd status err result)

codecCommand
  :: forall cmd m
   . (Applicative m, IsCommand cmd)
  => (SomeCommand cmd -> Put)
  -> Get (SomeCommand cmd)
  -> (SomeCommandId cmd -> Put)
  -> Get (SomeCommandId cmd)
  -> (forall status err result. TokCommand cmd status err result -> Get (CommandId cmd status err result))
  -> (forall status err result. TokCommand cmd status err result -> status -> Put)
  -> (forall status err result. TokCommand cmd status err result -> Get status)
  -> (forall status err result. TokCommand cmd status err result -> err -> Put)
  -> (forall status err result. TokCommand cmd status err result -> Get err)
  -> (forall status err result. TokCommand cmd status err result -> result -> Put)
  -> (forall status err result. TokCommand cmd status err result -> Get result)
  -> Codec (Command cmd) DeserializeError m LBS.ByteString
codecCommand putCmd getCmd putCmdId getSomeCmdId getCmdId putStatus getStatus putErr getErr putResult getResult =
  Codec (encodePut . putMsg) $ decodeGet . getMsg
  where
    putMsg
      :: forall (pr :: PeerRole) (st :: Command cmd) (st' :: Command cmd)
       . PeerHasAgency pr st
      -> Message (Command cmd) st st'
      -> Put
    putMsg = \case
      ClientAgency TokInit -> \case
        MsgExec cmd -> do
          putWord8 0x01
          putCmd $ SomeCommand cmd
        MsgResume cmdId -> do
          putWord8 0x02
          putCmdId $ SomeCommandId cmdId
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
          putCmdId $ SomeCommandId cmdId
      ClientAgency (TokAwait _) -> \case
        MsgPoll -> putWord8 0x06
        MsgDone -> putWord8 0x07

    getMsg
      :: forall (pr :: PeerRole) (st :: Command cmd)
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
            SomeCommandId cmdId <- getSomeCmdId
            pure $ SomeMessage $ MsgResume cmdId
          _ -> fail "Invalid protocol state for MsgResume"
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
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgDone
          _                         -> fail "Invalid protocol state for MsgDone"
        _ -> fail $ "Invalid msg tag " <> show tag

encodePut :: (a -> Put) -> a -> LBS.ByteString
encodePut = fmap runPut

decodeGet :: Applicative m => Get a -> m (DecodeStep LBS.ByteString DeserializeError m a)
decodeGet = go . runGetIncremental
  where
    go = pure . \case
      Fail unconsumedInput offset message -> DecodeFail DeserializeError{..}
      Partial f                           -> DecodePartial $ go . f . fmap LBS.toStrict
      Done unconsumedInput _ a            -> DecodeDone a $ mfilter (not . LBS.null) $ Just $ LBS.fromStrict unconsumedInput
