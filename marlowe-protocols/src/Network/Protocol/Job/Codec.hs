{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Job.Codec
  where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Type.Equality (type (:~:)(Refl))
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.Protocol.Job.Types
import Network.TypedProtocol.Codec


codecJob
  :: forall cmd m
   . (Applicative m, Command cmd)
  => Codec (Job cmd) DeserializeError m LBS.ByteString
codecJob = binaryCodec putMsg getMsg
  where
    putMsg :: PutMessage (Job cmd)
    putMsg = \case
      ClientAgency TokInit -> \case
        MsgRequestHandshake schemaVersion -> do
          putWord8 0x01
          put schemaVersion
      ServerAgency TokHandshake -> \case
        MsgConfirmHandshake -> putWord8 0x02
        MsgRejectHandshake supportedVersion -> do
         putWord8 0x03
         put supportedVersion
      ClientAgency TokIdle -> \case
        MsgExec cmd -> do
          putWord8 0x04
          let tag = tagFromCommand cmd
          putTag tag
          putCommand cmd
        MsgAttach jobId -> do
          putWord8 0x05
          let tag = tagFromJobId jobId
          putTag tag
          putJobId jobId
        MsgDone -> do
          putWord8 0x06
      ServerAgency (TokCmd tag) -> \case
        MsgFail err -> do
          putWord8 0x07
          putTag tag
          putErr tag err
        MsgSucceed result -> do
          putWord8 0x08
          putTag tag
          putResult tag result
        MsgAwait status jobId -> do
          putWord8 0x09
          putTag tag
          putStatus tag status
          putJobId jobId
      ServerAgency (TokAttach _) -> \case
        MsgAttached     -> putWord8 0x0a
        MsgAttachFailed -> putWord8 0x0b
      ClientAgency (TokAwait _) -> \case
        MsgPoll   -> putWord8 0x0c
        MsgDetach -> putWord8 0x0d

    getMsg :: GetMessage (Job cmd)
    getMsg tok = do
      tag <- getWord8
      case tag of
        0x01 -> case tok of
          ClientAgency TokInit -> do
            SomeMessage . MsgRequestHandshake <$> get
          _ -> fail "Invalid protocol state for MsgRequestHandshake"
        0x02 -> case tok of
          ServerAgency TokHandshake -> pure $ SomeMessage MsgConfirmHandshake
          _                         -> fail "Invalid protocol state for MsgConfirmHandshake"
        0x03 -> case tok of
          ServerAgency TokHandshake -> SomeMessage . MsgRejectHandshake <$> get
          _                         -> fail "Invalid protocol state for MsgRejectHandshake"
        0x04 -> case tok of
          ClientAgency TokIdle -> do
            SomeTag ctag <- getTag
            SomeMessage . MsgExec <$> getCommand ctag
          _ -> fail "Invalid protocol state for MsgExec"
        0x05 -> case tok of
          ClientAgency TokIdle -> do
            SomeTag ctag <- getTag
            SomeMessage . MsgAttach <$> getJobId ctag
          _ -> fail "Invalid protocol state for MsgAttach"
        0x06 -> case tok of
          ClientAgency TokIdle -> pure . SomeMessage $ MsgDone
          _                         -> fail "Invalid protocol state for MsgDone"
        0x07 -> case tok of
          ServerAgency (TokCmd ctag) -> do
            SomeTag ctag' <- getTag
            case tagEq ctag ctag' of
              Nothing                 -> fail "decoded command tag does not match expected command tag"
              Just (Refl, Refl, Refl) -> SomeMessage . MsgFail <$> getErr ctag'
          _ -> fail "Invalid protocol state for MsgFail"
        0x08 -> case tok of
          ServerAgency (TokCmd ctag) -> do
            SomeTag ctag' <- getTag
            case tagEq ctag ctag' of
              Nothing                 -> fail "decoded command tag does not match expected command tag"
              Just (Refl, Refl, Refl) -> SomeMessage . MsgSucceed <$> getResult ctag'
          _ -> fail "Invalid protocol state for MsgSucceed"
        0x09 -> case tok of
          ServerAgency (TokCmd ctag) -> do
            SomeTag ctag' <- getTag
            case tagEq ctag ctag' of
              Nothing                 -> fail "decoded command tag does not match expected command tag"
              Just (Refl, Refl, Refl) -> SomeMessage <$> (MsgAwait <$> getStatus ctag' <*> getJobId ctag')
          _ -> fail "Invalid protocol state for MsgAwait"
        0x0a -> case tok of
          ServerAgency (TokAttach _) -> pure $ SomeMessage MsgAttached
          _                          -> fail "Invalid protocol state for MsgAttached"
        0x0b -> case tok of
          ServerAgency (TokAttach _) -> pure $ SomeMessage MsgAttachFailed
          _                          -> fail "Invalid protocol state for MsgAttachFailed"
        0x0c -> case tok of
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgPoll
          _                         -> fail "Invalid protocol state for MsgPoll"
        0x0d -> case tok of
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgDetach
          _                         -> fail "Invalid protocol state for MsgDetach"
        _ -> fail $ "Invalid msg tag " <> show tag

