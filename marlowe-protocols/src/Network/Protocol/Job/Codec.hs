{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}

module Network.Protocol.Job.Codec where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Type.Equality (type (:~:)(Refl))
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.Protocol.Job.Types
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

codecJob
  :: forall cmd m
   . (Applicative m, Command cmd)
  => Codec (Job cmd) DeserializeError m LBS.ByteString
codecJob = binaryCodec putMsg getMsg
  where
    putMsg :: PutMessage (Job cmd)
    putMsg = \case
      ClientAgency TokInit -> \case
        MsgExec cmd -> do
          putWord8 0x01
          let tag = tagFromCommand cmd
          putTag tag
          putCommand cmd
        MsgAttach jobId -> do
          putWord8 0x02
          let tag = tagFromJobId jobId
          putTag tag
          putJobId jobId
      ServerAgency (TokCmd tag) -> \case
        MsgFail err -> do
          putWord8 0x03
          putTag (coerceTag tag)
          putErr (coerceTag tag) err
        MsgSucceed result -> do
          putWord8 0x04
          putTag (coerceTag tag)
          putResult (coerceTag tag) result
        MsgAwait status jobId -> do
          putWord8 0x05
          putTag (coerceTag tag)
          putStatus (coerceTag tag) status
          putJobId jobId
      ClientAgency (TokAwait _) -> \case
        MsgPoll   -> putWord8 0x06
        MsgDetach -> putWord8 0x07

    getMsg :: GetMessage (Job cmd)
    getMsg tok = do
      tag <- getWord8
      case tag of
        0x01 -> case tok of
          ClientAgency TokInit -> do
            SomeTag ctag <- getTag
            SomeMessage . MsgExec <$> getCommand ctag
          _ -> fail "Invalid protocol state for MsgExec"
        0x02 -> case tok of
          ClientAgency TokInit -> do
            SomeTag ctag <- getTag
            SomeMessage . MsgAttach <$> getJobId ctag
          _ -> fail "Invalid protocol state for MsgAttach"
        0x03 -> case tok of
          ServerAgency (TokCmd ctag) -> do
            SomeTag ctag' <- getTag
            case tagEq (coerceTag ctag) ctag' of
              Nothing                 -> fail "decoded command tag does not match expected command tag"
              Just (Refl, Refl, Refl) -> SomeMessage . MsgFail <$> getErr ctag'
          _ -> fail "Invalid protocol state for MsgFail"
        0x04 -> case tok of
          ServerAgency (TokCmd ctag) -> do
            SomeTag ctag' <- getTag
            case tagEq (coerceTag ctag) ctag' of
              Nothing                 -> fail "decoded command tag does not match expected command tag"
              Just (Refl, Refl, Refl) -> SomeMessage . MsgSucceed <$> getResult ctag'
          _ -> fail "Invalid protocol state for MsgSucceed"
        0x05 -> case tok of
          ServerAgency (TokCmd ctag) -> do
            SomeTag ctag' <- getTag
            case tagEq (coerceTag ctag) ctag' of
              Nothing                 -> fail "decoded command tag does not match expected command tag"
              Just (Refl, Refl, Refl) -> SomeMessage <$> (MsgAwait <$> getStatus ctag' <*> getJobId ctag')
          _ -> fail "Invalid protocol state for MsgAwait"
        0x06 -> case tok of
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgPoll
          _                         -> fail "Invalid protocol state for MsgPoll"
        0x07 -> case tok of
          ClientAgency (TokAwait _) -> pure $ SomeMessage MsgDetach
          _                         -> fail "Invalid protocol state for MsgDetach"
        _ -> fail $ "Invalid msg tag " <> show tag

    -- Unfortunately, the poly-kinded cmd parameter doesn't play nicely with
    -- the `PeerHasAgency` type and it gets confused, thinking that cmd1
    -- and 'cmd' are unrelated types. So we have to coerce them (they will
    -- absolutely be the same type constructor though).
    coerceTag :: forall cmd1 status err result. Tag cmd1 status err result -> Tag cmd status err result
    coerceTag = unsafeCoerce
