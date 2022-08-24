{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Network.Protocol.Query.Codec where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.Protocol.Query.Types
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

codecQuery
  :: forall query m
   . (Applicative m, IsQuery query)
  => Codec (Query query) DeserializeError m LBS.ByteString
codecQuery = binaryCodec putMsg getMsg
  where
    putMsg :: PutMessage (Query query)
    putMsg = \case
      ClientAgency TokInit -> \case
        MsgRequest query -> do
          putWord8 0x01
          putTag (tagFromQuery query)
          putQuery query
      ServerAgency (TokNext _ tag) -> \case
        MsgReject err -> do
          putWord8 0x02
          putTag (coerceTag tag)
          putErr (coerceTag tag) err
        MsgNextPage results delimiter -> do
          putWord8 0x03
          putTag (coerceTag tag)
          putResult (coerceTag tag) results
          case delimiter of
            Nothing -> putWord8 0x01
            Just d  -> putDelimiter (coerceTag tag) d
      ClientAgency (TokPage tag) -> \case
        MsgRequestNext delimiter -> do
          putWord8 0x04
          putTag (coerceTag tag)
          putDelimiter (coerceTag tag) delimiter
        MsgDone -> putWord8 0x05

    getMsg :: GetMessage (Query query)
    getMsg tok = do
      tag <- getWord8
      case tag of
        0x01 -> case tok of
          ClientAgency TokInit -> do
            SomeTag qtag <- getTag
            SomeMessage . MsgRequest <$> getQuery qtag
          _ -> fail "Invalid protocol state for MsgRequest"
        0x02 -> case tok of
          ServerAgency (TokNext TokCanReject qtag) -> do
            SomeTag qtag' :: SomeTag query <- getTag
            case tagEq (coerceTag qtag) qtag' of
              Nothing   -> fail "decoded query tag does not match expected query tag"
              Just Refl -> SomeMessage . MsgReject <$> getErr qtag'
          _ -> fail "Invalid protocol state for MsgReject"
        0x03 -> case tok of
          ServerAgency (TokNext _ qtag) -> do
            SomeTag qtag' :: SomeTag query <- getTag
            case tagEq (coerceTag qtag) qtag' of
              Nothing   -> fail "decoded query tag does not match expected query tag"
              Just Refl -> do
                result <- getResult qtag'
                maybeTag <- getWord8
                delimiter <- case maybeTag of
                  0x01 -> pure Nothing
                  0x02 -> Just <$> getDelimiter qtag'
                  _    -> fail $ "Invalid maybe tag: " <> show maybeTag
                pure $ SomeMessage $ MsgNextPage result delimiter
          _ -> fail "Invalid protocol state for MsgNextPage"
        0x04 -> case tok of
          ClientAgency (TokPage qtag) -> do
            SomeTag qtag' :: SomeTag query <- getTag
            case tagEq (coerceTag qtag) qtag' of
              Nothing   -> fail "decoded query tag does not match expected query tag"
              Just Refl -> SomeMessage . MsgRequestNext <$> getDelimiter qtag'
          _                            -> fail "Invalid protocol state for MsgRequestNext"
        0x05 -> case tok of
          ClientAgency (TokPage _) -> pure $ SomeMessage MsgDone
          _                        -> fail "Invalid protocol state for MsgDone"
        _ -> fail $ "Invalid msg tag " <> show tag

    -- Unfortunately, the poly-kinded query parameter doesn't play nicely with
    -- the `PeerHasAgency` type and it gets confused, thinking that 'query1'
    -- and 'query' are unrelated types. So we have to coerce them (they will
    -- absolutely be the same type constructor though).
    coerceTag :: forall query1 delimiter err results. Tag query1 delimiter err results -> Tag query delimiter err results
    coerceTag = unsafeCoerce
