{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Query.Codec
  where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Type.Equality (type (:~:)(Refl))
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.Protocol.Query.Types
import Network.TypedProtocol.Codec

codecQuery
  :: forall query m
   . (Applicative m, IsQuery query)
  => Codec (Query query) DeserializeError m LBS.ByteString
codecQuery = binaryCodec putMsg getMsg
  where
    putMsg :: PutMessage (Query query)
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
        MsgDone -> putWord8 0x04
        MsgRequest query -> do
          putWord8 0x05
          putTag (tagFromQuery query)
          putQuery query
      ServerAgency (TokNext _ tag) -> \case
        MsgReject err -> do
          putWord8 0x06
          putTag tag
          putErr tag err
        MsgNextPage results delimiter -> do
          putWord8 0x07
          putTag tag
          putResult tag results
          case delimiter of
            Nothing -> putWord8 0x01
            Just d  -> putDelimiter tag d
      ClientAgency (TokPage tag) -> \case
        MsgRequestNext delimiter -> do
          putWord8 0x08
          putTag tag
          putDelimiter tag delimiter
        MsgRequestDone -> putWord8 0x09

    getMsg :: GetMessage (Query query)
    getMsg tok = do
      tag <- getWord8
      case tag of
        0x01 -> case tok of
          ClientAgency TokInit -> do
            expectedVersion <- get
            pure . SomeMessage . MsgRequestHandshake $ expectedVersion
          _ -> fail "Invalid protocol state for MsgRequestHandshake"
        0x02 -> case tok of
          ServerAgency TokHandshake -> pure . SomeMessage $ MsgConfirmHandshake
          _ -> fail "Invalid protocol state for MsgConfirmHanshake"
        0x03 -> case tok of
          ServerAgency TokHandshake -> do
            supportedVersion <- get
            pure . SomeMessage . MsgRejectHandshake $ supportedVersion
          _ -> fail "Invalid protocol state for MsgRejectHandshake"
        0x04 -> case tok of
          ClientAgency TokIdle -> pure . SomeMessage $ MsgDone
          _ -> fail "Invalid protocol state for MsgDone"
        0x05 -> case tok of
          ClientAgency TokIdle -> do
            SomeTag qtag <- getTag
            SomeMessage . MsgRequest <$> getQuery qtag
          _ -> fail "Invalid protocol state for MsgRequest"
        0x06 -> case tok of
          ServerAgency (TokNext TokCanReject qtag) -> do
            SomeTag qtag' :: SomeTag query <- getTag
            case tagEq qtag qtag' of
              Nothing                 -> fail "decoded query tag does not match expected query tag"
              Just (Refl, Refl, Refl) -> SomeMessage . MsgReject <$> getErr qtag'
          _ -> fail "Invalid protocol state for MsgReject"
        0x07 -> case tok of
          ServerAgency (TokNext _ qtag) -> do
            SomeTag qtag' :: SomeTag query <- getTag
            case tagEq qtag qtag' of
              Nothing   -> fail "decoded query tag does not match expected query tag"
              Just (Refl, Refl, Refl) -> do
                result <- getResult qtag'
                maybeTag <- getWord8
                delimiter <- case maybeTag of
                  0x01 -> pure Nothing
                  0x02 -> Just <$> getDelimiter qtag'
                  _    -> fail $ "Invalid maybe tag: " <> show maybeTag
                pure $ SomeMessage $ MsgNextPage result delimiter
          _ -> fail "Invalid protocol state for MsgNextPage"
        0x08 -> case tok of
          ClientAgency (TokPage qtag) -> do
            SomeTag qtag' :: SomeTag query <- getTag
            case tagEq qtag qtag' of
              Nothing                 -> fail "decoded query tag does not match expected query tag"
              Just (Refl, Refl, Refl) -> SomeMessage . MsgRequestNext <$> getDelimiter qtag'
          _                            -> fail "Invalid protocol state for MsgRequestNext"
        0x09 -> case tok of
          ClientAgency (TokPage _) -> pure $ SomeMessage MsgRequestDone
          _                        -> fail "Invalid protocol state for MsgDone"
        _ -> fail $ "Invalid msg tag " <> show tag

