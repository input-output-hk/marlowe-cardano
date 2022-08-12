{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Network.Protocol.Query.Codec where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.Codec (DeserializeError, decodeGet, encodePut)
import Network.Protocol.Query.Types
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

data SomeQuery query = forall delimiter err result. SomeQuery (query delimiter err result)

codecQuery
  :: forall query m
   . Applicative m
  => (SomeQuery query -> Put)
  -> Get (SomeQuery query)
  -> (forall delimiter err result. query delimiter err result -> delimiter -> Put)
  -> (forall delimiter err result. query delimiter err result -> Get delimiter)
  -> (forall delimiter err result. query delimiter err result -> err -> Put)
  -> (forall delimiter err result. query delimiter err result -> Get err)
  -> (forall delimiter err result. query delimiter err result -> result -> Put)
  -> (forall delimiter err result. query delimiter err result -> Get result)
  -> Codec (Query query) DeserializeError m LBS.ByteString
codecQuery putCmd getCmd putDelimiter getDelimiter putErr getErr putResult getResult =
  Codec (encodePut . putMsg) $ decodeGet . getMsg
  where
    putMsg
      :: forall (pr :: PeerRole) (st :: Query query) (st' :: Query query)
       . PeerHasAgency pr st
      -> Message (Query query) st st'
      -> Put
    putMsg = \case
      ClientAgency TokInit -> \case
        MsgRequest query -> do
          putWord8 0x01
          putCmd $ SomeQuery query
      ServerAgency (TokNext _ _) -> \case
        MsgReject query err -> do
          putWord8 0x02
          putErr query err
        MsgNextPage query result delimiter -> do
          putWord8 0x03
          putResult query result
          case delimiter of
            Nothing -> putWord8 0x01
            Just d  -> putDelimiter query d
      ClientAgency (TokPage _) -> \case
        MsgRequestNext query delimiter -> do
          putWord8 0x04
          putDelimiter query delimiter
        MsgDone -> putWord8 0x05

    getMsg
      :: forall (pr :: PeerRole) (st :: Query query)
       . PeerHasAgency pr st
      -> Get (SomeMessage st)
    getMsg tok = do
      tag <- getWord8
      case tag of
        0x01 -> case tok of
          ClientAgency TokInit -> do
            SomeQuery query <- getCmd
            pure $ SomeMessage $ MsgRequest query
          _ -> fail "Invalid protocol state for MsgRequest"
        0x02 -> case tok of
          ServerAgency (TokNext TokCanReject query) -> coerceNext <$> getReject (coerceQuery query)
          _                                         -> fail "Invalid protocol state for MsgReject"
        0x03 -> case tok of
          ServerAgency (TokNext _ query) -> coerceNext <$> getNextPage (coerceQuery query)
          _                              -> fail "Invalid protocol state for MsgNextPage"
        0x04 -> case tok of
          ClientAgency (TokPage query) -> coercePage <$> getRequestNext (coerceQuery query)
          _                            -> fail "Invalid protocol state for MsgAwait"
        0x05 -> case tok of
          ClientAgency (TokPage _) -> pure $ SomeMessage MsgDone
          _                        -> fail "Invalid protocol state for MsgRequetNext"
        _ -> fail $ "Invalid msg tag " <> show tag

    getReject
      :: forall (st :: Query query) delimiter err results
       . st ~ 'StNext 'CanReject delimiter err results
      => query delimiter err results
      -> Get (SomeMessage st)
    getReject query = do
      err <- getErr query
      pure $ SomeMessage $ MsgReject query err

    getNextPage
      :: forall (st :: Query query) k delimiter err results
       . st ~ 'StNext k delimiter err results
      => query delimiter err results
      -> Get (SomeMessage st)
    getNextPage query = do
      result <- getResult query
      maybeTag <- getWord8
      delimiter <- case maybeTag of
        0x01 -> pure Nothing
        0x02 -> Just <$> getDelimiter query
        _    -> fail $ "Invalid maybe tag: " <> show maybeTag
      pure $ SomeMessage $ MsgNextPage query result delimiter

    getRequestNext
      :: forall (st :: Query query) delimiter err results
       . st ~ 'StPage delimiter err results
      => query delimiter err results
      -> Get (SomeMessage st)
    getRequestNext query = do
      delimiter <- getDelimiter query
      pure $ SomeMessage $ MsgRequestNext query delimiter

    -- Unfortunately, PeerAgency does not appear to work well with protocols
    -- that are parameterized by higher-kinded variables like `query` when they
    -- are existentially quantified in the GADT constructors. The kind
    -- parameters get instantiated twice, hence the compiler is unable to unify
    -- `delimiter0 ~ delimiter1` etc... Some unsafe coercing is required. If
    -- someone knows how to solve this problem, a fix would be greatly
    -- appreciated!
    --
    -- If we were working with concrete types for `query`, this wouldn't be an
    -- issue, because we could introduce the neccessary type equallity
    -- constraints into scope via GADT pattern matching.
    coerceNext
      :: SomeMessage ('StNext k delimiter0 err0 results)
      -> SomeMessage ('StNext k delimiter1 err1 resutls)
    coerceNext = unsafeCoerce

    coercePage
      :: SomeMessage ('StPage delimiter0 err0 results)
      -> SomeMessage ('StPage delimiter1 err1 resutls)
    coercePage = unsafeCoerce

    coerceQuery :: query1 delimiter1 err1 results1 -> query delimiter0 err0 results0
    coerceQuery = unsafeCoerce
