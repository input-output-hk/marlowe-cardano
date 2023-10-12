{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Defines the marlowe bulk sync protocol, which is a protocol for clients to synchronize with blocks of
-- marlowe-specific transaction information.
module Language.Marlowe.Protocol.BulkSync.Types where

import Control.Monad (join)
import Data.Binary (Binary (..), getWord8, putWord8)
import qualified Data.List.NonEmpty as NE
import Data.String (IsString (..))
import Data.Word (Word8)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.History.Api (
  MarloweBlock,
 )
import Network.Protocol.Codec (BinaryMessage (..), ShowProtocol)
import Network.Protocol.Codec.Spec (
  MessageEq (..),
  MessageVariations (..),
  SomePeerHasAgency (..),
  Variations (..),
  varyAp,
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Peer.Trace (MessageAttributes (..), OTelProtocol (..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))

-- | The states of the marlowe bulk sync client. This type is intended to be used as a kind.
data MarloweBulkSync where
  -- | The idle state, from which a client can request the next batch of blocks from the server, intersect, or terminate
  -- the session.
  StIdle :: MarloweBulkSync
  -- | The intersect state, during which the server is responding to an intersect request from the client.
  StIntersect :: MarloweBulkSync
  -- | The next state, during which the server is fetching the next batch of blocks for the client/
  StNext :: MarloweBulkSync
  -- | The poll state, when the client has reached the tip. The client can poll for new marlowe blocks in this state.
  StPoll :: MarloweBulkSync
  -- | The done state, when the session is done.
  StDone :: MarloweBulkSync

instance HasSignature MarloweBulkSync where
  signature _ = "MarloweBulkSync"

instance Protocol MarloweBulkSync where
  -- The messages that transition protocol states in the bulk sync protocol.
  data Message MarloweBulkSync st st' where
    -- Request the next batch of blocks from the server.
    MsgRequestNext
      -- The number of *extra* blocks to fetch (0 = fetch one block).
      :: Word8
      -> Message MarloweBulkSync 'StIdle 'StNext
    -- Send more blocks to the client.
    MsgRollForward
      -- The blocks, ordered by block number.
      :: [MarloweBlock]
      -- The current tip.
      -> BlockHeader
      -> Message MarloweBulkSync 'StNext 'StIdle
    -- Roll the client back to a previous point in the chain.
    MsgRollBackward
      -- The client's new point in the chain.
      :: ChainPoint
      -- The current tip.
      -> ChainPoint
      -> Message MarloweBulkSync 'StNext 'StIdle
    -- Inform the client they have reached the tip and should poll to receive new blocks.
    MsgWait :: Message MarloweBulkSync 'StNext 'StPoll
    -- Ask the server if more blocks are available yet.
    MsgPoll :: Message MarloweBulkSync 'StPoll 'StNext
    -- Cancel the poll, returning to the idle state.
    MsgCancel :: Message MarloweBulkSync 'StPoll 'StIdle
    -- Intersect the client's chain with the server's by finding the latest common point.
    MsgIntersect
      -- Points from the client's chain to try to intersect with the server.
      :: [BlockHeader]
      -> Message MarloweBulkSync 'StIdle 'StIntersect
    -- A common point was found between the client and the server. Syncing will begin from this point.
    MsgIntersectFound
      -- The point that was found.
      :: BlockHeader
      -- The current tip.
      -> BlockHeader
      -> Message MarloweBulkSync 'StIntersect 'StIdle
    -- No common points were found. Syncing will resume from the client's previous point.
    MsgIntersectNotFound
      -- The current tip.
      :: ChainPoint
      -> Message MarloweBulkSync 'StIntersect 'StIdle
    -- End the session.
    MsgDone :: Message MarloweBulkSync 'StIdle 'StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokPoll :: ClientHasAgency 'StPoll

  data ServerHasAgency st where
    TokIntersect :: ServerHasAgency 'StIntersect
    TokNext :: ServerHasAgency 'StNext

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokPoll = \case {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

instance BinaryMessage MarloweBulkSync where
  putMessage = \case
    ClientAgency TokIdle -> \case
      MsgRequestNext extraBlockCount -> do
        putWord8 0x01
        put extraBlockCount
      MsgIntersect blocks -> putWord8 0x02 *> put blocks
      MsgDone -> putWord8 0x03
    ServerAgency TokNext -> \case
      MsgRollForward block header -> do
        putWord8 0x04
        put block
        put header
      MsgRollBackward block tip -> do
        putWord8 0x05
        put block
        put tip
      MsgWait -> putWord8 0x06
    ClientAgency TokPoll -> \case
      MsgPoll -> putWord8 0x07
      MsgCancel -> putWord8 0x08
    ServerAgency TokIntersect -> \case
      MsgIntersectFound block tip -> do
        putWord8 0x09
        put block
        put tip
      MsgIntersectNotFound tip -> do
        putWord8 0x0a
        put tip

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokIdle -> SomeMessage . MsgRequestNext <$> get
        _ -> fail "Invalid protocol state for MsgRequestNext"
      0x02 -> case tok of
        ClientAgency TokIdle -> SomeMessage . MsgIntersect <$> get
        _ -> fail "Invalid protocol state for MsgRollForward"
      0x03 -> case tok of
        ClientAgency TokIdle -> pure $ SomeMessage MsgDone
        _ -> fail "Invalid protocol state for MsgDone"
      0x04 -> case tok of
        ServerAgency TokNext -> fmap SomeMessage . MsgRollForward <$> get <*> get
        _ -> fail "Invalid protocol state for MsgRollForward"
      0x05 -> case tok of
        ServerAgency TokNext -> fmap SomeMessage . MsgRollBackward <$> get <*> get
        _ -> fail "Invalid protocol state for MsgRollBackward"
      0x06 -> case tok of
        ServerAgency TokNext -> pure $ SomeMessage MsgWait
        _ -> fail "Invalid protocol state for MsgWait"
      0x07 -> case tok of
        ClientAgency TokPoll -> pure $ SomeMessage MsgPoll
        _ -> fail "Invalid protocol state for MsgPoll"
      0x08 -> case tok of
        ClientAgency TokPoll -> pure $ SomeMessage MsgCancel
        _ -> fail "Invalid protocol state for MsgCancel"
      0x09 -> case tok of
        ServerAgency TokIntersect -> fmap SomeMessage . MsgIntersectFound <$> get <*> get
        _ -> fail "Invalid protocol state for MsgIntersectFound"
      0x0a -> case tok of
        ServerAgency TokIntersect -> SomeMessage . MsgIntersectNotFound <$> get
        _ -> fail "Invalid protocol state for MsgIntersectNotFound"
      _ -> fail $ "Invalid message tag " <> show tag

deriving instance Show (Message MarloweBulkSync st st')
deriving instance Eq (Message MarloweBulkSync st st')
deriving instance Show (ClientHasAgency (st :: MarloweBulkSync))
deriving instance Eq (ClientHasAgency (st :: MarloweBulkSync))
deriving instance Show (ServerHasAgency (st :: MarloweBulkSync))
deriving instance Eq (ServerHasAgency (st :: MarloweBulkSync))
deriving instance Show (NobodyHasAgency (st :: MarloweBulkSync))
deriving instance Eq (NobodyHasAgency (st :: MarloweBulkSync))

instance MessageVariations MarloweBulkSync where
  agencyVariations =
    NE.fromList
      [ SomePeerHasAgency $ ClientAgency TokIdle
      , SomePeerHasAgency $ ClientAgency TokPoll
      , SomePeerHasAgency $ ServerAgency TokNext
      , SomePeerHasAgency $ ServerAgency TokIntersect
      ]
  messageVariations = \case
    ClientAgency TokIdle ->
      join $
        NE.fromList
          [ pure $ SomeMessage MsgDone
          , SomeMessage . MsgRequestNext <$> variations
          ]
    ClientAgency TokPoll -> NE.fromList [SomeMessage MsgPoll, SomeMessage MsgCancel]
    ServerAgency TokNext ->
      join $
        NE.fromList
          [ fmap SomeMessage . MsgRollForward <$> variations `varyAp` variations
          , fmap SomeMessage . MsgRollBackward <$> variations `varyAp` variations
          , pure $ SomeMessage MsgWait
          ]
    ServerAgency TokIntersect ->
      join $
        NE.fromList
          [ fmap SomeMessage . MsgIntersectFound <$> variations `varyAp` variations
          , SomeMessage . MsgIntersectNotFound <$> variations
          ]

instance MessageEq MarloweBulkSync where
  messageEq (AnyMessageAndAgency _ msg1) (AnyMessageAndAgency _ msg2) = case msg1 of
    MsgIntersect{} -> case msg2 of
      MsgIntersect{} -> msg1 == msg2
      _ -> False
    MsgDone -> case msg2 of
      MsgDone -> True
      _ -> False
    MsgRequestNext{} -> case msg2 of
      MsgRequestNext{} -> msg1 == msg2
      _ -> False
    MsgRollForward{} -> case msg2 of
      MsgRollForward{} -> msg1 == msg2
      _ -> False
    MsgRollBackward{} -> case msg2 of
      MsgRollBackward{} -> msg1 == msg2
      _ -> False
    MsgWait -> case msg2 of
      MsgWait -> True
      _ -> False
    MsgPoll -> case msg2 of
      MsgPoll -> True
      _ -> False
    MsgCancel -> case msg2 of
      MsgCancel -> True
      _ -> False
    MsgIntersectFound{} -> case msg2 of
      MsgIntersectFound{} -> msg1 == msg2
      _ -> False
    MsgIntersectNotFound{} -> case msg2 of
      MsgIntersectNotFound{} -> msg1 == msg2
      _ -> False

instance ShowProtocol MarloweBulkSync

instance OTelProtocol MarloweBulkSync where
  protocolName _ = "marlowe_bulk_sync"
  messageAttributes = \case
    ClientAgency tok -> case tok of
      TokIdle -> \case
        MsgRequestNext extraBlockCount ->
          MessageAttributes
            { messageType = "request_next"
            , messageParameters = fromString <$> [show extraBlockCount]
            }
        MsgIntersect blocks ->
          MessageAttributes
            { messageType = "intersect"
            , messageParameters = fromString . show <$> blocks
            }
        MsgDone ->
          MessageAttributes
            { messageType = "done"
            , messageParameters = []
            }
      TokPoll -> \case
        MsgPoll ->
          MessageAttributes
            { messageType = "poll"
            , messageParameters = []
            }
        MsgCancel ->
          MessageAttributes
            { messageType = "cancel"
            , messageParameters = []
            }
    ServerAgency tok -> case tok of
      TokNext -> \case
        MsgRollForward block headers ->
          MessageAttributes
            { messageType = "request_next/new_headers"
            , messageParameters = fromString <$> [show block, show headers]
            }
        MsgRollBackward block tip ->
          MessageAttributes
            { messageType = "request_next/roll_backward"
            , messageParameters = fromString <$> [show block, show tip]
            }
        MsgWait ->
          MessageAttributes
            { messageType = "request_next/wait"
            , messageParameters = []
            }
      TokIntersect -> \case
        MsgIntersectFound block tip ->
          MessageAttributes
            { messageType = "intersect/found"
            , messageParameters = fromString <$> [show block, show tip]
            }
        MsgIntersectNotFound tip ->
          MessageAttributes
            { messageType = "intersect/not_found"
            , messageParameters = fromString <$> [show tip]
            }
