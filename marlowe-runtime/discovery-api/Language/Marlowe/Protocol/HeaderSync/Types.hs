{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.HeaderSync.Types
  where

import Control.Monad (join)
import Data.Aeson (Value(..), object, (.=))
import Data.Binary (get, put, putWord8)
import Data.Binary.Get (getWord8)
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Codec.Spec (MessageVariations(..), SomePeerHasAgency(SomePeerHasAgency), Variations(..), varyAp)
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol (PeerHasAgency(..), Protocol(..))
import Network.TypedProtocol.Codec (SomeMessage(..))
import Observe.Event.Network.Protocol (MessageToJSON(..))

data MarloweHeaderSync where
  StIdle :: MarloweHeaderSync
  StIntersect :: MarloweHeaderSync
  StNext :: MarloweHeaderSync
  StWait :: MarloweHeaderSync
  StDone :: MarloweHeaderSync

instance HasSignature MarloweHeaderSync where
  signature _ = "MarloweHeaderSync"

instance Protocol MarloweHeaderSync where
  data Message MarloweHeaderSync from to where
    MsgIntersect :: [BlockHeader] -> Message MarloweHeaderSync
      'StIdle
      'StIntersect
    MsgDone :: Message MarloweHeaderSync
      'StIdle
      'StDone
    MsgRequestNext :: Message MarloweHeaderSync
      'StIdle
      'StNext
    MsgNewHeaders :: BlockHeader -> [ContractHeader] -> Message MarloweHeaderSync
      'StNext
      'StIdle
    MsgRollBackward :: ChainPoint -> Message MarloweHeaderSync
      'StNext
      'StIdle
    MsgWait :: Message MarloweHeaderSync
      'StNext
      'StWait
    MsgPoll :: Message MarloweHeaderSync
      'StWait
      'StNext
    MsgCancel :: Message MarloweHeaderSync
      'StWait
      'StIdle
    MsgIntersectFound :: BlockHeader -> Message MarloweHeaderSync
      'StIntersect
      'StIdle
    MsgIntersectNotFound :: Message MarloweHeaderSync
      'StIntersect
      'StIdle

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokWait :: ClientHasAgency 'StWait

  data ServerHasAgency st where
    TokNext :: ServerHasAgency 'StNext
    TokIntersect :: ServerHasAgency 'StIntersect

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case
  exclusionLemma_ClientAndServerHaveAgency TokWait = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

instance BinaryMessage MarloweHeaderSync where
  putMessage = \case
    ClientAgency TokIdle -> \case
      MsgRequestNext -> putWord8 0x01
      MsgIntersect blocks -> putWord8 0x02 *> put blocks
      MsgDone -> putWord8 0x03

    ServerAgency TokNext -> \case
      MsgNewHeaders block headers -> do
        putWord8 0x04
        put block
        put headers
      MsgRollBackward block -> do
        putWord8 0x05
        put block

      MsgWait -> putWord8 0x06

    ClientAgency TokWait -> \case
      MsgPoll -> putWord8 0x07
      MsgCancel -> putWord8 0x08

    ServerAgency TokIntersect -> \case
      MsgIntersectFound block -> do
        putWord8 0x09
        put block
      MsgIntersectNotFound -> putWord8 0x0a

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokIdle -> pure $ SomeMessage MsgRequestNext
        _                        -> fail "Invalid protocol state for MsgRequestNext"

      0x02 -> case tok of
        ClientAgency TokIdle -> SomeMessage .MsgIntersect <$> get
        _ -> fail "Invalid protocol state for MsgNewHeaders"

      0x03 -> case tok of
        ClientAgency TokIdle -> pure $ SomeMessage MsgDone
        _                        -> fail "Invalid protocol state for MsgDone"

      0x04 -> case tok of
        ServerAgency TokNext -> do
          block <- get
          SomeMessage . MsgNewHeaders block <$> get
        _ -> fail "Invalid protocol state for MsgNewHeaders"

      0x05 -> case tok of
        ServerAgency TokNext -> SomeMessage . MsgRollBackward <$> get
        _                        -> fail "Invalid protocol state for MsgRollBackward"

      0x06 -> case tok of
        ServerAgency TokNext -> pure $ SomeMessage MsgWait
        _                        -> fail "Invalid protocol state for MsgWait"

      0x07 -> case tok of
        ClientAgency TokWait -> pure $ SomeMessage MsgPoll
        _                        -> fail "Invalid protocol state for MsgPoll"

      0x08 -> case tok of
        ClientAgency TokWait -> pure $ SomeMessage MsgCancel
        _                        -> fail "Invalid protocol state for MsgCancel"

      0x09 -> case tok of
        ServerAgency TokIntersect -> SomeMessage . MsgIntersectFound <$> get
        _                             -> fail "Invalid protocol state for MsgIntersectFound"

      0x0a -> case tok of
        ServerAgency TokIntersect -> pure $ SomeMessage MsgIntersectNotFound
        _                             -> fail "Invalid protocol state for MsgIntersectNotFound"

      _ -> fail $ "Invalid message tag " <> show tag

instance MessageVariations MarloweHeaderSync where
  agencyVariations = NE.fromList
    [ SomePeerHasAgency $ ClientAgency TokIdle
    , SomePeerHasAgency $ ClientAgency TokWait
    , SomePeerHasAgency $ ServerAgency TokNext
    , SomePeerHasAgency $ ServerAgency TokIntersect
    ]
  messageVariations = \case
    ClientAgency TokIdle -> NE.fromList [SomeMessage MsgDone, SomeMessage MsgRequestNext]
    ClientAgency TokWait -> NE.fromList [SomeMessage MsgPoll, SomeMessage MsgCancel]
    ServerAgency TokNext -> join $ NE.fromList
      [ fmap SomeMessage $ MsgNewHeaders <$> variations `varyAp` variations
      , SomeMessage . MsgRollBackward <$> variations
      , pure $ SomeMessage MsgWait
      ]
    ServerAgency TokIntersect -> join $ NE.fromList
      [ SomeMessage . MsgIntersectFound <$> variations
      , pure $ SomeMessage MsgIntersectNotFound
      ]


instance MessageToJSON MarloweHeaderSync where
  messageToJSON = \case
    ClientAgency TokIdle -> \case
      MsgDone -> String "done"
      MsgRequestNext -> String "request-next"
      MsgIntersect headers -> object [ "intersect" .= headers ]
    ClientAgency TokWait -> \case
      MsgPoll -> String "poll"
      MsgCancel -> String "cancel"
    ServerAgency TokNext -> \case
      MsgNewHeaders blockHeader headers -> object
        [ "new-headers" .= object
          [ "block-header" .= blockHeader
          , "contract-headers" .= headers
          ]
        ]
      MsgRollBackward blockHeader -> object [ "roll-backward" .= blockHeader ]
      MsgWait -> String "wait"
    ServerAgency TokIntersect -> \case
      MsgIntersectFound blockHeader -> object [ "intersect-found" .= blockHeader ]
      MsgIntersectNotFound -> String "intersect-not-found"
