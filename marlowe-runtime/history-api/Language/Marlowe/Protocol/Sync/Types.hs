{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Sync.Types
  where

import Control.Monad (join)
import Data.Aeson (Key, ToJSON, Value(..), object, (.=))
import Data.Binary (get, getWord8, put, putWord8)
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersion(..), MarloweVersionTag, SomeMarloweVersion(..))
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Codec.Spec (MessageVariations(..), SomePeerHasAgency(SomePeerHasAgency), Variations(..), varyAp)
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol (PeerHasAgency(..), Protocol(..), SomeMessage(..))
import Observe.Event.Network.Protocol (MessageToJSON(..))

data MarloweSync where
  StInit :: MarloweSync
  StFollow :: MarloweSync
  StDone :: MarloweSync
  StIdle :: MarloweVersionTag -> MarloweSync
  StNext :: MarloweVersionTag -> MarloweSync
  StWait :: MarloweVersionTag -> MarloweSync
  StIntersect :: MarloweVersionTag -> MarloweSync

instance HasSignature MarloweSync where
  signature _ = "MarloweSync"

instance Protocol MarloweSync where
  data Message MarloweSync from to where
    MsgFollowContract :: ContractId -> Message MarloweSync
      'StInit
      'StFollow
    MsgIntersect :: ContractId -> MarloweVersion v -> [BlockHeader] -> Message MarloweSync
      'StInit
      ('StIntersect v)
    MsgContractNotFound :: Message MarloweSync
      'StFollow
      'StDone
    MsgContractFound :: BlockHeader -> MarloweVersion v -> CreateStep v -> Message MarloweSync
      'StFollow
      ('StIdle v)
    MsgDone :: Message MarloweSync
      ('StIdle v)
      'StDone
    MsgRequestNext :: Message MarloweSync
      ('StIdle v)
      ('StNext v)
    MsgRollForward :: BlockHeader -> [ContractStep v] -> Message MarloweSync
      ('StNext v)
      ('StIdle v)
    MsgRollBackward :: BlockHeader -> Message MarloweSync
      ('StNext v)
      ('StIdle v)
    MsgRollBackCreation :: Message MarloweSync
      ('StNext v)
      'StDone
    MsgWait :: Message MarloweSync
      ('StNext v)
      ('StWait v)
    MsgPoll :: Message MarloweSync
      ('StWait v)
      ('StNext v)
    MsgCancel :: Message MarloweSync
      ('StWait v)
      ('StIdle v)
    MsgIntersectFound :: BlockHeader -> Message MarloweSync
      ('StIntersect v)
      ('StIdle v)
    MsgIntersectNotFound :: Message MarloweSync
      ('StIntersect v)
      'StDone

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokIdle :: MarloweVersion v -> ClientHasAgency ('StIdle v)
    TokWait :: MarloweVersion v -> ClientHasAgency ('StWait v)

  data ServerHasAgency st where
    TokFollow :: ServerHasAgency 'StFollow
    TokNext :: MarloweVersion v -> ServerHasAgency ('StNext v)
    TokIntersect :: MarloweVersion v -> ServerHasAgency ('StIntersect v)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit     = \case
  exclusionLemma_ClientAndServerHaveAgency (TokIdle _) = \case
  exclusionLemma_ClientAndServerHaveAgency (TokWait _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

instance BinaryMessage MarloweSync where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgFollowContract contractId -> do
        putWord8 0x01
        put contractId
      MsgIntersect contractId version blockHeaders -> do
        putWord8 0x02
        put contractId
        put $ SomeMarloweVersion version
        put blockHeaders

    ServerAgency TokFollow -> \case
      MsgContractNotFound -> putWord8 0x03
      MsgContractFound blockHeader version createStep -> do
        putWord8 0x04
        put blockHeader
        put $ SomeMarloweVersion version
        case version of
          MarloweV1 -> put createStep

    ClientAgency (TokIdle _) -> \case
      MsgRequestNext -> putWord8 0x05
      MsgDone        -> putWord8 0x06

    ServerAgency (TokNext version) -> \case
      MsgRollForward blockHeader contractSteps -> do
        putWord8 0x07
        put blockHeader
        case version of
          MarloweV1 -> put contractSteps
      MsgRollBackward blockHeader -> do
        putWord8 0x08
        put blockHeader
      MsgRollBackCreation -> putWord8 0x0e

      MsgWait -> putWord8 0x09

    ClientAgency (TokWait _) -> \case
      MsgPoll   -> putWord8 0x0a
      MsgCancel -> putWord8 0x0b

    ServerAgency (TokIntersect _) -> \case
      MsgIntersectFound blockHeader -> do
        putWord8 0x0c
        put blockHeader
      MsgIntersectNotFound -> putWord8 0x0d

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> SomeMessage . MsgFollowContract <$> get
        _                    -> fail "Invalid protocol state for MsgFollowContract"

      0x02 -> case tok of
        ClientAgency TokInit -> do
          contractId <- get
          SomeMarloweVersion version <- get
          SomeMessage . MsgIntersect contractId version <$> get
        _                        -> fail "Invalid protocol state for MsgIntersect"

      0x03 -> case tok of
        ServerAgency TokFollow -> pure $ SomeMessage MsgContractNotFound
        _                      -> fail "Invalid protocol state for MsgContractNotFound"

      0x04 -> case tok of
        ServerAgency TokFollow -> do
          blockHeader <- get @BlockHeader
          SomeMarloweVersion version <- get
          case version of
            MarloweV1 -> SomeMessage . MsgContractFound blockHeader version <$> get
        _ -> fail "Invalid protocol state for MsgContractFound"

      0x05 -> case tok of
        ClientAgency (TokIdle _) -> pure $ SomeMessage MsgRequestNext
        _                        -> fail "Invalid protocol state for MsgRequestNext"

      0x06 -> case tok of
        ClientAgency (TokIdle _) -> pure $ SomeMessage MsgDone
        _                        -> fail "Invalid protocol state for MsgDone"

      0x07 -> case tok of
        ServerAgency (TokNext version) -> do
          blockHeader <- get @BlockHeader
          case version of
            MarloweV1 -> SomeMessage . MsgRollForward blockHeader <$> get
        _ -> fail "Invalid protocol state for MsgRollForward"

      0x08 -> case tok of
        ServerAgency (TokNext _) -> SomeMessage . MsgRollBackward <$> get
        _                        -> fail "Invalid protocol state for MsgRollBackward"

      0x09 -> case tok of
        ServerAgency (TokNext _) -> pure $ SomeMessage MsgWait
        _                        -> fail "Invalid protocol state for MsgWait"

      0x0a -> case tok of
        ClientAgency (TokWait _) -> pure $ SomeMessage MsgPoll
        _                        -> fail "Invalid protocol state for MsgPoll"

      0x0b -> case tok of
        ClientAgency (TokWait _) -> pure $ SomeMessage MsgCancel
        _                        -> fail "Invalid protocol state for MsgCancel"

      0x0c -> case tok of
        ServerAgency (TokIntersect _) -> SomeMessage . MsgIntersectFound <$> get
        _                             -> fail "Invalid protocol state for MsgIntersectFound"

      0x0d -> case tok of
        ServerAgency (TokIntersect _) -> pure $ SomeMessage MsgIntersectNotFound
        _                             -> fail "Invalid protocol state for MsgIntersectNotFound"

      0x0e -> case tok of
        ServerAgency (TokNext _) -> pure $ SomeMessage MsgRollBackCreation
        _                        -> fail "Invalid protocol state for MsgRollBackCreation"

      _ -> fail $ "Invalid message tag " <> show tag

instance MessageVariations MarloweSync where
  agencyVariations = join $ NE.fromList
    [ pure $ SomePeerHasAgency $ ClientAgency TokInit
    , pure $ SomePeerHasAgency $ ServerAgency TokFollow
    , do
        SomeMarloweVersion v <- variations
        pure $ SomePeerHasAgency $ ClientAgency $ TokIdle v
    , do
        SomeMarloweVersion v <- variations
        pure $ SomePeerHasAgency $ ClientAgency $ TokWait v
    , do
        SomeMarloweVersion v <- variations
        pure $ SomePeerHasAgency $ ServerAgency $ TokNext v
    , do
        SomeMarloweVersion v <- variations
        pure $ SomePeerHasAgency $ ServerAgency $ TokIntersect v
    ]
  messageVariations = \case
    ClientAgency TokInit -> join $ NE.fromList
      [ SomeMessage . MsgFollowContract <$> variations
      , do
          SomeMarloweVersion v <- variations
          fmap SomeMessage $ MsgIntersect <$> variations <*> pure v `varyAp` variations
      ]
    ServerAgency TokFollow -> join $ NE.fromList
      [ pure $ SomeMessage MsgContractNotFound
      , do
          SomeMarloweVersion MarloweV1 <- variations
          fmap SomeMessage $ MsgContractFound <$> variations <*> pure MarloweV1 `varyAp` variations
      ]
    ClientAgency (TokIdle _) -> NE.fromList [SomeMessage MsgDone, SomeMessage MsgRequestNext]
    ClientAgency (TokWait _) -> NE.fromList [SomeMessage MsgPoll, SomeMessage MsgCancel]
    ServerAgency (TokNext MarloweV1) -> join $ NE.fromList
      [ fmap SomeMessage $ MsgRollForward <$> variations `varyAp` variations
      , SomeMessage . MsgRollBackward <$> variations
      , pure $ SomeMessage MsgRollBackCreation
      , pure $ SomeMessage MsgWait
      ]
    ServerAgency (TokIntersect _) -> join $ NE.fromList
      [ SomeMessage . MsgIntersectFound <$> variations
      , pure $ SomeMessage MsgIntersectNotFound
      ]


instance MessageToJSON MarloweSync where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgFollowContract contractId -> msgObject "follow-contract" contractId
      MsgIntersect contractId MarloweV1 headers -> msgObject "intersect" $ object
        [ "contract-id" .= contractId
        , "version" .= String "v1"
        , "block-headers" .= headers
        ]
    ClientAgency (TokIdle _) -> \case
      MsgDone -> String "done"
      MsgRequestNext -> String "request-next"
    ClientAgency (TokWait _) -> \case
      MsgPoll -> String "poll"
      MsgCancel -> String "cancel"
    ServerAgency TokFollow -> \case
      MsgContractNotFound -> String "contract-not-found"
      MsgContractFound blockHeader MarloweV1 createStep -> msgObject "contract-found" $ object
        [ "block-header" .= blockHeader
        , "version" .= String "v1"
        , "create-step" .= createStep
        ]
    ServerAgency (TokNext MarloweV1) -> \case
      MsgRollForward blockHeader steps -> msgObject "roll-forward" $ object
        [ "block-header" .= blockHeader
        , "steps" .= steps
        ]
      MsgRollBackward blockHeader -> msgObject "roll-backward" blockHeader
      MsgRollBackCreation -> String "roll-back-creation"
      MsgWait -> String "wait"
    ServerAgency (TokIntersect _) -> \case
      MsgIntersectFound blockHeader -> msgObject "intersect-found" blockHeader
      MsgIntersectNotFound -> String "intersect-not-found"

msgObject :: ToJSON a => Key -> a -> Value
msgObject key a = object [key .= a]
