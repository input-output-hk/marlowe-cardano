{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Sync.Types
  where

import Data.Aeson (Key, ToJSON, Value(..), object, (.=))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId(..), MarloweVersion(..), MarloweVersionTag)
import Language.Marlowe.Runtime.History.Api
import Network.Protocol.Driver (MessageToJSON(..))
import Network.TypedProtocol (PeerHasAgency(..), Protocol(..))

data MarloweSync where
  StInit :: MarloweSync
  StFollow :: MarloweSync
  StDone :: MarloweSync
  StIdle :: MarloweVersionTag -> MarloweSync
  StNext :: MarloweVersionTag -> MarloweSync
  StWait :: MarloweVersionTag -> MarloweSync
  StIntersect :: MarloweVersionTag -> MarloweSync

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
