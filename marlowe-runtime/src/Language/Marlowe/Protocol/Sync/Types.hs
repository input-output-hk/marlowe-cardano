{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Sync.Types where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion, MarloweVersionTag)
import Language.Marlowe.Runtime.History.Api
import Network.TypedProtocol (Protocol (..))

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
