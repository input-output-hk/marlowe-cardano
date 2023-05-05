{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Marlowe load protocol is a protocol for incrementally loading and
-- merkleizing large contracts in a space-efficient way.

module Language.Marlowe.Protocol.Load.Types
  where

import Data.Binary (Binary(..), Get, Put, getWord8, putWord8)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash, fromDatum, toDatum)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol
import Plutus.V2.Ledger.Api (FromData, ToData)

data MarloweLoad where
  StProcessing :: Node -> MarloweLoad
  StCanPush :: N -> Node -> MarloweLoad
  StDone :: MarloweLoad
  StComplete :: MarloweLoad

data Node where
  RootNode :: Node
  PayNode :: Node -> Node
  IfLNode :: Node -> Node
  IfRNode :: Node -> Node
  WhenNode :: Node -> Node
  CaseNode :: Node -> Node
  LetNode :: Node -> Node
  AssertNode :: Node -> Node

data SNode (node :: Node) where
  SRootNode :: SNode 'RootNode
  SPayNode :: SNode node -> SNode ('PayNode node)
  SIfLNode :: SNode node -> SNode ('IfLNode node)
  SIfRNode :: SNode node -> SNode ('IfRNode node)
  SWhenNode :: SNode node -> SNode ('WhenNode node)
  SCaseNode :: SNode node -> SNode ('CaseNode node)
  SLetNode :: SNode node -> SNode ('LetNode node)
  SAssertNode :: SNode node -> SNode ('AssertNode node)

type family Pop (n :: N) (node :: Node) :: MarloweLoad where
  Pop n 'RootNode = 'StComplete
  Pop n ('PayNode node) = Pop n node
  Pop n ('IfLNode node) = Push n ('IfRNode node)
  Pop n ('IfRNode node) = Pop n node
  Pop n ('WhenNode node) = Pop n node
  Pop n ('CaseNode node) = Push n ('WhenNode node)
  Pop n ('LetNode node) = Pop n node
  Pop n ('AssertNode node) = Pop n node

type family Push (n :: N) (node :: Node) :: MarloweLoad where
  Push 'Z node = 'StProcessing node
  Push ('S n) node = 'StCanPush n node

instance HasSignature MarloweLoad where
  signature _ = "MarloweLoad"

instance Protocol MarloweLoad where
  data Message MarloweLoad st st' where
    MsgResume :: Nat ('S n) -> Message MarloweLoad
      ('StProcessing node)
      ('StCanPush n node)
    MsgPushClose :: Message MarloweLoad
      ('StCanPush n node)
      (Pop n node)
    MsgPushPay :: AccountId -> Payee -> Token -> Value Observation -> Message MarloweLoad
      ('StCanPush n node)
      (Push n ('PayNode node))
    MsgPushIf :: Observation -> Message MarloweLoad
      ('StCanPush n node)
      (Push n ('IfLNode node))
    MsgPushWhen :: Timeout -> Message MarloweLoad
      ('StCanPush n node)
      (Push n ('WhenNode node))
    MsgPushCase :: Action -> Message MarloweLoad
      ('StCanPush n ('WhenNode node))
      (Push n ('CaseNode node))
    MsgPushLet :: ValueId -> Value Observation -> Message MarloweLoad
      ('StCanPush n node)
      (Push n ('LetNode node))
    MsgPushAssert :: Observation -> Message MarloweLoad
      ('StCanPush n node)
      (Push n ('AssertNode node))
    MsgComplete :: DatumHash -> Message MarloweLoad
      'StComplete
      'StDone

  data ClientHasAgency st where
    TokCanPush :: Nat n -> SNode node -> ClientHasAgency ('StCanPush n node)

  data ServerHasAgency st where
    TokProcessing :: SNode node -> ServerHasAgency ('StProcessing node)
    TokComplete :: ServerHasAgency 'StComplete

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokCanPush{} = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data SomePeerHasAgency (st :: k) = forall pr. SomePeerHasAgency (PeerHasAgency pr st)

sPop :: Nat n -> SNode node -> SomePeerHasAgency (Pop n node)
sPop n = \case
  SRootNode -> SomePeerHasAgency $ ServerAgency TokComplete
  SPayNode node -> sPop n node
  SIfLNode node -> sPush n $ SIfRNode node
  SIfRNode node -> sPop n node
  SWhenNode node -> sPop n node
  SCaseNode node -> sPush n $ SWhenNode node
  SLetNode node -> sPop n node
  SAssertNode node -> sPop n node

sPush :: Nat n -> SNode node -> SomePeerHasAgency (Push n node)
sPush Zero node = SomePeerHasAgency $ ServerAgency $ TokProcessing node
sPush (Succ n) node = SomePeerHasAgency $ ClientAgency $ TokCanPush n node

instance BinaryMessage MarloweLoad where
  putMessage = \case
    ClientAgency TokCanPush{} -> \case
      MsgPushClose -> putWord8 0x00
      MsgPushPay payor payee token value -> do
        putWord8 0x01
        putDatum payor
        putDatum payee
        putDatum token
        putDatum value
      MsgPushIf cond -> do
        putWord8 0x02
        putDatum cond
      MsgPushWhen timeout -> do
        putWord8 0x03
        putDatum timeout
      MsgPushCase action -> do
        putWord8 0x04
        putDatum action
      MsgPushLet valueId value -> do
        putWord8 0x05
        putDatum valueId
        putDatum value
      MsgPushAssert obs -> do
        putWord8 0x06
        putDatum obs
    ServerAgency TokComplete -> \case
      MsgComplete hash -> put hash
    ServerAgency (TokProcessing _) -> \case
      MsgResume n -> put $ natToInt n

  getMessage = \case
    ClientAgency (TokCanPush _ tok) -> do
      tag <- getWord8
      case tag of
        0x00 -> pure $ SomeMessage MsgPushClose
        0x01 -> do
          msg <- MsgPushPay
            <$> getDatum "payor"
            <*> getDatum "payee"
            <*> getDatum "token"
            <*> getDatum "value"
          pure $ SomeMessage msg
        0x02 -> do
          msg <- MsgPushIf <$> getDatum "cond"
          pure $ SomeMessage msg
        0x03 -> do
          msg <- MsgPushWhen <$> getDatum "timeout"
          pure $ SomeMessage msg
        0x04 -> case tok of
          SWhenNode _ -> do
            msg <- MsgPushCase <$> getDatum "action"
            pure $ SomeMessage msg
          _ -> fail "Invalid protocol state for MsgPushCase"
        0x05 -> do
          msg <- MsgPushLet
            <$> getDatum "valueId"
            <*> getDatum "value"
          pure $ SomeMessage msg
        0x06 -> do
          msg <- MsgPushAssert <$> getDatum "obs"
          pure $ SomeMessage msg
        _ -> fail $ "Invalid message tag " <> show tag
    ServerAgency TokComplete -> SomeMessage . MsgComplete <$> get
    -- unsafeIntToNat is actually safe here - why? Because we immediately
    -- forget the type because of existential types. The correct type will
    -- be brought into scope when pattern matching on the pattern synonyms
    -- `Zero` or `Succ n`.
    ServerAgency (TokProcessing _) -> SomeMessage . MsgResume . unsafeIntToNat <$> get

putDatum :: ToData a => a -> Put
putDatum = put . toDatum

getDatum :: FromData a => String -> Get a
getDatum name = do
  datum <- get
  case fromDatum datum of
    Nothing -> fail $ "invalid " <> name <> " datum"
    Just a -> pure a
