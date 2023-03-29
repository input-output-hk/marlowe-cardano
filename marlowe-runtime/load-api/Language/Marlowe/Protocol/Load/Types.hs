{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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
  StCanPush :: CanPush -> MarloweLoad
  StDone :: MarloweLoad
  StComplete :: MarloweLoad

data CanPush where
  StRoot :: CanPush
  StPay :: CanPush -> CanPush
  StIfL :: CanPush -> CanPush
  StIfR :: CanPush -> CanPush
  StWhen :: CanPush -> CanPush
  StCase :: CanPush -> CanPush
  StLet :: CanPush -> CanPush
  StAssert :: CanPush -> CanPush

data StCanPush (x :: CanPush) where
  TokRoot :: StCanPush 'StRoot
  TokPay :: StCanPush st -> StCanPush ('StPay st)
  TokIfL :: StCanPush st -> StCanPush ('StIfL st)
  TokIfR :: StCanPush st -> StCanPush ('StIfR st)
  TokWhen :: StCanPush st -> StCanPush ('StWhen st)
  TokCase :: StCanPush st -> StCanPush ('StCase st)
  TokLet :: StCanPush st -> StCanPush ('StLet st)
  TokAssert :: StCanPush st -> StCanPush ('StAssert st)

type family Pop (st :: CanPush) :: MarloweLoad where
  Pop 'StRoot = 'StComplete
  Pop ('StPay st) = Pop st
  Pop ('StIfL st) = 'StCanPush ('StIfR st)
  Pop ('StIfR st) = Pop st
  Pop ('StWhen st) = Pop st
  Pop ('StCase st) = 'StCanPush ('StWhen st)
  Pop ('StLet st) = Pop st
  Pop ('StAssert st) = Pop st

instance HasSignature MarloweLoad where
  signature _ = "MarloweLoad"

instance Protocol MarloweLoad where
  data Message MarloweLoad st st' where
    MsgPushClose :: Message MarloweLoad
      ('StCanPush st)
      (Pop st)
    MsgPushPay :: AccountId -> Payee -> Token -> Value Observation -> Message MarloweLoad
      ('StCanPush st)
      ('StCanPush ('StPay st))
    MsgPushIf :: Observation -> Message MarloweLoad
      ('StCanPush st)
      ('StCanPush ('StIfL st))
    MsgPushWhen :: Timeout -> Message MarloweLoad
      ('StCanPush st)
      ('StCanPush ('StWhen st))
    MsgPushCase :: Action -> Message MarloweLoad
      ('StCanPush ('StWhen st))
      ('StCanPush ('StCase st))
    MsgPushLet :: ValueId -> Value Observation -> Message MarloweLoad
      ('StCanPush st)
      ('StCanPush ('StLet st))
    MsgPushAssert :: Observation -> Message MarloweLoad
      ('StCanPush st)
      ('StCanPush ('StAssert st))
    MsgComplete :: DatumHash -> Message MarloweLoad
      'StComplete
      'StDone

  data ClientHasAgency st where
    TokCanPush :: StCanPush st -> ClientHasAgency ('StCanPush st)

  data ServerHasAgency st where
    TokComplete :: ServerHasAgency 'StComplete

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokCanPush{} = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data SomePeerHasAgency (st :: k) = forall pr. SomePeerHasAgency (PeerHasAgency pr st)

stPop :: StCanPush st -> SomePeerHasAgency (Pop st)
stPop = \case
  TokRoot -> SomePeerHasAgency $ ServerAgency TokComplete
  TokPay st -> stPop st
  TokIfL st -> SomePeerHasAgency $ ClientAgency $ TokCanPush $ TokIfR st
  TokIfR st -> stPop st
  TokWhen st -> stPop st
  TokCase st -> SomePeerHasAgency $ ClientAgency $ TokCanPush $ TokWhen st
  TokLet st -> stPop st
  TokAssert st -> stPop st

instance BinaryMessage MarloweLoad where
  putMessage = \case
    ClientAgency (TokCanPush _) -> \case
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

  getMessage = \case
    ClientAgency (TokCanPush tok) -> do
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
          TokWhen _ -> do
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

putDatum :: ToData a => a -> Put
putDatum = put . toDatum

getDatum :: FromData a => String -> Get a
getDatum name = do
  datum <- get
  case fromDatum datum of
    Nothing -> fail $ "invalid " <> name <> " datum"
    Just a -> pure a
