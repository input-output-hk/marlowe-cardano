{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Types
  where

import Data.Binary (getWord8, putWord8)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Proxy (Proxy(..))
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as MarloweHeaderSync
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import qualified Language.Marlowe.Protocol.Query.Types as MarloweQuery
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as MarloweSync
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Codec
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

data MarloweRuntime where
  StInit :: MarloweRuntime
  StMarloweSync :: MarloweSync -> MarloweRuntime
  StMarloweHeaderSync :: MarloweHeaderSync -> MarloweRuntime
  StMarloweQuery :: MarloweQuery -> MarloweRuntime
  StTxJob :: Job MarloweTxCommand -> MarloweRuntime

instance Protocol MarloweRuntime where
  data Message MarloweRuntime st st' where
    MsgRunMarloweSync :: Message MarloweRuntime 'StInit ('StMarloweSync 'MarloweSync.StInit)
    MsgRunMarloweHeaderSync :: Message MarloweRuntime 'StInit ('StMarloweHeaderSync 'MarloweHeaderSync.StIdle)
    MsgRunMarloweQuery :: Message MarloweRuntime 'StInit ('StMarloweQuery 'MarloweQuery.StReq)
    MsgRunTxJob :: Message MarloweRuntime 'StInit ('StTxJob 'Job.StInit)
    MsgMarloweSync :: Message MarloweSync st st' -> Message MarloweRuntime ('StMarloweSync st) ('StMarloweSync st')
    MsgMarloweHeaderSync :: Message MarloweHeaderSync st st' -> Message MarloweRuntime ('StMarloweHeaderSync st) ('StMarloweHeaderSync st')
    MsgMarloweQuery :: Message MarloweQuery st st' -> Message MarloweRuntime ('StMarloweQuery st) ('StMarloweQuery st')
    MsgTxJob :: Message (Job MarloweTxCommand) st st' -> Message MarloweRuntime ('StTxJob st) ('StTxJob st')

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokClientMarloweSync :: MarloweSync.ClientHasAgency st -> ClientHasAgency ('StMarloweSync st)
    TokClientMarloweHeaderSync :: MarloweHeaderSync.ClientHasAgency st -> ClientHasAgency ('StMarloweHeaderSync st)
    TokClientMarloweQuery :: MarloweQuery.ClientHasAgency st -> ClientHasAgency ('StMarloweQuery st)
    TokClientTxJob :: Job.ClientHasAgency st -> ClientHasAgency ('StTxJob st)

  data ServerHasAgency st where
    TokServerMarloweSync :: MarloweSync.ServerHasAgency st -> ServerHasAgency ('StMarloweSync st)
    TokServerMarloweHeaderSync :: MarloweHeaderSync.ServerHasAgency st -> ServerHasAgency ('StMarloweHeaderSync st)
    TokServerMarloweQuery :: MarloweQuery.ServerHasAgency st -> ServerHasAgency ('StMarloweQuery st)
    TokServerTxJob :: Job.ServerHasAgency st -> ServerHasAgency ('StTxJob st)

  data NobodyHasAgency st where
    TokNobodyMarloweSync :: MarloweSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweSync st)
    TokNobodyMarloweHeaderSync :: MarloweHeaderSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweHeaderSync st)
    TokNobodyMarloweQuery :: MarloweQuery.NobodyHasAgency st -> NobodyHasAgency ('StMarloweQuery st)
    TokNobodyTxJob :: Job.NobodyHasAgency st -> NobodyHasAgency ('StTxJob st)

  exclusionLemma_ClientAndServerHaveAgency = \case
    TokInit -> \case
    TokClientMarloweSync tok -> \case
      TokServerMarloweSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientMarloweHeaderSync tok -> \case
      TokServerMarloweHeaderSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientMarloweQuery tok -> \case
      TokServerMarloweQuery tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientTxJob tok -> \case
      TokServerTxJob tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'

  exclusionLemma_NobodyAndClientHaveAgency = \case
    TokNobodyMarloweSync tok -> \case
      TokClientMarloweSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyMarloweHeaderSync tok -> \case
      TokClientMarloweHeaderSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyMarloweQuery tok -> \case
      TokClientMarloweQuery tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyTxJob tok -> \case
      TokClientTxJob tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'

  exclusionLemma_NobodyAndServerHaveAgency = \case
    TokNobodyMarloweSync tok -> \case
      TokServerMarloweSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyMarloweHeaderSync tok -> \case
      TokServerMarloweHeaderSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyMarloweQuery tok -> \case
      TokServerMarloweQuery tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyTxJob tok -> \case
      TokServerTxJob tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'

instance OTelProtocol MarloweRuntime where
  protocolName _ = "marlowe_runtime"
  messageAttributes tok = \case
    MsgRunMarloweSync -> MessageAttributes
      { messageType = "run_marlowe_sync"
      , messageParameters = []
      }
    MsgRunMarloweHeaderSync -> MessageAttributes
      { messageType = "run_marlowe_header_sync"
      , messageParameters = []
      }
    MsgRunMarloweQuery -> MessageAttributes
      { messageType = "run_marlowe_query"
      , messageParameters = []
      }
    MsgRunTxJob -> MessageAttributes
      { messageType = "run_tx_job"
      , messageParameters = []
      }
    MsgMarloweSync msg -> case tok of
      ClientAgency (TokClientMarloweSync tok') ->
        messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweSync tok') ->
        messageAttributes (ServerAgency tok') msg
    MsgMarloweHeaderSync msg -> case tok of
      ClientAgency (TokClientMarloweHeaderSync tok') ->
        messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweHeaderSync tok') ->
        messageAttributes (ServerAgency tok') msg
    MsgMarloweQuery msg -> case tok of
      ClientAgency (TokClientMarloweQuery tok') ->
        messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweQuery tok') ->
        messageAttributes (ServerAgency tok') msg
    MsgTxJob msg -> case tok of
      ClientAgency (TokClientTxJob tok') ->
        messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerTxJob tok') ->
        messageAttributes (ServerAgency tok') msg


instance BinaryMessage MarloweRuntime where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgRunMarloweSync -> putWord8 0x00
      MsgRunMarloweHeaderSync -> putWord8 0x01
      MsgRunMarloweQuery -> putWord8 0x02
      MsgRunTxJob -> putWord8 0x03
    ClientAgency (TokClientMarloweSync tok) -> \case
      MsgMarloweSync msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientMarloweHeaderSync tok) -> \case
      MsgMarloweHeaderSync msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientMarloweQuery tok) -> \case
      MsgMarloweQuery msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientTxJob tok) -> \case
      MsgTxJob msg -> putMessage (ClientAgency tok) msg
    ServerAgency (TokServerMarloweSync tok) -> \case
      MsgMarloweSync msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerMarloweHeaderSync tok) -> \case
      MsgMarloweHeaderSync msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerMarloweQuery tok) -> \case
      MsgMarloweQuery msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerTxJob tok) -> \case
      MsgTxJob msg -> putMessage (ServerAgency tok) msg

  getMessage = \case
    ClientAgency TokInit -> getWord8 >>= \case
      0x00 -> pure $ SomeMessage MsgRunMarloweSync
      0x01 -> pure $ SomeMessage MsgRunMarloweHeaderSync
      0x02 -> pure $ SomeMessage MsgRunMarloweQuery
      0x03 -> pure $ SomeMessage MsgRunTxJob
      tag -> fail $ "invalid message tag " <> show tag
    ClientAgency (TokClientMarloweSync tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgMarloweSync msg
    ClientAgency (TokClientMarloweHeaderSync tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgMarloweHeaderSync msg
    ClientAgency (TokClientMarloweQuery tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgMarloweQuery msg
    ClientAgency (TokClientTxJob tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgTxJob msg
    ServerAgency (TokServerMarloweSync tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweSync msg
    ServerAgency (TokServerMarloweHeaderSync tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweHeaderSync msg
    ServerAgency (TokServerMarloweQuery tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweQuery msg
    ServerAgency (TokServerTxJob tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgTxJob msg

instance HasSignature MarloweRuntime where
  signature _ = fold $ intersperse " "
    [ "MarloweRuntime"
    , signature $ Proxy @MarloweSync
    , signature $ Proxy @MarloweHeaderSync
    , signature $ Proxy @MarloweQuery
    , signature $ Proxy @(Job MarloweTxCommand)
    ]
