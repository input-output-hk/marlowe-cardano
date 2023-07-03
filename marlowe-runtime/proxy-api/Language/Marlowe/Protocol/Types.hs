{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Types where

import Data.Binary (getWord8, putWord8)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Proxy (Proxy (..))
import GHC.Show (showSpace)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as MarloweHeaderSync
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import qualified Language.Marlowe.Protocol.Load.Types as MarloweLoad
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as MarloweSync
import Language.Marlowe.Protocol.Transfer.Types (MarloweTransfer)
import qualified Language.Marlowe.Protocol.Transfer.Types as MarloweTransfer
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Codec
import Network.Protocol.Codec.Spec (ShowProtocol (..))
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Types (Query)
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol

data MarloweRuntime where
  StInit :: MarloweRuntime
  StMarloweSync :: MarloweSync -> MarloweRuntime
  StMarloweHeaderSync :: MarloweHeaderSync -> MarloweRuntime
  StMarloweQuery :: MarloweQuery -> MarloweRuntime
  StTxJob :: Job MarloweTxCommand -> MarloweRuntime
  StMarloweLoad :: MarloweLoad -> MarloweRuntime
  StContractQuery :: Query ContractRequest -> MarloweRuntime
  StMarloweImport :: MarloweTransfer -> MarloweRuntime

instance Protocol MarloweRuntime where
  data Message MarloweRuntime st st' where
    MsgRunMarloweSync :: Message MarloweRuntime 'StInit ('StMarloweSync 'MarloweSync.StInit)
    MsgRunMarloweHeaderSync :: Message MarloweRuntime 'StInit ('StMarloweHeaderSync 'MarloweHeaderSync.StIdle)
    MsgRunMarloweQuery :: Message MarloweRuntime 'StInit ('StMarloweQuery 'Query.StReq)
    MsgRunMarloweLoad :: Message MarloweRuntime 'StInit ('StMarloweLoad ('MarloweLoad.StProcessing 'MarloweLoad.RootNode))
    MsgRunMarloweImport :: Message MarloweRuntime 'StInit ('StMarloweImport 'MarloweTransfer.StIdle)
    MsgRunTxJob :: Message MarloweRuntime 'StInit ('StTxJob 'Job.StInit)
    MsgRunContractQuery :: Message MarloweRuntime 'StInit ('StContractQuery 'Query.StReq)
    MsgMarloweSync :: Message MarloweSync st st' -> Message MarloweRuntime ('StMarloweSync st) ('StMarloweSync st')
    MsgMarloweHeaderSync
      :: Message MarloweHeaderSync st st' -> Message MarloweRuntime ('StMarloweHeaderSync st) ('StMarloweHeaderSync st')
    MsgMarloweQuery :: Message MarloweQuery st st' -> Message MarloweRuntime ('StMarloweQuery st) ('StMarloweQuery st')
    MsgMarloweLoad :: Message MarloweLoad st st' -> Message MarloweRuntime ('StMarloweLoad st) ('StMarloweLoad st')
    MsgMarloweImport
      :: Message MarloweTransfer st st' -> Message MarloweRuntime ('StMarloweImport st) ('StMarloweImport st')
    MsgTxJob :: Message (Job MarloweTxCommand) st st' -> Message MarloweRuntime ('StTxJob st) ('StTxJob st')
    MsgContractQuery
      :: Message (Query ContractRequest) st st' -> Message MarloweRuntime ('StContractQuery st) ('StContractQuery st')

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokClientMarloweSync :: MarloweSync.ClientHasAgency st -> ClientHasAgency ('StMarloweSync st)
    TokClientMarloweHeaderSync :: MarloweHeaderSync.ClientHasAgency st -> ClientHasAgency ('StMarloweHeaderSync st)
    TokClientMarloweQuery :: Query.ClientHasAgency st -> ClientHasAgency ('StMarloweQuery st)
    TokClientMarloweLoad :: MarloweLoad.ClientHasAgency st -> ClientHasAgency ('StMarloweLoad st)
    TokClientMarloweImport :: MarloweTransfer.ClientHasAgency st -> ClientHasAgency ('StMarloweImport st)
    TokClientTxJob :: Job.ClientHasAgency st -> ClientHasAgency ('StTxJob st)
    TokClientContractQuery :: Query.ClientHasAgency st -> ClientHasAgency ('StContractQuery st)

  data ServerHasAgency st where
    TokServerMarloweSync :: MarloweSync.ServerHasAgency st -> ServerHasAgency ('StMarloweSync st)
    TokServerMarloweHeaderSync :: MarloweHeaderSync.ServerHasAgency st -> ServerHasAgency ('StMarloweHeaderSync st)
    TokServerMarloweQuery :: Query.ServerHasAgency st -> ServerHasAgency ('StMarloweQuery st)
    TokServerMarloweLoad :: MarloweLoad.ServerHasAgency st -> ServerHasAgency ('StMarloweLoad st)
    TokServerMarloweImport :: MarloweTransfer.ServerHasAgency st -> ServerHasAgency ('StMarloweImport st)
    TokServerTxJob :: Job.ServerHasAgency st -> ServerHasAgency ('StTxJob st)
    TokServerContractQuery :: Query.ServerHasAgency st -> ServerHasAgency ('StContractQuery st)

  data NobodyHasAgency st where
    TokNobodyMarloweSync :: MarloweSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweSync st)
    TokNobodyMarloweHeaderSync :: MarloweHeaderSync.NobodyHasAgency st -> NobodyHasAgency ('StMarloweHeaderSync st)
    TokNobodyMarloweQuery :: Query.NobodyHasAgency st -> NobodyHasAgency ('StMarloweQuery st)
    TokNobodyMarloweLoad :: MarloweLoad.NobodyHasAgency st -> NobodyHasAgency ('StMarloweLoad st)
    TokNobodyMarloweImport :: MarloweTransfer.NobodyHasAgency st -> NobodyHasAgency ('StMarloweImport st)
    TokNobodyTxJob :: Job.NobodyHasAgency st -> NobodyHasAgency ('StTxJob st)
    TokNobodyContractQuery :: Query.NobodyHasAgency st -> NobodyHasAgency ('StContractQuery st)

  exclusionLemma_ClientAndServerHaveAgency = \case
    TokInit -> \case {}
    TokClientMarloweSync tok -> \case
      TokServerMarloweSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientMarloweHeaderSync tok -> \case
      TokServerMarloweHeaderSync tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientMarloweQuery tok -> \case
      TokServerMarloweQuery tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientMarloweLoad tok -> \case
      TokServerMarloweLoad tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientMarloweImport tok -> \case
      TokServerMarloweImport tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientTxJob tok -> \case
      TokServerTxJob tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'
    TokClientContractQuery tok -> \case
      TokServerContractQuery tok' -> exclusionLemma_ClientAndServerHaveAgency tok tok'

  exclusionLemma_NobodyAndClientHaveAgency = \case
    TokNobodyMarloweSync tok -> \case
      TokClientMarloweSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyMarloweHeaderSync tok -> \case
      TokClientMarloweHeaderSync tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyMarloweQuery tok -> \case
      TokClientMarloweQuery tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyMarloweLoad tok -> \case
      TokClientMarloweLoad tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyMarloweImport tok -> \case
      TokClientMarloweImport tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyTxJob tok -> \case
      TokClientTxJob tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
    TokNobodyContractQuery tok -> \case
      TokClientContractQuery tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'

  exclusionLemma_NobodyAndServerHaveAgency = \case
    TokNobodyMarloweSync tok -> \case
      TokServerMarloweSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyMarloweHeaderSync tok -> \case
      TokServerMarloweHeaderSync tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyMarloweQuery tok -> \case
      TokServerMarloweQuery tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyMarloweLoad tok -> \case
      TokServerMarloweLoad tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyMarloweImport tok -> \case
      TokServerMarloweImport tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyTxJob tok -> \case
      TokServerTxJob tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'
    TokNobodyContractQuery tok -> \case
      TokServerContractQuery tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'

instance ShowProtocol MarloweRuntime where
  showsPrecMessage p tok = \case
    MsgRunMarloweSync -> showString "MsgRunMarloweSync"
    MsgRunMarloweHeaderSync -> showString "MsgRunMarloweHeaderSync"
    MsgRunMarloweQuery -> showString "MsgRunMarloweQuery"
    MsgRunMarloweLoad -> showString "MsgRunMarloweLoad"
    MsgRunMarloweImport -> showString "MsgRunMarloweImport"
    MsgRunTxJob -> showString "MsgRunTxJob"
    MsgRunContractQuery -> showString "MsgRunContractQuery"
    MsgMarloweSync msg ->
      showParen
        (p >= 11)
        ( showString "MsgMarloweSync"
            . showSpace
            . case tok of
              ClientAgency (TokClientMarloweSync tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerMarloweSync tok') -> showsPrecMessage p (ServerAgency tok') msg
        )
    MsgMarloweHeaderSync msg ->
      showParen
        (p >= 11)
        ( showString "MsgMarloweHeaderSync"
            . showSpace
            . case tok of
              ClientAgency (TokClientMarloweHeaderSync tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerMarloweHeaderSync tok') -> showsPrecMessage p (ServerAgency tok') msg
        )
    MsgMarloweQuery msg ->
      showParen
        (p >= 11)
        ( showString "MsgMarloweQuery"
            . showSpace
            . case tok of
              ClientAgency (TokClientMarloweQuery tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerMarloweQuery tok') -> showsPrecMessage p (ServerAgency tok') msg
        )
    MsgMarloweLoad msg ->
      showParen
        (p >= 11)
        ( showString "MsgMarloweLoad"
            . showSpace
            . case tok of
              ClientAgency (TokClientMarloweLoad tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerMarloweLoad tok') -> showsPrecMessage p (ServerAgency tok') msg
        )
    MsgMarloweImport msg ->
      showParen
        (p >= 11)
        ( showString "MsgMarloweImport"
            . showSpace
            . case tok of
              ClientAgency (TokClientMarloweImport tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerMarloweImport tok') -> showsPrecMessage p (ServerAgency tok') msg
        )
    MsgTxJob msg ->
      showParen
        (p >= 11)
        ( showString "MsgTxJob"
            . showSpace
            . case tok of
              ClientAgency (TokClientTxJob tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerTxJob tok') -> showsPrecMessage p (ServerAgency tok') msg
        )
    MsgContractQuery msg ->
      showParen
        (p >= 11)
        ( showString "MsgContractQuery"
            . showSpace
            . case tok of
              ClientAgency (TokClientContractQuery tok') -> showsPrecMessage p (ClientAgency tok') msg
              ServerAgency (TokServerContractQuery tok') -> showsPrecMessage p (ServerAgency tok') msg
        )

  showsPrecServerHasAgency p = \case
    TokServerMarloweSync tok ->
      showParen
        (p >= 11)
        ( showString "TokServerMarloweSync"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )
    TokServerMarloweHeaderSync tok ->
      showParen
        (p >= 11)
        ( showString "TokServerMarloweHeaderSync"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )
    TokServerMarloweQuery tok ->
      showParen
        (p >= 11)
        ( showString "TokServerMarloweQuery"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )
    TokServerMarloweLoad tok ->
      showParen
        (p >= 11)
        ( showString "TokServerMarloweLoad"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )
    TokServerMarloweImport tok ->
      showParen
        (p >= 11)
        ( showString "TokServerMarloweImport"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )
    TokServerTxJob tok ->
      showParen
        (p >= 11)
        ( showString "TokServerTxJob"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )
    TokServerContractQuery tok ->
      showParen
        (p >= 11)
        ( showString "TokServerContractQuery"
            . showSpace
            . showsPrecServerHasAgency 11 tok
        )

  showsPrecClientHasAgency p = \case
    TokInit -> showString "TokInit"
    TokClientMarloweSync tok ->
      showParen
        (p >= 11)
        ( showString "TokClientMarloweSync"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )
    TokClientMarloweHeaderSync tok ->
      showParen
        (p >= 11)
        ( showString "TokClientMarloweHeaderSync"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )
    TokClientMarloweQuery tok ->
      showParen
        (p >= 11)
        ( showString "TokClientMarloweQuery"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )
    TokClientMarloweLoad tok ->
      showParen
        (p >= 11)
        ( showString "TokClientMarloweLoad"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )
    TokClientMarloweImport tok ->
      showParen
        (p >= 11)
        ( showString "TokClientMarloweImport"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )
    TokClientTxJob tok ->
      showParen
        (p >= 11)
        ( showString "TokClientTxJob"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )
    TokClientContractQuery tok ->
      showParen
        (p >= 11)
        ( showString "TokClientContractQuery"
            . showSpace
            . showsPrecClientHasAgency 11 tok
        )

instance OTelProtocol MarloweRuntime where
  protocolName _ = "marlowe_runtime"
  messageAttributes tok = \case
    MsgRunMarloweSync ->
      MessageAttributes
        { messageType = "run_marlowe_sync"
        , messageParameters = []
        }
    MsgRunMarloweHeaderSync ->
      MessageAttributes
        { messageType = "run_marlowe_header_sync"
        , messageParameters = []
        }
    MsgRunMarloweQuery ->
      MessageAttributes
        { messageType = "run_marlowe_query"
        , messageParameters = []
        }
    MsgRunMarloweLoad ->
      MessageAttributes
        { messageType = "run_marlowe_load"
        , messageParameters = []
        }
    MsgRunMarloweImport ->
      MessageAttributes
        { messageType = "run_marlowe_import"
        , messageParameters = []
        }
    MsgRunTxJob ->
      MessageAttributes
        { messageType = "run_tx_job"
        , messageParameters = []
        }
    MsgRunContractQuery ->
      MessageAttributes
        { messageType = "run_contract_query"
        , messageParameters = []
        }
    MsgMarloweSync msg -> case tok of
      ClientAgency (TokClientMarloweSync tok') ->
        subMessageAttributes "marlowe_sync" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweSync tok') ->
        subMessageAttributes "marlowe_sync" $ messageAttributes (ServerAgency tok') msg
    MsgMarloweHeaderSync msg -> case tok of
      ClientAgency (TokClientMarloweHeaderSync tok') ->
        subMessageAttributes "marlowe_header_sync" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweHeaderSync tok') ->
        subMessageAttributes "marlowe_header_sync" $ messageAttributes (ServerAgency tok') msg
    MsgMarloweQuery msg -> case tok of
      ClientAgency (TokClientMarloweQuery tok') ->
        subMessageAttributes "marlowe_query" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweQuery tok') ->
        subMessageAttributes "marlowe_query" $ messageAttributes (ServerAgency tok') msg
    MsgMarloweLoad msg -> case tok of
      ClientAgency (TokClientMarloweLoad tok') ->
        subMessageAttributes "marlowe_load" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweLoad tok') ->
        subMessageAttributes "marlowe_load" $ messageAttributes (ServerAgency tok') msg
    MsgMarloweImport msg -> case tok of
      ClientAgency (TokClientMarloweImport tok') ->
        subMessageAttributes "marlowe_import" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerMarloweImport tok') ->
        subMessageAttributes "marlowe_import" $ messageAttributes (ServerAgency tok') msg
    MsgTxJob msg -> case tok of
      ClientAgency (TokClientTxJob tok') ->
        subMessageAttributes "tx_job" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerTxJob tok') ->
        subMessageAttributes "tx_job" $ messageAttributes (ServerAgency tok') msg
    MsgContractQuery msg -> case tok of
      ClientAgency (TokClientContractQuery tok') ->
        subMessageAttributes "contract_query" $ messageAttributes (ClientAgency tok') msg
      ServerAgency (TokServerContractQuery tok') ->
        subMessageAttributes "contract_query" $ messageAttributes (ServerAgency tok') msg
    where
      subMessageAttributes subProtocol attrs@MessageAttributes{..} =
        attrs
          { messageType = subProtocol <> "." <> messageType
          }

instance BinaryMessage MarloweRuntime where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgRunMarloweSync -> putWord8 0x00
      MsgRunMarloweHeaderSync -> putWord8 0x01
      MsgRunMarloweQuery -> putWord8 0x02
      MsgRunTxJob -> putWord8 0x03
      MsgRunMarloweLoad -> putWord8 0x04
      MsgRunContractQuery -> putWord8 0x05
      MsgRunMarloweImport -> putWord8 0x06
    ClientAgency (TokClientMarloweSync tok) -> \case
      MsgMarloweSync msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientMarloweHeaderSync tok) -> \case
      MsgMarloweHeaderSync msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientMarloweQuery tok) -> \case
      MsgMarloweQuery msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientMarloweLoad tok) -> \case
      MsgMarloweLoad msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientMarloweImport tok) -> \case
      MsgMarloweImport msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientTxJob tok) -> \case
      MsgTxJob msg -> putMessage (ClientAgency tok) msg
    ClientAgency (TokClientContractQuery tok) -> \case
      MsgContractQuery msg -> putMessage (ClientAgency tok) msg
    ServerAgency (TokServerMarloweSync tok) -> \case
      MsgMarloweSync msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerMarloweHeaderSync tok) -> \case
      MsgMarloweHeaderSync msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerMarloweQuery tok) -> \case
      MsgMarloweQuery msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerMarloweLoad tok) -> \case
      MsgMarloweLoad msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerMarloweImport tok) -> \case
      MsgMarloweImport msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerTxJob tok) -> \case
      MsgTxJob msg -> putMessage (ServerAgency tok) msg
    ServerAgency (TokServerContractQuery tok) -> \case
      MsgContractQuery msg -> putMessage (ServerAgency tok) msg

  getMessage = \case
    ClientAgency TokInit ->
      getWord8 >>= \case
        0x00 -> pure $ SomeMessage MsgRunMarloweSync
        0x01 -> pure $ SomeMessage MsgRunMarloweHeaderSync
        0x02 -> pure $ SomeMessage MsgRunMarloweQuery
        0x03 -> pure $ SomeMessage MsgRunTxJob
        0x04 -> pure $ SomeMessage MsgRunMarloweLoad
        0x05 -> pure $ SomeMessage MsgRunContractQuery
        0x06 -> pure $ SomeMessage MsgRunMarloweImport
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
    ClientAgency (TokClientMarloweLoad tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgMarloweLoad msg
    ClientAgency (TokClientMarloweImport tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgMarloweImport msg
    ClientAgency (TokClientTxJob tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgTxJob msg
    ClientAgency (TokClientContractQuery tok) -> do
      SomeMessage msg <- getMessage (ClientAgency tok)
      pure $ SomeMessage $ MsgContractQuery msg
    ServerAgency (TokServerMarloweSync tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweSync msg
    ServerAgency (TokServerMarloweHeaderSync tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweHeaderSync msg
    ServerAgency (TokServerMarloweQuery tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweQuery msg
    ServerAgency (TokServerMarloweLoad tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweLoad msg
    ServerAgency (TokServerMarloweImport tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgMarloweImport msg
    ServerAgency (TokServerTxJob tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgTxJob msg
    ServerAgency (TokServerContractQuery tok) -> do
      SomeMessage msg <- getMessage (ServerAgency tok)
      pure $ SomeMessage $ MsgContractQuery msg

instance HasSignature MarloweRuntime where
  signature _ =
    fold $
      intersperse
        " "
        [ "MarloweRuntime"
        , signature $ Proxy @MarloweSync
        , signature $ Proxy @MarloweHeaderSync
        , signature $ Proxy @MarloweQuery
        , signature $ Proxy @MarloweLoad
        , signature $ Proxy @MarloweTransfer
        , signature $ Proxy @(Job MarloweTxCommand)
        , signature $ Proxy @(Query ContractRequest)
        ]
