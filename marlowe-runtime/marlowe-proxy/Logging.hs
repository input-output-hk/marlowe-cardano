{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging where

import Control.Monad.Event.Class
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Protocol.Transfer.Types (MarloweTransfer)
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver.Trace (
  TcpClientSelector,
  TcpServerSelector,
  renderTcpClientSelectorOTel,
  renderTcpServerSelectorOTel,
 )
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Explicit (injectSelector)
import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel)

data RootSelector f where
  MarloweRuntimeServer :: TcpServerSelector (Handshake MarloweRuntime) f -> RootSelector f
  MarloweSyncClient :: TcpClientSelector (Handshake MarloweSync) f -> RootSelector f
  MarloweHeaderSyncClient :: TcpClientSelector (Handshake MarloweHeaderSync) f -> RootSelector f
  MarloweQueryClient :: TcpClientSelector (Handshake MarloweQuery) f -> RootSelector f
  MarloweLoadClient :: TcpClientSelector (Handshake MarloweLoad) f -> RootSelector f
  MarloweTransferClient :: TcpClientSelector (Handshake MarloweTransfer) f -> RootSelector f
  TxJobClient :: TcpClientSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  ContractQueryClient :: TcpClientSelector (Handshake (Query ContractRequest)) f -> RootSelector f

instance Inject (TcpServerSelector (Handshake MarloweRuntime)) RootSelector where
  inject = injectSelector MarloweRuntimeServer

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  MarloweRuntimeServer sel -> renderTcpServerSelectorOTel sel
  MarloweSyncClient sel -> renderTcpClientSelectorOTel sel
  MarloweHeaderSyncClient sel -> renderTcpClientSelectorOTel sel
  MarloweQueryClient sel -> renderTcpClientSelectorOTel sel
  MarloweLoadClient sel -> renderTcpClientSelectorOTel sel
  MarloweTransferClient sel -> renderTcpClientSelectorOTel sel
  TxJobClient sel -> renderTcpClientSelectorOTel sel
  ContractQueryClient sel -> renderTcpClientSelectorOTel sel
