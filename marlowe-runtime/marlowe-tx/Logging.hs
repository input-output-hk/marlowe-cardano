{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging (
  RootSelector (..),
  renderRootSelectorOTel,
) where

import Control.Monad.Event.Class (Inject (..))
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction (
  renderLoadMarloweContextSelectorOTel,
  renderLoadWalletContextSelectorOTel,
  renderTransactionServerSelectorOTel,
 )
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Server
import Network.Protocol.Driver.Trace (
  TcpClientSelector,
  TcpServerSelector,
  renderTcpClientSelectorOTel,
  renderTcpServerSelectorOTel,
 )
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry

data RootSelector f where
  ChainSyncJobClient :: TcpClientSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ChainSyncQueryClient :: TcpClientSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  ContractQueryClient :: TcpClientSelector (Handshake (Query ContractRequest)) f -> RootSelector f
  Server :: TcpServerSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  LoadWalletContext :: Q.LoadWalletContextSelector f -> RootSelector f
  LoadMarloweContext :: Q.LoadMarloweContextSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject Q.LoadWalletContextSelector RootSelector where
  inject = injectSelector LoadWalletContext

instance Inject Q.LoadMarloweContextSelector RootSelector where
  inject = injectSelector LoadMarloweContext

instance Inject TransactionServerSelector RootSelector where
  inject = injectSelector App

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  ChainSyncJobClient sel -> renderTcpClientSelectorOTel sel
  ChainSyncQueryClient sel -> renderTcpClientSelectorOTel sel
  ContractQueryClient sel -> renderTcpClientSelectorOTel sel
  Server sel -> renderTcpServerSelectorOTel sel
  App sel -> renderTransactionServerSelectorOTel sel
  LoadWalletContext sel -> renderLoadWalletContextSelectorOTel sel
  LoadMarloweContext sel -> renderLoadMarloweContextSelectorOTel sel
