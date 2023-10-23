{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging (
  RootSelector (..),
  renderRootSelectorOTel,
) where

import Control.Monad.Event.Class (Inject (..))
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainSyncCommand,
  ChainSyncQuery,
  RuntimeChainSeekClientSelector,
  renderChainSeekClientSelectorOTel,
 )
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Transaction (
  renderLoadMarloweContextSelectorOTel,
  renderLoadPayoutContextSelectorOTel,
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
import qualified Network.Protocol.Peer.Monad.TCP as PeerT
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Render.OpenTelemetry

data RootSelector f where
  ChainSyncJobClient :: TcpClientSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ChainSyncQueryClient :: TcpClientSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  ChainSeekClient :: PeerT.TcpClientSelector RuntimeChainSeekClientSelector f -> RootSelector f
  ContractQueryClient :: TcpClientSelector (Handshake (Query ContractRequest)) f -> RootSelector f
  Server :: TcpServerSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  LoadWalletContext :: Q.LoadWalletContextSelector f -> RootSelector f
  LoadMarloweContext :: Q.LoadMarloweContextSelector f -> RootSelector f
  LoadPayoutContext :: Q.LoadPayoutContextSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject Q.LoadWalletContextSelector RootSelector where
  inject = injectSelector LoadWalletContext

instance Inject Q.LoadMarloweContextSelector RootSelector where
  inject = injectSelector LoadMarloweContext

instance Inject Q.LoadPayoutContextSelector RootSelector where
  inject = injectSelector LoadPayoutContext

instance Inject TransactionServerSelector RootSelector where
  inject = injectSelector App

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  ChainSyncJobClient sel -> renderTcpClientSelectorOTel sel
  ChainSyncQueryClient sel -> renderTcpClientSelectorOTel sel
  ChainSeekClient sel -> PeerT.renderTcpClientSelectorOTel renderChainSeekClientSelectorOTel sel
  ContractQueryClient sel -> renderTcpClientSelectorOTel sel
  Server sel -> renderTcpServerSelectorOTel sel
  App sel -> renderTransactionServerSelectorOTel sel
  LoadWalletContext sel -> renderLoadWalletContextSelectorOTel sel
  LoadMarloweContext sel -> renderLoadMarloweContextSelectorOTel sel
  LoadPayoutContext sel -> renderLoadPayoutContextSelectorOTel sel
