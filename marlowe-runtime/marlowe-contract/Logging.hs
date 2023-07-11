{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging where

import Control.Monad.Event.Class
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import Language.Marlowe.Protocol.Transfer.Types (MarloweTransfer)
import Language.Marlowe.Runtime.Contract (renderContractStoreSelectorOTel)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.Store (ContractStoreSelector (..))
import Network.Protocol.Driver.Trace (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Explicit (injectSelector)
import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel)

data RootSelector f where
  ContractStoreSelector :: ContractStoreSelector f -> RootSelector f
  MarloweLoadServer :: TcpServerSelector (Handshake MarloweLoad) f -> RootSelector f
  MarloweTransferServer :: TcpServerSelector (Handshake MarloweTransfer) f -> RootSelector f
  QueryServer :: TcpServerSelector (Handshake (Query ContractRequest)) f -> RootSelector f

instance Inject (TcpServerSelector (Handshake (Query ContractRequest))) RootSelector where
  inject = injectSelector QueryServer

instance Inject (TcpServerSelector (Handshake MarloweLoad)) RootSelector where
  inject = injectSelector MarloweLoadServer

instance Inject (TcpServerSelector (Handshake MarloweTransfer)) RootSelector where
  inject = injectSelector MarloweTransferServer

instance Inject ContractStoreSelector RootSelector where
  inject = injectSelector ContractStoreSelector

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  MarloweLoadServer sel -> renderTcpServerSelectorOTel sel
  MarloweTransferServer sel -> renderTcpServerSelectorOTel sel
  QueryServer sel -> renderTcpServerSelectorOTel sel
  ContractStoreSelector sel -> renderContractStoreSelectorOTel sel
