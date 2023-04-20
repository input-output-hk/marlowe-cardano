{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  where

import Control.Monad.Event.Class
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Network.Protocol.Driver (TcpServerSelector, renderTcpServerSelectorOTel)
import Observe.Event.Explicit (injectSelector)
import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel)

data RootSelector f where
  MarloweRuntimeServer :: TcpServerSelector MarloweRuntime f -> RootSelector f

instance Inject (TcpServerSelector MarloweRuntime) RootSelector where
  inject = injectSelector MarloweRuntimeServer

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  MarloweRuntimeServer sel -> renderTcpServerSelectorOTel "marlowe-proxy" sel
