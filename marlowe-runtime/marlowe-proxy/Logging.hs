{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  where

import Control.Monad.Event.Class
import Language.Marlowe.Protocol.Server (MarloweRuntimeServerSelector, renderMarloweRuntimeServerSelectorOTel)
import Network.Protocol.Driver (TcpServerSelector, renderTcpServerSelectorOTel)
import Observe.Event.Explicit (injectSelector)
import Observe.Event.Render.OpenTelemetry (RenderSelectorOTel)

data RootSelector f where
  MarloweRuntimeServer :: TcpServerSelector MarloweRuntimeServerSelector f -> RootSelector f

instance Inject (TcpServerSelector MarloweRuntimeServerSelector) RootSelector where
  inject = injectSelector MarloweRuntimeServer

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  MarloweRuntimeServer sel -> renderTcpServerSelectorOTel "MarloweRuntimeService" renderMarloweRuntimeServerSelectorOTel sel
