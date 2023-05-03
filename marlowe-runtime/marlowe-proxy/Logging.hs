{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  where

import Control.Monad.Event.Class
import Language.Marlowe.Protocol.Server (ProxySelector(..))
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Network.Protocol.Driver (TcpServerSelector, renderTcpServerSelectorOTel)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol.Codec (AnyMessageAndAgency(AnyMessageAndAgency))
import Observe.Event.Explicit (injectSelector)
import Observe.Event.Render.OpenTelemetry (OTelRendered(..), RenderSelectorOTel)
import OpenTelemetry.Trace

data RootSelector f where
  ProxySelector :: ProxySelector f -> RootSelector f
  MarloweRuntimeServer :: TcpServerSelector (Handshake MarloweRuntime) f -> RootSelector f

instance Inject (TcpServerSelector (Handshake MarloweRuntime)) RootSelector where
  inject = injectSelector MarloweRuntimeServer

renderRootSelectorOTel :: RenderSelectorOTel RootSelector
renderRootSelectorOTel = \case
  MarloweRuntimeServer sel -> renderTcpServerSelectorOTel sel
  ProxySelector sel -> case sel of
    Handshake -> OTelRendered
      { eventName = "marlowe/proxy/handshake"
      , eventKind = Client
      , renderField = pure . ("marlowe.proxy.handshake.signature",) . toAttribute
      }
    HandshakeAccepted -> OTelRendered
      { eventName = "marlowe/proxy/handshake/accepted"
      , eventKind = Client
      , renderField = \case
      }
    HandshakeRejected -> OTelRendered
      { eventName = "marlowe/proxy/handshake/accepted"
      , eventKind = Client
      , renderField = \case
      }
    SendMarloweSync -> OTelRendered
      { eventName = "marlowe/proxy/send/marlowe_sync"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.send.type", toAttribute messageType)
              , ("typed-protocols.message.send.parameters", toAttribute messageParameters)
              ]
      }
    SendMarloweHeaderSync -> OTelRendered
      { eventName = "marlowe/proxy/send/marlowe_header_sync"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.send.type", toAttribute messageType)
              , ("typed-protocols.message.send.parameters", toAttribute messageParameters)
              ]
      }
    SendMarloweQuery -> OTelRendered
      { eventName = "marlowe/proxy/send/marlowe_query"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.send.type", toAttribute messageType)
              , ("typed-protocols.message.send.parameters", toAttribute messageParameters)
              ]
      }
    SendTxJob -> OTelRendered
      { eventName = "marlowe/proxy/send/tx_job"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.send.type", toAttribute messageType)
              , ("typed-protocols.message.send.parameters", toAttribute messageParameters)
              ]
      }
    RecvMarloweSync -> OTelRendered
      { eventName = "marlowe/proxy/recv/marlowe_sync"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.recv.type", toAttribute messageType)
              , ("typed-protocols.message.recv.parameters", toAttribute messageParameters)
              ]
      }
    RecvMarloweHeaderSync -> OTelRendered
      { eventName = "marlowe/proxy/recv/marlowe_header_sync"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.recv.type", toAttribute messageType)
              , ("typed-protocols.message.recv.parameters", toAttribute messageParameters)
              ]
      }
    RecvMarloweQuery -> OTelRendered
      { eventName = "marlowe/proxy/recv/marlowe_query"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.recv.type", toAttribute messageType)
              , ("typed-protocols.message.recv.parameters", toAttribute messageParameters)
              ]
      }
    RecvTxJob -> OTelRendered
      { eventName = "marlowe/proxy/recv/tx_job"
      , eventKind = Client
      , renderField = \case
          AnyMessageAndAgency tok msg -> case messageAttributes tok msg of
            MessageAttributes{..} ->
              [ ("typed-protocols.message.recv.type", toAttribute messageType)
              , ("typed-protocols.message.recv.parameters", toAttribute messageParameters)
              ]
      }
