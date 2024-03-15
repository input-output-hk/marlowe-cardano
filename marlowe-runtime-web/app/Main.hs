{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Colog (LogAction (LogAction), cmap, fmtMessage, logTextStdout)
import Colog.Message (Message)
import Control.Concurrent.Component (runComponent_)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Marlowe (MarloweT (..))
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntimeTraced)
import Language.Marlowe.Runtime.Web.RuntimeServer (
  ServeRequest (..),
  ServeRequestField (ReqField, ResField),
  ServerDependencies (
    ServerDependencies,
    accessControlAllowOriginAll,
    connector,
    openAPIEnabled,
    runApplication
  ),
  ServerSelector (..),
  runtimeServer,
 )
import Network.HTTP.Types (
  HeaderName,
  HttpVersion (HttpVersion, httpMajor, httpMinor),
  Status (statusCode),
  hContentLength,
  hUserAgent,
 )
import Network.Protocol.Driver.Trace (TcpClientSelector, renderTcpClientSelectorOTel, sockAddrToAttributes)
import Network.Socket (PortNumber)
import Network.Wai (
  Request (
    httpVersion,
    rawPathInfo,
    remoteHost,
    requestBodyLength,
    requestHeaders,
    requestMethod
  ),
  RequestBodyLength (ChunkedBody, KnownLength),
  responseHeaders,
  responseStatus,
 )
import Language.Marlowe.Runtime.Web.Server
import Language.Marlowe.Runtime.Web.Server.Logging (renderServerSelectorOTel)
import Network.Wai.Handler.Warp (
  run,
 )
import Observe.Event (injectSelector)
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace (
  InstrumentationLibrary (
    InstrumentationLibrary,
    libraryName,
    libraryVersion
  ),
  PrimitiveAttribute (IntAttribute, TextAttribute),
  SpanKind (Server),
  ToAttribute (toAttribute),
 )
import Options (
  Options (
    Options,
    accessControlAllowOriginAll,
    openAPIEnabled,
    port,
    runtimeHost,
    runtimePort
  ),
  getOptions,
 )
import OpenTelemetry.Trace
import Options
import Paths_marlowe_runtime_web (version)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import UnliftIO (MonadUnliftIO, newMVar, withMVar)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Options{..} <- getOptions
  runAppMTraced instrumentationLibrary (renderServerSelectorOTel port) do
    dependencies <- connectToMarloweRuntimeTraced (injectSelector RuntimeClient) runtimeHost runtimePort $ MarloweT do
      connector <- ask
      pure
        ServerDependencies
          { openAPIEnabled
          , accessControlAllowOriginAll
          , runApplication = run $ fromIntegral port
          , connector
          }
    runComponent_ runtimeServer dependencies
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-web-server"
        , libraryVersion = fromString $ showVersion version
        }

concurrentLogger :: (MonadUnliftIO m) => IO (LogAction m Message)
concurrentLogger = do
  lock <- newMVar ()
  let LogAction baseAction = cmap fmtMessage logTextStdout
  pure $ LogAction $ withMVar lock . const . baseAction
