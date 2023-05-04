{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main
  where

import Control.Arrow ((&&&))
import Control.Concurrent.Component (runComponent_)
import Control.Exception (bracket)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Trans.Marlowe (MarloweTracedT(..))
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntimeTraced)
import Language.Marlowe.Runtime.Web.Server
import Language.Marlowe.Runtime.Web.Server.Monad (runBackendM)
import Network.HTTP.Types
import Network.Protocol.Driver (renderTcpClientSelectorOTel, sockAddrToAttributes)
import Network.Socket (PortNumber)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Observe.Event (injectSelector)
import Observe.Event.Render.OpenTelemetry (OTelRendered(..), RenderSelectorOTel, tracerEventBackend)
import OpenTelemetry.Trace
import Options
import Paths_marlowe_runtime_web (version)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Options{..} <- getOptions
  withTracer \tracer -> do
    let backend = tracerEventBackend tracer $ renderServerSelectorOTel port
    flip runReaderT backend $ runBackendM do
      dependencies <- connectToMarloweRuntimeTraced (injectSelector RuntimeClient) runtimeHost runtimePort $ MarloweTracedT do
        marloweTracedContext <- ask
        pure ServerDependencies
          { openAPIEnabled
          , accessControlAllowOriginAll
          , runApplication = run $ fromIntegral port
          , marloweTracedContext
          }
      runComponent_ server dependencies
  where
    withTracer f = bracket
      initializeGlobalTracerProvider
      shutdownTracerProvider
      \provider -> f $ makeTracer provider instrumentationLibrary tracerOptions

    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-web-server"
      , libraryVersion = fromString $ showVersion version
      }

renderServerSelectorOTel :: PortNumber -> RenderSelectorOTel ServerSelector
renderServerSelectorOTel port = \case
  Http sel -> renderServeRequestOTel port sel
  RuntimeClient sel -> renderTcpClientSelectorOTel sel

-- TODO push upstream into eventuo11y-batteries. Attempts to follow
-- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/http/
-- as close as possible.
renderServeRequestOTel :: PortNumber -> RenderSelectorOTel ServeRequest
renderServeRequestOTel port = \case
  ServeRequest req -> OTelRendered
    { eventName = decodeUtf8 $ requestMethod req -- We don't have a low-cardinality route, so we just use the method name.
    , eventKind = Server
    , renderField = \case
        ReqField req' -> catMaybes
          [ ("http.request_content_length",) <$> contentLength
          , Just ("http.method", toAttribute $ T.toUpper $ decodeUtf8 $ requestMethod req)
          , Just ("http.version", fromString let HttpVersion{..} = httpVersion req in show httpMajor <> "." <> show httpMinor)
          , ("user_agent.original",) . toAttribute . TextAttribute <$> userAgent
          , Just ("http.target", toAttribute $ decodeUtf8 $ rawPathInfo req)
          , ("http.client_ip",) . toAttribute . TextAttribute<$> clientIp
          , Just ("http.scheme", "http")
          , Just ("net.host.name", "localhost")
          , Just ("net.host.port", toAttribute $ IntAttribute $ fromIntegral port)
          ] <> headerAttributes <> sockAddrToAttributes True (remoteHost req)
          where
            headers = requestHeaders req'
            contentLength = case requestBodyLength req of
              ChunkedBody -> Nothing
              KnownLength len -> Just $ toAttribute $ IntAttribute $ fromIntegral len
            userAgent = readMaybe . show =<< lookup hUserAgent headers
            clientIp = readMaybe . show =<< lookup "x-forwarded-for" headers
            headerAttributes =fmap ((toHeaderName . fst . head) &&& toAttribute . fmap (decodeUtf8 . snd))
              $ groupBy (on (==) fst)
              $ sortOn fst headers
            toHeaderName :: HeaderName -> T.Text
            toHeaderName = mappend "http.request.header." . read . show
        ResField res -> catMaybes
          [ ("http.response_content_length",) . toAttribute . IntAttribute <$> contentLength
          , Just ("http.status_code", toAttribute $ IntAttribute $ fromIntegral $ statusCode $ responseStatus res)
          ] <> headerAttributes <> sockAddrToAttributes True (remoteHost req)
          where
            headers = responseHeaders res
            contentLength = readMaybe . read . show =<< lookup hContentLength headers
            headerAttributes =fmap ((toHeaderName . fst . head) &&& toAttribute . fmap (decodeUtf8 . snd))
              $ groupBy (on (==) fst)
              $ sortOn fst headers
            toHeaderName :: HeaderName -> T.Text
            toHeaderName = mappend "http.response.header." . read . show
    }
