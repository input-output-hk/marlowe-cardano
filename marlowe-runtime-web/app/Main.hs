{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Colog (LogAction(LogAction), cmap, fmtMessage, logTextStdout)
import Colog.Message (Message)
import Control.Arrow ((&&&))
import Control.Concurrent.Component (runComponent_)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Marlowe (MarloweT(..))
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Version (showVersion)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntimeTraced)
import Language.Marlowe.Runtime.Web.Server
import Network.HTTP.Types
import Network.Protocol.Driver.Trace (TcpClientSelector, renderTcpClientSelectorOTel, sockAddrToAttributes)
import Network.Socket (PortNumber)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Observe.Event (injectSelector)
import Observe.Event.Render.OpenTelemetry (OTelRendered(..), RenderSelectorOTel)
import OpenTelemetry.Trace
import Options
import Paths_marlowe_runtime_web (version)
import System.IO (BufferMode(..), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)
import UnliftIO (MonadUnliftIO, newMVar, withMVar)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Options{..} <- getOptions
  runAppMTraced instrumentationLibrary (renderServerSelectorOTel port) do
    dependencies <- connectToMarloweRuntimeTraced (injectSelector RuntimeClient) runtimeHost runtimePort $ MarloweT do
      connector <- ask
      pure ServerDependencies
        { openAPIEnabled
        , accessControlAllowOriginAll
        , runApplication = run $ fromIntegral port
        , connector
        }
    runComponent_ server dependencies
  where
    instrumentationLibrary = InstrumentationLibrary
      { libraryName = "marlowe-web-server"
      , libraryVersion = fromString $ showVersion version
      }

concurrentLogger :: MonadUnliftIO m => IO (LogAction m Message)
concurrentLogger = do
  lock <- newMVar ()
  let LogAction baseAction = cmap fmtMessage logTextStdout
  pure $ LogAction $ withMVar lock . const . baseAction

renderServerSelectorOTel :: PortNumber -> RenderSelectorOTel (ServerSelector TcpClientSelector)
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
