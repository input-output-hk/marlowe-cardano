{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Server.Logging where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Maybe (catMaybes)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

import Language.Marlowe.Runtime.Web.RuntimeServer (
  ServeRequest (..),
  ServeRequestField (ReqField, ResField),
  ServerSelector (..),
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
import Observe.Event.Render.OpenTelemetry (OTelRendered (..), RenderSelectorOTel)
import OpenTelemetry.Trace (
  PrimitiveAttribute (IntAttribute, TextAttribute),
  SpanKind (Server),
  ToAttribute (toAttribute),
 )
import Text.Read (readMaybe)

renderServerSelectorOTel :: PortNumber -> RenderSelectorOTel (ServerSelector TcpClientSelector)
renderServerSelectorOTel port = \case
  Http sel -> renderServeRequestOTel port sel
  RuntimeClient sel -> renderTcpClientSelectorOTel sel

-- TODO push upstream into eventuo11y-batteries. Attempts to follow
-- https://opentelemetry.io/docs/reference/specification/trace/semantic_conventions/http/
-- as close as possible.
renderServeRequestOTel :: PortNumber -> RenderSelectorOTel ServeRequest
renderServeRequestOTel port = \case
  ServeRequest req ->
    OTelRendered
      { eventName = decodeUtf8 $ requestMethod req -- We don't have a low-cardinality route, so we just use the method name.
      , eventKind = Server
      , renderField = \case
          ReqField req' ->
            catMaybes
              [ ("http.request_content_length",) <$> contentLength
              , Just ("http.method", toAttribute $ T.toUpper $ decodeUtf8 $ requestMethod req)
              , Just ("http.version", fromString let HttpVersion{..} = httpVersion req in show httpMajor <> "." <> show httpMinor)
              , ("user_agent.original",) . toAttribute . TextAttribute <$> userAgent
              , Just ("http.target", toAttribute $ decodeUtf8 $ rawPathInfo req)
              , ("http.client_ip",) . toAttribute . TextAttribute <$> clientIp
              , Just ("http.scheme", "http")
              , Just ("net.host.name", "localhost")
              , Just ("net.host.port", toAttribute $ IntAttribute $ fromIntegral port)
              ]
              <> headerAttributes
              <> sockAddrToAttributes True (remoteHost req)
            where
              headers = requestHeaders req'
              contentLength = case requestBodyLength req of
                ChunkedBody -> Nothing
                KnownLength len -> Just $ toAttribute $ IntAttribute $ fromIntegral len
              userAgent = readMaybe . show =<< lookup hUserAgent headers
              clientIp = readMaybe . show =<< lookup "x-forwarded-for" headers
              headerAttributes =
                fmap ((toHeaderName . fst . head) &&& toAttribute . fmap (decodeUtf8 . snd)) $
                  groupBy (on (==) fst) $
                    sortOn fst headers
              toHeaderName :: HeaderName -> T.Text
              toHeaderName = mappend "http.request.header." . read . show
          ResField res ->
            catMaybes
              [ ("http.response_content_length",) . toAttribute . IntAttribute <$> contentLength
              , Just ("http.status_code", toAttribute $ IntAttribute $ fromIntegral $ statusCode $ responseStatus res)
              ]
              <> headerAttributes
              <> sockAddrToAttributes True (remoteHost req)
            where
              headers = responseHeaders res
              contentLength = readMaybe . read . show =<< lookup hContentLength headers
              headerAttributes =
                fmap ((toHeaderName . fst . head) &&& toAttribute . fmap (decodeUtf8 . snd)) $
                  groupBy (on (==) fst) $
                    sortOn fst headers
              toHeaderName :: HeaderName -> T.Text
              toHeaderName = mappend "http.response.header." . read . show
      }
