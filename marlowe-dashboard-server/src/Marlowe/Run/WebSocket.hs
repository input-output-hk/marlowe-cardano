{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Marlowe.Run.WebSocket where

import Cardano.Prelude hiding (log)
import Colog (LogAction (..), Message, Msg (..))
import Colog.Core.Severity (Severity (..))
import Data.Aeson (FromJSON)
import qualified Data.Aeson as JSON
import Data.Aeson.Types (ToJSON)
import Data.Text as Text
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Network.WebSockets as WS
import Network.WebSockets.Connection (Connection, PendingConnection, receiveData, withPingThread)

log :: LogAction IO Message -> Severity -> Text -> IO ()
log logAction msgSeverity msgText =
    withFrozenCallStack (unLogAction logAction $ Msg{ msgStack = callStack, .. })

handle :: MonadIO m => LogAction IO Message -> PendingConnection -> m ()
handle logAction pending = liftIO $ do
  connection <- WS.acceptRequest pending
  uuid <- nextRandom
  log logAction Debug "Received ws connection"
  withPingThread connection 30 (pure ()) $ handleRequest logAction connection uuid

handleRequest :: LogAction IO Message -> Connection -> UUID -> IO ()
handleRequest logAction connection uuid = forever $ do
  msg <- receiveData connection
  print msg
  v <- case JSON.eitherDecode msg of
    Left e -> do
      log logAction Error $ Text.pack e
      pure False
    Right (ServerMsg v) -> pure v
  let resp = JSON.encode (ClientMsg (not v))
  WS.sendTextData connection resp

newtype StreamToServer
  = ServerMsg Bool
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype StreamToClient
  = ClientMsg Bool
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
