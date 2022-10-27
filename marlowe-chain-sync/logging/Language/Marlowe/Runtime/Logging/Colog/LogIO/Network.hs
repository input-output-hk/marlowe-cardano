{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.Logging.Colog.LogIO.Network
  where

import Colog (logDebug, logError, logInfo)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, bracketLogIO, bracketOnErrorLogIO, catchLogIO, throwLogIO)
import Network.Socket
  ( AddrInfo(..)
  , Socket
  , SocketOption(ReuseAddr)
  , bind
  , close
  , connect
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  )

newtype ProtocolName = ProtocolName String

openServerSocket :: HasCallStack => ProtocolName -> AddrInfo -> LogIO Socket
openServerSocket (ProtocolName name) addr = do
  let
    closeOnError socket = do
      logError . T.pack $ "Initialization of " <> show name <> " server failed. Socket setup failure at " <> show (addrAddress addr)
      liftIO $ close socket

  bracketOnErrorLogIO (liftIO $ openSocket addr) closeOnError \socket -> do
    logInfo . T.pack $ "Starting " <> show name <> " server at " <> show (addrAddress addr)
    liftIO $ do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket

withServerSocket :: HasCallStack => ProtocolName -> AddrInfo -> (Socket -> LogIO ()) -> LogIO ()
withServerSocket protocolName@(ProtocolName name) addr action = do
  let
    close' socket = do
      logInfo . T.pack $ "Closing service " <> show name <> " socket at " <> show (addrAddress addr)
      liftIO $ close socket
  bracketLogIO (openServerSocket protocolName addr) close' action

openClientSocket :: ProtocolName -> AddrInfo -> LogIO Socket
openClientSocket (ProtocolName name) addr = do
  let
    closeOnError socket = do
      logError . T.pack $ "Initialization of " <> show name <> " client failed. Tried to connect to " <> show (addrAddress addr)
      liftIO $ close socket
  bracketOnErrorLogIO (liftIO $ openSocket addr) closeOnError \sock -> do
    liftIO $ connect sock $ addrAddress addr
    pure sock

withClientSocket :: HasCallStack => ProtocolName -> AddrInfo -> (Socket -> LogIO a) -> LogIO a
withClientSocket protocolName@(ProtocolName name) addr action = do
  let
    open = do
      logDebug . T.pack $ "Opening up " <> show name <> " client connection to " <> show (addrAddress addr)
      openClientSocket protocolName addr

    close' socket = do
      logDebug . T.pack $ "Closing " <> show name <> " client connection"
      liftIO $ close socket

  bracketLogIO open close' action `catchLogIO` \(err :: SomeException) -> do
    let
      msg = T.pack $ show name <> " client failure: " <> show err
    throwLogIO msg err

