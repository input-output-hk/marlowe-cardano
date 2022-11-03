{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Language.Marlowe.Runtime.Logging.Colog.LogIO.Network
  where

import Colog (logDebug, logError, logInfo)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Language.Marlowe.Runtime.Logging.Colog (prefixLogger)
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

openServerSocket :: HasCallStack => AddrInfo -> LogIO Socket
openServerSocket addr = do
  let
    closeOnError socket = do
      logError . T.pack $ "Socket initialization failed."
      liftIO $ close socket

  bracketOnErrorLogIO (liftIO $ openSocket addr) closeOnError \socket -> do
    logInfo . T.pack $ "Openning up a socket."
    liftIO $ do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket

withServerSocket :: HasCallStack => ProtocolName -> AddrInfo -> (Socket -> LogIO ()) -> LogIO ()
withServerSocket (ProtocolName name) addr action = do
  let prefix = T.pack $ "[" <> name <> " server at " <> show (addrAddress addr) <> "] "
  let
    close' socket = prefixLogger prefix do
      logInfo "Closing a socket."
      liftIO $ close socket
    open' = prefixLogger prefix $ openServerSocket addr
  bracketLogIO open' close' action

clientPrefix :: ProtocolName -> AddrInfo -> String
clientPrefix (ProtocolName name) addr = "[" <> name <> " client of " <> show (addrAddress addr) <> "] "

openClientSocket :: AddrInfo -> LogIO Socket
openClientSocket addr = do
  let
    closeOnError socket = do
      logError "Connection failed."
      liftIO $ close socket
  bracketOnErrorLogIO (liftIO $ openSocket addr) closeOnError \sock -> do
    liftIO $ connect sock $ addrAddress addr
    logDebug "Connected."
    pure sock

withClientSocket :: HasCallStack => ProtocolName -> AddrInfo -> (Socket -> LogIO a) -> LogIO a
withClientSocket pn addr action = do
  let
    prefix = T.pack $ clientPrefix pn addr
    open = prefixLogger prefix do
      logDebug "Connecting..."
      openClientSocket addr

    close' socket = do
      liftIO $ close socket
  bracketLogIO open close' action `catchLogIO` \(err :: SomeException) -> do
    let
      msg = T.pack $ "Failure: " <> show err
    throwLogIO msg err

