{-# LANGUAGE GADTs #-}

module Main
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, bracketOnError, throwIO)
import Data.Either (fromRight)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Protocol.Sync.Server (marloweSyncServerPeer)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncQuery(..), RuntimeChainSeekClient, WithGenesis(..), runtimeChainSeekCodec)
import Language.Marlowe.Runtime.History (HistoryDependencies(..), history)
import Language.Marlowe.Runtime.History.Api (historyJobCodec, historyQueryCodec)
import Language.Marlowe.Runtime.History.Store (hoistHistoryQueries)
import Language.Marlowe.Runtime.History.Store.Memory (mkHistoryQueriesInMemory)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (acceptRunServerPeerOverSocket, runClientPeerOverSocket)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , HostName
  , PortNumber
  , SocketOption(..)
  , SocketType(..)
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  , withSocketsDo
  )
import Options.Applicative
  ( auto
  , execParser
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , strOption
  , value
  )

main :: IO ()
main = run =<< getOptions

clientHints :: AddrInfo
clientHints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  jobAddr <- resolve commandPort
  queryAddr <- resolve queryPort
  syncAddr <- resolve syncPort
  bracket (openServer jobAddr) close \jobSocket ->
    bracket (openServer queryAddr) close \querySocket -> do
      bracket (openServer syncAddr) close \syncSocket -> do
        slotConfig <- queryChainSync GetSlotConfig
        securityParameter <- queryChainSync GetSecurityParameter
        let

          connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
          connectToChainSeek client = do
            addr' <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekPort)
            runClientPeerOverSocket throwIO addr' runtimeChainSeekCodec (chainSeekClientPeer Genesis) client

          acceptRunJobServer = acceptRunServerPeerOverSocket throwIO jobSocket historyJobCodec jobServerPeer
          acceptRunQueryServer = acceptRunServerPeerOverSocket throwIO querySocket historyQueryCodec queryServerPeer
          acceptRunSyncServer = acceptRunServerPeerOverSocket throwIO syncSocket codecMarloweSync marloweSyncServerPeer
        let followerPageSize = 1024 -- TODO move to config with a default
        historyQueries <- atomically $ hoistHistoryQueries atomically <$> mkHistoryQueriesInMemory
        runComponent_ history HistoryDependencies{..}
  where
    openServer addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket

    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync query = do
      addr <- head <$> getAddrInfo (Just clientHints) (Just chainSeekHost) (Just $ show chainSeekQueryPort)
      result <- runClientPeerOverSocket throwIO addr codecQuery queryClientPeer $ liftQuery query
      pure $ fromRight (error "failed to query chain seek server") result

data Options = Options
  { chainSeekPort      :: PortNumber
  , chainSeekQueryPort :: PortNumber
  , commandPort        :: PortNumber
  , queryPort          :: PortNumber
  , syncPort           :: PortNumber
  , chainSeekHost      :: HostName
  , host               :: HostName
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options
      <$> chainSeekPortParser
      <*> chainSeekQueryPortParser
      <*> commandPortParser
      <*> queryPortParser
      <*> syncPortParser
      <*> chainSeekHostParser
      <*> hostParser

    chainSeekPortParser = option auto $ mconcat
      [ long "chain-seek-port-number"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server."
      , showDefault
      ]

    chainSeekQueryPortParser = option auto $ mconcat
      [ long "chain-seek-query-port-number"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The port number of the chain sync query server."
      , showDefault
      ]

    commandPortParser = option auto $ mconcat
      [ long "command-port"
      , value 3717
      , metavar "PORT_NUMBER"
      , help "The port number to run the job server on."
      , showDefault
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port"
      , value 3718
      , metavar "PORT_NUMBER"
      , help "The port number to run the query server on."
      , showDefault
      ]

    syncPortParser = option auto $ mconcat
      [ long "sync-port"
      , value 3719
      , metavar "PORT_NUMBER"
      , help "The port number to run the sync server on."
      , showDefault
      ]

    chainSeekHostParser = strOption $ mconcat
      [ long "chain-seek-host"
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name to run the history server on."
      , showDefault
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract history service for Marlowe Runtime"
      , header "marlowe-history : a contract history service for the Marlowe Runtime."
      ]
