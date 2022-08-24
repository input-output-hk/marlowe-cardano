{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.STM (atomically)
import Control.Exception (bracket, bracketOnError, throwIO)
import Data.Either (fromRight)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery (..), RuntimeChainSeekClient, WithGenesis (..),
                                               runtimeChainSeekCodec)
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.History (History (..), HistoryDependencies (..), mkHistory)
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (AddrInfo (..), HostName, PortNumber, SocketType (..), close, connect, defaultHints, getAddrInfo,
                       openSocket, withSocketsDo)
import Network.TypedProtocol (runPeerWithDriver, startDState)
import Options.Applicative (auto, execParser, fullDesc, header, help, info, long, metavar, option, progDesc, short,
                            strOption, value)

main :: IO ()
main = run =<< getOptions

hints :: AddrInfo
hints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  slotConfig <- queryChainSync GetSlotConfig
  securityParameter <- queryChainSync GetSecurityParameter
  let
    connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
    connectToChainSeek client = do
      chainSeekAddr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
      bracket (open chainSeekAddr) close \chainSeekSocket -> do
        let driver = mkDriver throwIO runtimeChainSeekCodec $ socketAsChannel chainSeekSocket
        let peer = chainSeekClientPeer Genesis client
        fst <$> runPeerWithDriver driver peer (startDState driver)
  let getMarloweVersion = Core.getMarloweVersion
  History{..} <- atomically $ mkHistory HistoryDependencies{..}
  runHistory
  where
    open addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock

    queryChainSync :: ChainSyncQuery Void e a -> IO a
    queryChainSync query = do
      addr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show queryPort)
      bracket (open addr) close \socket -> do
        let driver = mkDriver throwIO codecQuery $ socketAsChannel socket
        let client = liftQuery query
        let peer = queryClientPeer client
        result <- fst <$> runPeerWithDriver driver peer (startDState driver)
        pure $ fromRight (error "failed to query chain seek server") result

data Options = Options
  { port      :: PortNumber
  , queryPort :: PortNumber
  , host      :: HostName
  }

getOptions :: IO Options
getOptions = execParser $ info parser infoMod
  where
    parser = Options <$> portParser <*> queryPortParser <*> hostParser

    portParser = option auto $ mconcat
      [ long "port-number"
      , short 'p'
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server"
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port-number"
      , short 'p'
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The query port number of the chain seek server"
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'p'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server"
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract follower for Marlowe Runtime"
      , header "marlowe-follower : a contract follower for the Marlowe Runtime."
      ]
