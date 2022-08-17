{-# LANGUAGE GADTs #-}
module Main where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad (forever)
import Data.Foldable (for_, traverse_)
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient, WithGenesis (Genesis), runtimeChainSeekCodec)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion (..), parseContractId)
import Language.Marlowe.Runtime.History.Follower (ContractChanges (..), ContractStep (..), CreateStep (..),
                                                  Follower (..), FollowerDependencies (..), SomeContractChanges (..),
                                                  mkFollower)
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (mkDriver)
import Network.Socket (AddrInfo (..), HostName, PortNumber, SocketType (..), close, connect, defaultHints, getAddrInfo,
                       openSocket, withSocketsDo)
import Network.TypedProtocol (runPeerWithDriver, startDState)
import Options.Applicative (argument, auto, execParser, fullDesc, header, help, info, long, maybeReader, metavar,
                            option, progDesc, short, strOption, value)
import System.IO (hPrint, stderr)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
  bracket (open addr) close \socket -> do
    let driver = mkDriver throwIO runtimeChainSeekCodec $ socketAsChannel socket
    let
      connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
      connectToChainSeek client = fst <$> runPeerWithDriver driver peer (startDState driver)
        where peer = chainSeekClientPeer Genesis client
    Follower{..} <- atomically $ mkFollower FollowerDependencies{..}
    Left result <- race runFollower (logChanges changes)
    case result of
      Left err -> hPrint stderr err
      Right () -> pure ()
  where
    open addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock

logChanges :: STM (Maybe SomeContractChanges) -> IO Void
logChanges readChanges = forever do
  SomeContractChanges version ContractChanges{..} <- atomically do
    mchanges <- readChanges
    maybe retry pure mchanges
  for_ rollbackTo \slotNo -> do
    putStrLn $ "Rollback to slot " <> show slotNo
  (traverse_ . traverse_) (logStep version) steps

logStep :: MarloweVersion v -> ContractStep v -> IO ()
logStep version = \case
  Create CreateStep{..} -> do
    putStr "Create{scriptHash="
    putStr $ show scriptHash
    putStr ",datum="
    putStr case version of
      MarloweV1 -> show datum
    putStr "}"
  Transaction _ -> error "not implemented"
  RedeemPayout _ -> error "not implemented"

data Options = Options
  { port       :: PortNumber
  , host       :: HostName
  , contractId :: ContractId
  }

getOptions :: IO Options
getOptions = execParser $ info parser infoMod
  where
    parser = Options <$> portParser <*> hostParser <*> contractIdParser

    portParser = option auto $ mconcat
      [ long "port-number"
      , short 'p'
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server"
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'p'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server"
      ]

    contractIdParser = argument (maybeReader parseContractId) $ mconcat
      [ metavar "CONTRACT_ID"
      , help "The UTxO that created the contract"
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract follower for Marlowe Runtime"
      , header "marlowe-follower : a contract follower for the Marlowe Runtime."
      ]
