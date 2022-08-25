module Main where

import Control.Category ((>>>))
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad
import Data.Foldable (fold)
import qualified Data.Text.IO as T
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.Core.Api (ContractId, parseContractId, renderContractId)
import Language.Marlowe.Runtime.History.Api
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Job.Client (jobClientPeer, liftCommand)
import Network.Socket (AddrInfo (..), HostName, PortNumber, Socket, SocketType (..), close, connect, defaultHints,
                       getAddrInfo, openSocket)
import Network.TypedProtocol (Driver (..), runPeerWithDriver)
import qualified Options.Applicative as O
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = do
  let hints = defaultHints { addrSocketType = Stream }
  commandAddr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show commandPort)
  bracket (open commandAddr) close (runCommand command)
  where
    open addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock

runCommand :: Command -> Socket -> IO ()
runCommand command commandSocket = case command of

  Add contractId -> runHistoryCommand (FollowContract contractId) >>= \case
    Left err -> failWith $ "Failed to follow contract: " <> show err
    Right hadEffect -> do
      when hadEffect $ T.putStrLn $ renderContractId contractId

  Rm contractId -> runHistoryCommand (StopFollowingContract contractId) >>= \case
    Right hadEffect -> do
      when hadEffect $ T.putStrLn $ renderContractId contractId
    Left err -> absurd err

  where
    runHistoryCommand :: HistoryCommand Void err result -> IO (Either err result)
    runHistoryCommand cmd =
      fst <$> runPeerWithDriver commandDriver (peer cmd) (startDState commandDriver)

    peer = jobClientPeer . liftCommand

    commandDriver = mkDriver throwIO historyJobCodec commandChannel
    commandChannel = socketAsChannel commandSocket

failWith :: String -> IO a
failWith msg = do
  hPutStrLn stderr msg
  exitFailure

data Options = Options
  { commandPort :: !PortNumber
  , host        :: !HostName
  , command     :: !Command
  }

data Command
  = Add ContractId
  | Rm ContractId

getOptions :: IO Options
getOptions = O.execParser $ O.info parser infoMod
  where
    parser = O.helper <*> (Options <$> portParser <*> hostParser <*> exampleParser)
    portParser = O.option O.auto $ mconcat
      [ O.long "command-port"
      , O.short 'p'
      , O.value 3717
      , O.metavar "PORT_NUMBER"
      , O.help "The port number for issuing commands to the history server"
      ]
    hostParser = O.strOption $ mconcat
      [ O.long "host"
      , O.short 'h'
      , O.value "127.0.0.1"
      , O.metavar "HOST_NAME"
      , O.help "The hostname of the history server to connect to"
      ]
    exampleParser = O.subparser $ fold
      [ O.command "add" $ Add <$> addParser
      , O.command "rm" $ Rm <$> rmParser
      ]
    addParser = O.info (contractIdParser "follow") $ O.progDesc "Start following a contract by its ID."
    rmParser = O.info (contractIdParser "stop following") $ O.progDesc "Stop following a contract by its ID."
    contractIdParser verb = O.argument parser' info
      where
        parser' = O.eitherReader $ parseContractId >>> \case
          Nothing  -> Left "Invalid contract ID - expected format: <hex-tx-id>#<tx-out-ix>"
          Just cid -> Right cid
        info = fold
          [ O.metavar "CONTRACT_ID"
          , O.help $ "The ID of the contract to " <> verb
          ]
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Example Chain Seek client for the Marlowe Chain Sync"
      ]
