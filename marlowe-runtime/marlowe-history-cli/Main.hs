{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Main where

import Control.Category ((>>>))
import Control.Exception (bracket, bracketOnError, throwIO)
import Data.ByteString.Base16 (encodeBase16)
import Data.Foldable (asum, fold, for_)
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void, absurd)
import GHC.Base (when)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Pretty (pretty)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), BlockHeaderHash (..), BlockNo (..), SlotNo (..),
                                               TxId (..), TxOutRef (..), toBech32)
import Language.Marlowe.Runtime.Core.Api (ContractId (..), MarloweVersion (..), Transaction (..),
                                          TransactionOutput (..), TransactionScriptOutput (..), parseContractId,
                                          renderContractId)
import Language.Marlowe.Runtime.History.Api
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Job.Client (jobClientPeer, liftCommand)
import Network.Protocol.Query.Client (ClientStInit (..), ClientStNext (..), ClientStNextCanReject (..),
                                      ClientStPage (..), QueryClient (..), queryClientPeer)
import Network.Socket (AddrInfo (..), HostName, PortNumber, Socket, SocketType (..), close, connect, defaultHints,
                       getAddrInfo, openSocket)
import Network.TypedProtocol (Driver (..), runPeerWithDriver)
import qualified Options.Applicative as O
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)
import System.Exit (die)
import Text.PrettyPrint.Leijen (Doc, indent, putDoc)

main :: IO ()
main = run =<< getOptions

run :: Options -> IO ()
run Options{..} = do
  case command of
    Add contractId -> runHistoryCommand (FollowContract contractId) >>= \case
      Left err -> die $ "Failed to follow contract: " <> show err
      Right hadEffect -> do
        when hadEffect $ T.putStrLn $ renderContractId contractId

    Rm contractId -> runHistoryCommand (StopFollowingContract contractId) >>= \case
      Right hadEffect -> do
        when hadEffect $ T.putStrLn $ renderContractId contractId
      Left err -> absurd err

    Ls lsCommand -> do
      queryAddr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show queryPort)
      bracket (open queryAddr) close $ runLsCommand lsCommand

    Show contractId -> do
      queryAddr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show queryPort)
      bracket (open queryAddr) close $ runShowCommand contractId
  where
    hints = defaultHints { addrSocketType = Stream }

    open addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock

    runHistoryCommand :: HistoryCommand Void err result -> IO (Either err result)
    runHistoryCommand cmd = do
      commandAddr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show commandPort)
      bracket (open commandAddr) close \commandSocket -> do
        let channel = socketAsChannel commandSocket
        let driver = mkDriver throwIO historyJobCodec channel
        let peer = jobClientPeer $ liftCommand cmd
        fst <$> runPeerWithDriver driver peer (startDState driver)

runLsCommand :: LsCommand -> Socket -> IO ()
runLsCommand LsCommand{..} socket = void $ runPeerWithDriver driver peer (startDState driver)
  where
    driver = mkDriver throwIO historyQueryCodec $ socketAsChannel socket
    peer = queryClientPeer client
    client = QueryClient $ pure $ SendMsgRequest GetFollowedContracts ClientStNextCanReject
      { recvMsgReject = absurd
      , recvMsgNextPage = nextPage
      }
    nextPage results mNextPageDelimiter = do
      void $ Map.traverseWithKey printResult $ Map.filter filterResult results
      pure case mNextPageDelimiter of
        Nothing -> SendMsgDone ()
        Just nextPageDelimiter -> SendMsgRequestNext nextPageDelimiter ClientStNext
          { recvMsgNextPage = nextPage
          }
    filterResult = case failedFlag of
      LsShowFailed -> const True
      LsHideFailed -> \case
        Failed _ -> False
        _        -> True
    printResult = case statusFlag of
      LsHideStatus -> const . printContractId
      LsShowStatus -> printStatus
    printContractId = T.putStrLn . renderContractId
    printStatus contractId status = do
      T.putStr $ renderContractId contractId
      putStr " Status: "
      print status

runShowCommand :: ContractId -> Socket -> IO ()
runShowCommand = error "not implemented"

data Options = Options
  { commandPort :: !PortNumber
  , queryPort   :: !PortNumber
  , host        :: !HostName
  , command     :: !Command
  }

data Command
  = Add ContractId
  | Rm ContractId
  | Ls LsCommand
  | Show ContractId

data LsStatusFlag
  = LsShowStatus
  | LsHideStatus

data LsFailedFlag
  = LsShowFailed
  | LsHideFailed

data LsCommand = LsCommand
  { statusFlag:: LsStatusFlag
  , failedFlag :: LsFailedFlag
  }

getOptions :: IO Options
getOptions = O.execParser $ O.info parser infoMod
  where
    parser = O.helper <*> (Options <$> commandPortParser <*> queryPortParser <*> hostParser <*> commandParser)
    commandPortParser = O.option O.auto $ mconcat
      [ O.long "command-port"
      , O.value 3717
      , O.metavar "PORT_NUMBER"
      , O.help "The port number for issuing commands to the history server. Default: 3717"
      ]
    queryPortParser = O.option O.auto $ mconcat
      [ O.long "query-port"
      , O.value 3718
      , O.metavar "PORT_NUMBER"
      , O.help "The port number for issuing queries to the history server. Default: 3718"
      ]
    hostParser = O.strOption $ mconcat
      [ O.long "host"
      , O.short 'h'
      , O.value "127.0.0.1"
      , O.metavar "HOST_NAME"
      , O.help "The hostname of the history server to connect to. Default value: 127.0.0.1"
      ]

    commandParser = asum
      [ O.hsubparser $ fold
          [ O.commandGroup "Commands:"
          , O.command "add" $ Add <$> addParser
          , O.command "rm" $ Rm <$> rmParser
          ]
      , O.hsubparser $ fold
          [ O.commandGroup "Queries:"
          , O.command "ls" $ Ls <$> lsParser
          , O.command "show" $ Show <$> showParser
          ]
      ]

    addParser = O.info (contractIdParser "follow") $ O.progDesc "Start following a contract by its ID."
    rmParser = O.info (contractIdParser "stop following") $ O.progDesc "Stop following a contract by its ID."
    showParser = O.info (contractIdParser "show history for") $ O.progDesc "Show the history of a contract"
    lsParser = O.info (LsCommand <$> showStatusParser <*> showFailedParser) $ O.progDesc "List followed contracts"
      where
        showStatusParser = O.flag LsHideStatus LsShowStatus $ fold
          [ O.long "show-status"
          , O.short 's'
          , O.help "Show the status of the contract as well as its ID"
          ]
        showFailedParser = O.flag LsHideFailed LsShowFailed $ fold
          [ O.long "show-failed"
          , O.short 'f'
          , O.help "Include contracts whose follower encountered an error"
          ]

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

logCreateStep :: MarloweVersion v -> ContractId -> BlockHeader -> CreateStep v -> IO ()
logCreateStep version contractId BlockHeader{..} CreateStep{..} = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "transaction "
  putStr $ T.unpack $ encodeBase16 $ unTxId $ txId $ unContractId contractId
  putStrLn " (creation)"
  setSGR [Reset]
  putStr "ContractId:      "
  putStrLn $ T.unpack $ renderContractId contractId
  putStr "SlotNo:          "
  print $ unSlotNo slotNo
  putStr "BlockNo:         "
  print $ unBlockNo blockNo
  putStr "BlockId:         "
  putStrLn $ T.unpack $ encodeBase16 $ unBlockHeaderHash headerHash
  for_ (toBech32 scriptAddress) \addr -> do
    putStr "ScriptAddress:   "
    putStrLn $ T.unpack addr
  putStr "Marlowe Version: "
  putStrLn case version of
    MarloweV1 -> "1"
  let
    contractDoc :: Doc
    contractDoc = indent 4 case version of
      MarloweV1 -> pretty $ V1.marloweContract datum
  putStrLn ""
  putDoc contractDoc
  putStrLn ""
  putStrLn ""

logStep :: MarloweVersion v -> ContractId -> BlockHeader -> ContractStep v -> IO ()
logStep version contractId BlockHeader{..} step = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "transaction "
  case step of
    ApplyTransaction Transaction{transactionId} -> do
      putStrLn $ T.unpack $ encodeBase16 $ unTxId transactionId
    RedeemPayout RedeemStep{..}-> do
      putStr $ T.unpack $ encodeBase16 $ unTxId redeemingTx
      putStrLn " (redeem)"
  setSGR [Reset]
  case step of
    ApplyTransaction Transaction{redeemer, output} -> do
      putStr "ContractId: "
      putStrLn $ T.unpack $ renderContractId contractId
      putStr "SlotNo:     "
      print $ unSlotNo slotNo
      putStr "BlockNo:    "
      print $ unBlockNo blockNo
      putStr "BlockId:    "
      putStrLn $ T.unpack $ encodeBase16 $ unBlockHeaderHash headerHash
      putStr "Inputs:     "
      putStrLn case version of
        MarloweV1 -> show redeemer
      putStrLn ""
      let TransactionOutput{..} = output
      case scriptOutput of
        Nothing -> putStrLn "    <contract closed>"
        Just TransactionScriptOutput{..} -> do
          let
            contractDoc :: Doc
            contractDoc = indent 4 case version of
              MarloweV1 -> pretty $ V1.marloweContract datum
          putDoc contractDoc
      putStrLn ""
      putStrLn ""

    RedeemPayout _ -> error "not implemented"
