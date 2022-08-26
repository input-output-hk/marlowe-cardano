{-# LANGUAGE GADTs #-}

module Main where

import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, retry)
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad (forever)
import Data.ByteString.Base16 (encodeBase16)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Pretty (pretty)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), BlockHeaderHash (..), BlockNo (..),
                                               ChainSyncQuery (..), RuntimeChainSeekClient, SlotNo (..), TxId (..),
                                               TxOutRef (..), WithGenesis (..), runtimeChainSeekCodec, toBech32)
import Language.Marlowe.Runtime.Core.Api (ContractId (..), MarloweVersion (..), Transaction (..),
                                          TransactionOutput (..), TransactionScriptOutput (..), parseContractId,
                                          renderContractId)
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.History.Follower
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Query.Client (liftQuery, queryClientPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Socket (AddrInfo (..), HostName, PortNumber, SocketType (..), close, connect, defaultHints, getAddrInfo,
                       openSocket, withSocketsDo)
import Network.TypedProtocol (runPeerWithDriver, startDState)
import Options.Applicative (argument, auto, execParser, fullDesc, header, help, helper, info, long, maybeReader,
                            metavar, option, progDesc, short, strOption, value)
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGR)
import System.IO (hPrint, stderr)
import Text.PrettyPrint.Leijen (Doc, indent, putDoc)

main :: IO ()
main = run =<< getOptions

hints :: AddrInfo
hints = defaultHints { addrSocketType = Stream }

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  chainSeekAddr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
  bracket (open chainSeekAddr) close \chainSeekSocket -> do
      slotConfig <- queryChainSync GetSlotConfig
      securityParameter <- queryChainSync GetSecurityParameter
      let driver = mkDriver throwIO runtimeChainSeekCodec $ socketAsChannel chainSeekSocket
      let
        connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
        connectToChainSeek client = fst <$> runPeerWithDriver driver peer (startDState driver)
          where peer = chainSeekClientPeer Genesis client
      let getMarloweVersion = Core.getMarloweVersion
      Follower{..} <- atomically $ mkFollower FollowerDependencies{..}
      Left result <- race runFollower (logChanges contractId changes)
      case result of
        Left err -> hPrint stderr err
        Right () -> pure ()
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

logChanges :: ContractId -> STM (Maybe SomeContractChanges) -> IO Void
logChanges contractId readChanges = forever do
  SomeContractChanges version ContractChanges{..} <- atomically do
    mchanges <- readChanges
    maybe retry pure mchanges
  for_ rollbackTo \slotNo -> do
    putStrLn $ "Rollback to slot: " <> show slotNo
  void $ Map.traverseWithKey (traverse . logStep version contractId) steps

logStep :: MarloweVersion v -> ContractId -> BlockHeader -> ContractStep v -> IO ()
logStep version contractId BlockHeader{..} step = do
  setSGR [SetColor Foreground Vivid Yellow]
  putStr "transaction "
  case step of
    Create _ -> do
      putStr $ T.unpack $ encodeBase16 $ unTxId $ txId $ unContractId contractId
      putStrLn " (creation)"
    ApplyTransaction Transaction{transactionId} -> do
      putStrLn $ T.unpack $ encodeBase16 $ unTxId transactionId
    RedeemPayout RedeemStep{..}-> do
      putStr $ T.unpack $ encodeBase16 $ unTxId redeemingTx
      putStrLn " (redeem)"
  setSGR [Reset]
  case step of
    Create CreateStep{..} -> do
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

data Options = Options
  { port       :: PortNumber
  , queryPort  :: PortNumber
  , host       :: HostName
  , contractId :: ContractId
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options <$> portParser <*> queryPortParser <*> hostParser <*> contractIdParser

    portParser = option auto $ mconcat
      [ long "port-number"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server. Default value: 3715"
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port-number"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The query port number of the chain seek server. Default value: 3716"
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server. Default value: 127.0.0.1"
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
