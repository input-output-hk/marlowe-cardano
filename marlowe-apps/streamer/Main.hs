{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Blaze.ByteString.Builder.Char8 (fromString)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.Time.Units (Second)
import Language.Marlowe.Runtime.App.Channel (RequeueFrequency(RequeueFrequency))
import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), TChanEOF)
import Language.Marlowe.Runtime.App.Types
  (Config, FinishOnClose(FinishOnClose), FinishOnWait(FinishOnWait), PollingFrequency(PollingFrequency))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Observe.Event (EventBackend)
import Observe.Event.Backend (unitEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector(..))

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Text as T
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import qualified Language.Marlowe.Runtime.App.Channel as App
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.History.Api as History
import qualified Options.Applicative as O
import qualified Plutus.V2.Ledger.Api as P


runDetection
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection eventBackend config pollingFrequency = do
  let
    finishOnWait = FinishOnWait True
    finishOnClose = FinishOnClose True
  App.runDetection (const True) eventBackend config pollingFrequency finishOnClose finishOnWait


data Verbosity = Terse | Standard | Verbose
  deriving (Eq, Enum, Ord, Read, Show)


runStreamer
  :: Verbosity
  -> EventBackend IO r DynamicEventSelector
  -> MVar (Maybe A.Value)
  -> RequeueFrequency
  -> FinishOnWait
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runStreamer Terse eventBackend streamMVar =
  App.watchContracts "StreamerProcess" eventBackend
    $ \_ ->
      \case
        ContractStreamStart{..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
            History.CreateStep{..} = csCreateStep
            Core.TransactionScriptOutput{..} = createOutput
            Chain.TxOutRef{..} = utxo
          in
            putMVar streamMVar . Just $ A.object
              [
                "slot"          A..= slotNo
              , "block"         A..= blockNo
              , "contractId"    A..= csContractId
              , "transactionId" A..= txId
              , "value"         A..= Just assets
              , "actions"       A..= ["create" :: String]
              ]
        ContractStreamContinued{csContractStep=History.ApplyTransaction Core.Transaction{..}, ..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
            Core.TransactionOutput{..} = output
            showToken (V1.Token "" "") = ""
            showToken (V1.Token c (P.TokenName n)) = show c <> "." <> BS8.unpack (P.fromBuiltin n)
            showParty (V1.Address network address) = T.unpack $ V1.serialiseAddressBech32 network address
            showParty (V1.Role role) = show role
            summarize (V1.IDeposit account party token amount) =
              A.object
                [
                  "action"  A..= ("deposit" :: String)
                , "actor"   A..= showParty party
                , "account" A..= showParty account
                , "token"   A..= showToken token
                , "amount"  A..= amount
                ]
            summarize (V1.IChoice (V1.ChoiceId name party) number) =
              A.object
                [
                  "action" A..= ("choose" :: String)
                , "actor"  A..= showParty party
                , "choice" A..= BS8.unpack (P.fromBuiltin name)
                , "number" A..= number
                ]
            summarize V1.INotify =
              A.object
                [
                  "action" A..= ("notify" :: String)
                ]
          in
            putMVar streamMVar . Just $ A.object
              [
                "slot"          A..= slotNo
              , "block"         A..= blockNo
              , "contractId"    A..= csContractId
              , "transactionId" A..= transactionId
              , "value"         A..= fmap (\Core.TransactionScriptOutput{..} -> assets) scriptOutput
              , "actions"       A..= fmap (summarize . V1.getInputContent) inputs
              ]
        ContractStreamContinued{csContractStep=History.RedeemPayout History.RedeemStep{..}, ..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
          in
            putMVar streamMVar . Just $ A.object
              [
                "slot"          A..= slotNo
              , "block"         A..= blockNo
              , "contractId"    A..= csContractId
              , "transactionId" A..= redeemingTx
              , "value"         A..= (Nothing :: Maybe Chain.Assets)
              , "actor"         A..= Chain.tokenName datum
              , "actions"       A..= ["redeem" :: String]
              ]
        _ -> pure ()
runStreamer Standard eventBackend streamMVar =
  App.watchContracts "StreamerProcess" eventBackend
    $ \_ ->
      \case
        ContractStreamStart{..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
            History.CreateStep{..} = csCreateStep
            Core.TransactionScriptOutput{..} = createOutput
            Chain.TxOutRef{..} = utxo
          in
            putMVar streamMVar . Just $ A.object
              [
                "slot"          A..= slotNo
              , "block"         A..= blockNo
              , "contractId"    A..= csContractId
              , "transactionId" A..= txId
              , "value"         A..= Just assets
              , "actions"       A..= ["create" :: String]
              ]
        ContractStreamContinued{csContractStep=History.ApplyTransaction Core.Transaction{..}, ..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
            Core.TransactionOutput{..} = output
          in
            putMVar streamMVar . Just $ A.object
              [
                "slot"          A..= slotNo
              , "block"         A..= blockNo
              , "contractId"    A..= csContractId
              , "transactionId" A..= transactionId
              , "value"         A..= fmap (\Core.TransactionScriptOutput{..} -> assets) scriptOutput
              , "actions"       A..= fmap V1.getInputContent inputs
              ]
        ContractStreamContinued{csContractStep=History.RedeemPayout History.RedeemStep{..}, ..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
          in
            putMVar streamMVar . Just $ A.object
              [
                "slot"          A..= slotNo
              , "block"         A..= blockNo
              , "contractId"    A..= csContractId
              , "transactionId" A..= redeemingTx
              , "value"         A..= (Nothing :: Maybe Chain.Lovelace)
              , "actions"       A..= ["redeem by " <> BS8.unpack (Chain.unTokenName $ Chain.tokenName datum)]
              ]
        _ -> pure ()
runStreamer Verbose eventBackend streamMVar =
  App.watchContracts "StreamerProcess" eventBackend
    $ \_ -> putMVar streamMVar . Just . A.toJSON


eventStream
  :: MVar (Maybe A.Value)
  -> IO ServerEvent
eventStream =
  fmap (maybe CloseEvent $ ServerEvent Nothing Nothing . pure . fromString . LBS8.unpack . A.encode)
    . takeMVar


main :: IO ()
main =
  do
    Command{..} <- O.execParser =<< commandParser
    let
      pollingFrequency' = PollingFrequency pollingFrequency
      requeueFrequency' = RequeueFrequency requeueFrequency
      eventBackend = unitEventBackend
    discoveryChannel <- App.runDiscovery' eventBackend config pollingFrequency' endOnWait
    detectionChannel <- runDetection eventBackend config pollingFrequency' discoveryChannel
    streamMVar <- newEmptyMVar
    void . forkIO
      . run port
      . addHeaders [("Access-Control-Allow-Origin", origin)]
      . eventSourceAppIO
      $ eventStream streamMVar
    runStreamer verbosity eventBackend streamMVar requeueFrequency' endOnWait detectionChannel discoveryChannel


data Command =
  Command
  {
    config :: Config
  , pollingFrequency :: Second
  , requeueFrequency :: Second
  , endOnWait :: FinishOnWait
  , origin :: BS8.ByteString
  , port :: Int
  , verbosity :: Verbosity
  }
    deriving (Show)


commandParser :: IO (O.ParserInfo Command)
commandParser =
  do
    configParser <- getConfigParser
    let
      commandOptions =
        Command
          <$> configParser
          <*> fmap fromInteger (O.option O.auto (O.long "polling" <> O.value 5 <> O.showDefault <> O.metavar "SECONDS" <> O.help "The polling frequency for waiting on Marlowe Runtime."))
          <*> fmap fromInteger (O.option O.auto (O.long "requeue" <> O.value 20 <> O.showDefault <> O.metavar "SECONDS" <> O.help "The requeuing frequency for reviewing the progress of contracts on Marlowe Runtime."))
          <*> O.flag (FinishOnWait False) (FinishOnWait True) (O.long "end-at-tip" <> O.help "Stop the process when the tip of all contracts has been reached.")
          <*> O.option O.auto (O.long "origin" <> O.value "0.0.0.0" <> O.showDefault <> O.metavar "HOST" <> O.help "Value for Access-Control-Allow-Origin")
          <*> O.option O.auto (O.long "port" <> O.value 1564 <> O.showDefault <> O.metavar "PORT" <> O.help "Port number for streaming data.")
          <*> O.option O.auto (O.long "verbosity" <> O.value Terse <> O.showDefault <> O.metavar "Terse|Standard|Verbose" <> O.help "The verbosity of the output data stream.")
    pure
      $ O.info
        (O.helper {- <*> O.versionOption -} <*> commandOptions)
        (
          O.fullDesc
            <> O.progDesc "This command-line tool watches the blockchain for Marlowe contracts for Marlowe contracts and streams them as server-sent events (SSE)."
            <> O.header "marlowe-streamer : stream all Marlowe contracts via HTTP SSE"
        )
