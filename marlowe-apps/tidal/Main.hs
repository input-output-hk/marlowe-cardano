-- editorconfig-checker-disable-file

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Data.Time.Units (Second)
import Language.Marlowe.Runtime.App.Channel (RequeueFrequency(RequeueFrequency))
import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Stream (ContractStream(..), TChanEOF)
import Language.Marlowe.Runtime.App.Types
  (Config, FinishOnClose(FinishOnClose), FinishOnWait(FinishOnWait), PollingFrequency(PollingFrequency))
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag(V1))
import Observe.Event (EventBackend)
import Observe.Event.Backend (unitEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector(..))

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map.Strict as M
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Runtime.App.Channel as App
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.History.Api as History
import qualified Options.Applicative as O
import qualified Plutus.V2.Ledger.Api as P
import qualified Sound.Tidal.Safe.Boot as Tidal
import qualified Sound.Tidal.Safe.Context as Tidal


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
  :: EventBackend IO r DynamicEventSelector
  -> Tidal.Stream
  -> [MVar Tidal.ControlPattern]
  -> RequeueFrequency
  -> FinishOnWait
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runStreamer eventBackend tidal pMVars =
  App.watchContracts "StreamerProcess" eventBackend
    $ \_ ->
      \case
        ContractStreamStart{} -> pure ()
        ContractStreamContinued{csContractStep=History.ApplyTransaction Core.Transaction{..}, ..} ->
          let
            Chain.BlockHeader{..} = csBlockHeader
            play :: V1.InputContent -> IO ()
            play (V1.IChoice (V1.ChoiceId pattern (V1.Role role)) i) =
              do
                let
                  message = "Slot " <> show (Chain.unSlotNo slotNo) <> ": d" <> show i <> " + " <> (init . tail . show) role <> " \"" <> (BS8.unpack . P.fromBuiltin) pattern <> "\""
                  f effect =
                    do
                      putStrLn message
                      let
                        d = [Tidal.d1, Tidal.d2, Tidal.d3, Tidal.d4] !! fromInteger (i - 1)
                        pMVar = pMVars !! fromInteger (i - 1)
                      p <- takeMVar pMVar
                      let
                        p' = p Tidal.# effect (fromString $ BS8.unpack $ P.fromBuiltin pattern)
                      putMVar pMVar p'
                      liftIO $ Tidal.exec tidal $ d p'
                case role of
                  "delayfeedback" -> f Tidal.delayfeedback
                  "gain"          -> f Tidal.gain
                  "legato"        -> f Tidal.legato
                  "n"             -> f Tidal.n
                  "note"          -> f Tidal.note
                  "resonance"     -> f Tidal.resonance
                  "room"          -> f Tidal.room
                  "sound"         -> f Tidal.sound
                  "speed"         -> f Tidal.speed
                  "vowel"         -> f Tidal.vowel
                  _               -> pure ()
            play _ = pure ()
            tag =
              M.singleton
                (Core.MarloweMetadataTag "tidal")
                (Just $ Chain.MetadataList [Chain.MetadataMap [(Chain.MetadataText "revision", Chain.MetadataNumber 2)]])
          in
            when (fmap Core.tags (Core.marloweMetadata metadata) == Just tag)
              $ mapM_ (play . V1.getInputContent) inputs
        ContractStreamContinued{csContractStep=History.RedeemPayout History.RedeemStep{}} -> pure ()
        ContractStreamWait{} -> pure ()
        ContractStreamFinish{} -> pure ()
        ContractStreamRolledBack{} -> pure ()


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
    tidal <-
      Tidal.startTidal
        (Tidal.superdirtTarget {Tidal.oLatency = 0.1, Tidal.oAddress = "127.0.0.1", Tidal.oPort = 57120})
        (Tidal.defaultConfig {Tidal.cFrameTimespan = 1/20})
    putStrLn "Start: d1 $ sound \"pluck*4 drum*2 arpy\" # gain \"1.2 1 0.9\""
    putStrLn "Start: d2 $ sound \"pluck*4 drum*2 arpy\" # gain \"1.2 1 0.9\""
    putStrLn "Start: d3 $ sound \"pluck*4 drum*2 arpy\" # gain \"1.2 1 0.9\""
    putStrLn "Start: d4 $ sound \"pluck*4 drum*2 arpy\" # gain \"1.2 1 0.9\""
    pMVars <-
      sequence
        [
          newMVar $ Tidal.sound "pluck*4 drum*2 arpy" Tidal.# Tidal.gain "1.1 1 0.9"
        , newMVar $ Tidal.sound "pluck*4 drum*2 arpy" Tidal.# Tidal.gain "1.1 1 0.9"
        , newMVar $ Tidal.sound "pluck*4 drum*2 arpy" Tidal.# Tidal.gain "1.1 1 0.9"
        , newMVar $ Tidal.sound "pluck*4 drum*2 arpy" Tidal.# Tidal.gain "1.1 1 0.9"
        ]
    runStreamer eventBackend tidal pMVars requeueFrequency' endOnWait detectionChannel discoveryChannel


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
