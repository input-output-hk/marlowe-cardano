{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (
  main,
) where

import Control.Arrow ((&&&))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (void, when)
import Data.Bifunctor (bimap)
import Data.Function (on)
import Data.List (groupBy)
import Data.Maybe (isNothing)
import Data.String (fromString)
import Data.Time.Units (Second)
import Language.Marlowe.Runtime.App.Channel (RequeueFrequency (RequeueFrequency))
import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Stream (ContractStream (..), TChanEOF)
import Language.Marlowe.Runtime.App.Types (
  Config,
  FinishOnClose (FinishOnClose),
  FinishOnWait (FinishOnWait),
  PollingFrequency (PollingFrequency),
 )
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersionTag (V1))
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Observe.Event (EventBackend)
import Observe.Event.Backend (unitEventBackend)
import Observe.Event.Dynamic (DynamicEventSelector (..))

import qualified Blaze.ByteString.Builder.Char8 as B8 (fromString)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as V1
import qualified Language.Marlowe.Runtime.App.Channel as App
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.History.Api as History
import qualified Options.Applicative as O
import qualified PlutusLedgerApi.V2 as P
import qualified PlutusTx.AssocMap as AM

runDetection
  :: EventBackend IO r DynamicEventSelector
  -> Config
  -> PollingFrequency
  -> TChanEOF ContractId
  -> IO (TChanEOF (ContractStream 'V1))
runDetection eventBackend config pollingFrequency = do
  let finishOnWait = FinishOnWait True
      finishOnClose = FinishOnClose True
  App.runDetection (const True) eventBackend config pollingFrequency finishOnClose finishOnWait

data Verbosity = Terse | Standard | Verbose
  deriving (Eq, Enum, Ord, Read, Show)

runStreamer
  :: Verbosity
  -> Chain.SlotNo
  -> Maybe Core.MarloweMetadataTag
  -> EventBackend IO r DynamicEventSelector
  -> MVar (Maybe A.Value)
  -> RequeueFrequency
  -> FinishOnWait
  -> TChanEOF (ContractStream 'V1)
  -> TChanEOF ContractId
  -> IO ()
runStreamer Terse minSlot requiredTag eventBackend streamMVar =
  App.watchContracts "StreamerProcess" eventBackend $
    \_ ->
      \case
        ContractStreamStart{..} ->
          let Chain.BlockHeader{..} = csBlockHeader
              History.CreateStep{..} = csCreateStep
              Core.TransactionScriptOutput{..} = createOutput
              Chain.TxOutRef{..} = utxo
              V1.MarloweData{..} = datum
              tags = maybe M.empty Core.tags (Core.marloweMetadata metadata)
           in when (slotNo >= minSlot && maybe True (`M.member` tags) requiredTag) $
                putMVar streamMVar . Just $
                  A.object
                    [ "slot" A..= slotNo
                    , "block" A..= blockNo
                    , "contractId" A..= csContractId
                    , "transactionId" A..= txId
                    , "tags" A..= tags
                    , "value" A..= showAssets assets
                    , "state" A..= showState marloweState
                    , "actions" A..= ["create" :: String]
                    ]
        ContractStreamContinued{csContractStep = History.ApplyTransaction Core.Transaction{..}, ..} ->
          let Chain.BlockHeader{..} = csBlockHeader
              Core.TransactionOutput{..} = output
              summarize (V1.IDeposit account party token amount) =
                A.object
                  [ "action" A..= ("deposit" :: String)
                  , "actor" A..= showParty party
                  , "account" A..= showParty account
                  , "token" A..= showToken token
                  , "amount" A..= amount
                  ]
              summarize (V1.IChoice (V1.ChoiceId name party) number) =
                A.object
                  [ "action" A..= ("choose" :: String)
                  , "actor" A..= showParty party
                  , "choice" A..= BS8.unpack (P.fromBuiltin name)
                  , "number" A..= number
                  ]
              summarize V1.INotify =
                A.object
                  [ "action" A..= ("notify" :: String)
                  ]
              tags = maybe M.empty Core.tags (Core.marloweMetadata metadata)
           in when (slotNo >= minSlot && maybe True (`M.member` tags) requiredTag) $
                putMVar streamMVar . Just $
                  A.object
                    [ "slot" A..= slotNo
                    , "block" A..= blockNo
                    , "contractId" A..= csContractId
                    , "transactionId" A..= transactionId
                    , "tags" A..= tags
                    , "value" A..= maybe (A.Array V.empty) (\Core.TransactionScriptOutput{..} -> showAssets assets) scriptOutput
                    , "state" A..= fmap (\Core.TransactionScriptOutput{..} -> showState $ V1.marloweState datum) scriptOutput
                    , "actions" A..= fmap (summarize . V1.getInputContent) inputs
                    ]
        ContractStreamContinued{csContractStep = History.RedeemPayout History.RedeemStep{..}, ..} ->
          let Chain.BlockHeader{..} = csBlockHeader
           in when (slotNo >= minSlot && isNothing requiredTag) $
                putMVar streamMVar . Just $
                  A.object
                    [ "slot" A..= slotNo
                    , "block" A..= blockNo
                    , "contractId" A..= csContractId
                    , "transactionId" A..= redeemingTx
                    , "tags" A..= Core.MarloweMetadata mempty Nothing
                    , "value" A..= A.Array V.empty
                    , "state" A..= (Nothing :: Maybe A.Value)
                    , "actions"
                        A..= [ A.object
                                [ "action" A..= ("redeem" :: String)
                                , "actor" A..= Chain.tokenName datum
                                ]
                             ]
                    ]
        ContractStreamWait{} -> pure ()
        ContractStreamFinish{} -> pure ()
        ContractStreamRolledBack{} -> pure ()
runStreamer Standard minSlot _ eventBackend streamMVar =
  App.watchContracts "StreamerProcess" eventBackend $
    \_ ->
      \case
        ContractStreamStart{..} ->
          let Chain.BlockHeader{..} = csBlockHeader
              History.CreateStep{..} = csCreateStep
              Core.TransactionScriptOutput{..} = createOutput
              Chain.TxOutRef{..} = utxo
              V1.MarloweData{..} = datum
           in when (slotNo >= minSlot) $
                putMVar streamMVar . Just $
                  A.object
                    [ "slot" A..= slotNo
                    , "block" A..= blockNo
                    , "contractId" A..= csContractId
                    , "transactionId" A..= txId
                    , "metadata" A..= Core.transactionMetadata metadata
                    , "value" A..= showAssets assets
                    , "contract" A..= Just marloweContract
                    , "state" A..= Just (showState marloweState)
                    , "actions" A..= ["create" :: String]
                    ]
        ContractStreamContinued{csContractStep = History.ApplyTransaction Core.Transaction{..}, ..} ->
          let Chain.BlockHeader{..} = csBlockHeader
              Core.TransactionOutput{..} = output
              summarize (V1.IDeposit account party token amount) =
                A.object
                  [ "action" A..= ("deposit" :: String)
                  , "actor" A..= showParty party
                  , "account" A..= showParty account
                  , "token" A..= showToken token
                  , "amount" A..= amount
                  ]
              summarize (V1.IChoice (V1.ChoiceId name party) number) =
                A.object
                  [ "action" A..= ("choose" :: String)
                  , "actor" A..= showParty party
                  , "choice" A..= BS8.unpack (P.fromBuiltin name)
                  , "number" A..= number
                  ]
              summarize V1.INotify =
                A.object
                  [ "action" A..= ("notify" :: String)
                  ]
           in when (slotNo >= minSlot) $
                putMVar streamMVar . Just $
                  A.object
                    [ "slot" A..= slotNo
                    , "block" A..= blockNo
                    , "contractId" A..= csContractId
                    , "transactionId" A..= transactionId
                    , "metadata" A..= Core.transactionMetadata metadata
                    , "value" A..= maybe (A.Array V.empty) (\Core.TransactionScriptOutput{..} -> showAssets assets) scriptOutput
                    , "contract" A..= fmap (\Core.TransactionScriptOutput{..} -> V1.marloweContract datum) scriptOutput
                    , "state" A..= fmap (\Core.TransactionScriptOutput{..} -> showState $ V1.marloweState datum) scriptOutput
                    , "actions" A..= fmap (summarize . V1.getInputContent) inputs
                    ]
        ContractStreamContinued{csContractStep = History.RedeemPayout History.RedeemStep{..}, ..} ->
          let Chain.BlockHeader{..} = csBlockHeader
           in when (slotNo >= minSlot) $
                putMVar streamMVar . Just $
                  A.object
                    [ "slot" A..= slotNo
                    , "block" A..= blockNo
                    , "contractId" A..= csContractId
                    , "transactionId" A..= redeemingTx
                    , "metadata" A..= Chain.TransactionMetadata mempty
                    , "value" A..= A.Array V.empty
                    , "contract" A..= (Nothing :: Maybe V1.Contract)
                    , "state" A..= (Nothing :: Maybe A.Value)
                    , "actions"
                        A..= [ A.object
                                [ "action" A..= ("redeem" :: String)
                                , "actor" A..= Chain.tokenName datum
                                ]
                             ]
                    ]
        ContractStreamWait{} -> pure ()
        ContractStreamFinish{} -> pure ()
        ContractStreamRolledBack{} -> pure ()
runStreamer Verbose _ _ eventBackend streamMVar =
  App.watchContracts "StreamerProcess" eventBackend $
    \_ cs ->
      putMVar streamMVar . Just . A.object $
        case cs of
          ContractStreamStart{} -> ["start" A..= cs]
          ContractStreamContinued{} -> ["continued" A..= cs]
          ContractStreamWait{} -> ["wait" A..= cs]
          ContractStreamFinish{} -> ["finish" A..= cs]
          ContractStreamRolledBack{} -> ["rolledback" A..= cs]

showParty
  :: V1.Party
  -> String
showParty (V1.Address network address) = T.unpack $ V1.serialiseAddressBech32 network address
showParty (V1.Role role) = init . tail $ show role

showToken
  :: V1.Token
  -> String
showToken (V1.Token "" "") = ""
showToken (V1.Token c (P.TokenName n)) = show c <> "." <> BS8.unpack (P.fromBuiltin n)

showAssets
  :: Chain.Assets
  -> A.Value
showAssets (Chain.Assets lovelace (Chain.Tokens tokens)) =
  A.Array $
    V.fromList
      [ A.object
        [ "token" A..= t
        , "amount" A..= n
        ]
      | let showAssetId (Chain.AssetId c (Chain.TokenName n)) = (init . tail . show) c <> "." <> BS8.unpack n
      , let ada = ("", toInteger lovelace)
      , let natives = bimap showAssetId toInteger <$> M.toList tokens
      , (t, n) <- ada : natives
      , n > 0
      ]

showChoice
  :: (V1.ChoiceId, Integer)
  -> A.Value
showChoice (V1.ChoiceId name party, number) =
  A.object
    [ "actor" A..= showParty party
    , "choice" A..= BS8.unpack (P.fromBuiltin name)
    , "number" A..= number
    ]

showState
  :: V1.State
  -> A.Value
showState V1.State{..} =
  let accounts' =
        ((fromString . showParty . fst . head) &&& (A.Array . V.fromList . fmap snd))
          <$> groupBy
            ((==) `on` fst)
            [ ( party
              , A.object
                  [ "token" A..= showToken token
                  , "amount" A..= amount
                  ]
              )
            | ((party, token), amount) <- AM.toList accounts
            ]
   in A.object
        [ "accounts" A..= A.object (uncurry (A..=) <$> accounts')
        , "choices" A..= A.Array (V.fromList $ showChoice <$> AM.toList choices)
        , "variables"
            A..= A.object ((\(V1.ValueId k, v) -> fromString (BS8.unpack $ P.fromBuiltin k) A..= v) <$> AM.toList boundValues)
        ]

eventStream
  :: MVar (Maybe A.Value)
  -> IO ServerEvent
eventStream =
  fmap (maybe CloseEvent $ ServerEvent Nothing Nothing . pure . B8.fromString . LBS8.unpack . A.encode)
    . takeMVar

main :: IO ()
main =
  do
    Command{..} <- O.execParser =<< commandParser
    let pollingFrequency' = PollingFrequency pollingFrequency
        requeueFrequency' = RequeueFrequency requeueFrequency
        eventBackend = unitEventBackend
    discoveryChannel <- App.runDiscovery' eventBackend config pollingFrequency' endOnWait
    detectionChannel <- runDetection eventBackend config pollingFrequency' discoveryChannel
    streamMVar <- newEmptyMVar
    void
      . forkIO
      . run port
      . addHeaders [("Access-Control-Allow-Origin", origin)]
      . eventSourceAppIO
      $ eventStream streamMVar
    runStreamer
      verbosity
      minSlot
      requiredTag
      eventBackend
      streamMVar
      requeueFrequency'
      endOnWait
      detectionChannel
      discoveryChannel

data Command = Command
  { config :: Config
  , pollingFrequency :: Second
  , requeueFrequency :: Second
  , endOnWait :: FinishOnWait
  , minSlot :: Chain.SlotNo
  , requiredTag :: Maybe Core.MarloweMetadataTag
  , origin :: BS8.ByteString
  , port :: Int
  , verbosity :: Verbosity
  }
  deriving (Show)

commandParser :: IO (O.ParserInfo Command)
commandParser =
  do
    configParser <- getConfigParser
    let commandOptions =
          Command
            <$> configParser
            <*> fmap
              fromInteger
              ( O.option
                  O.auto
                  ( O.long "polling"
                      <> O.value 5
                      <> O.showDefault
                      <> O.metavar "SECONDS"
                      <> O.help "The polling frequency for waiting on Marlowe Runtime."
                  )
              )
            <*> fmap
              fromInteger
              ( O.option
                  O.auto
                  ( O.long "requeue"
                      <> O.value 20
                      <> O.showDefault
                      <> O.metavar "SECONDS"
                      <> O.help "The requeuing frequency for reviewing the progress of contracts on Marlowe Runtime."
                  )
              )
            <*> O.flag
              (FinishOnWait False)
              (FinishOnWait True)
              (O.long "end-at-tip" <> O.help "Stop the process when the tip of all contracts has been reached.")
            <*> fmap
              fromInteger
              ( O.option
                  O.auto
                  ( O.long "min-slot"
                      <> O.value 0
                      <> O.showDefault
                      <> O.metavar "SLOT"
                      <> O.help "The first slot number to start reporting. Does not apply in verbose mode."
                  )
              )
            <*> (O.optional . fmap Core.MarloweMetadataTag . O.strOption)
              ( O.long "require-tag"
                  <> O.metavar "STRING"
                  <> O.help "Report just for transactions matching the specified Marlowe tag. Only applies to terse mode."
              )
            <*> O.option
              O.auto
              ( O.long "origin"
                  <> O.value "*"
                  <> O.showDefault
                  <> O.metavar "STRING"
                  <> O.help "Value for \"Access-Control-Allow-Origin\""
              )
            <*> O.option
              O.auto
              (O.long "port" <> O.value 1564 <> O.showDefault <> O.metavar "PORT" <> O.help "Port number for streaming data.")
            <*> O.option
              O.auto
              ( O.long "verbosity"
                  <> O.value Terse
                  <> O.showDefault
                  <> O.metavar "Terse|Standard|Verbose"
                  <> O.help "The verbosity of the output data stream."
              )
    pure $
      O.info
        (O.helper {- <*> O.versionOption -} <*> commandOptions)
        ( O.fullDesc
            <> O.progDesc
              "This command-line tool watches the blockchain for Marlowe contracts for Marlowe contracts and streams them as server-sent events (SSE)."
            <> O.header "marlowe-streamer : stream all Marlowe contracts via HTTP SSE"
        )
