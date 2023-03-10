-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Chain-sync client for Cardano node.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}


module Language.Marlowe.CLI.Sync
  ( -- * Handlers
    Idler
  , MarlowePrinter
  , Processor
  , Recorder
  , Reverter
  , TxHandler
    -- * Activity
  , walkBlocks
  , watchChain
  , watchMarlowe
  , watchMarloweWithPrinter
    -- * Points
  , loadPoint
  , savePoint
    -- * Queries
  , isMarloweIn
  , isMarloweOut
  , isMarloweTransaction
    -- * Utils
  , classifyOutputs
  ) where


import Cardano.Api
  ( Block(..)
  , BlockHeader(..)
  , BlockInMode(..)
  , CardanoMode
  , ChainPoint(..)
  , ChainTip
  , CtxTx
  , EraInMode(..)
  , IsCardanoEra
  , LocalChainSyncClient(..)
  , LocalNodeClientProtocols(..)
  , LocalNodeConnectInfo(..)
  , PolicyId
  , ShelleyBasedEra(..)
  , SlotNo(..)
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxId
  , TxIn(..)
  , TxIx(..)
  , TxMetadataInEra(..)
  , TxMetadataJsonSchema(TxMetadataJsonNoSchema)
  , TxMintValue(..)
  , TxOut(..)
  , TxOutDatum(..)
  , TxValidityLowerBound(..)
  , TxValidityUpperBound(..)
  , ValueNestedBundle(..)
  , ValueNestedRep(..)
  , connectToLocalNode
  , getTxBody
  , getTxId
  , metadataToJson
  , txOutValueToValue
  , valueToNestedRep
  )
import Cardano.Api.ChainSync.Client (ChainSyncClient(..), ClientStIdle(..), ClientStIntersect(..), ClientStNext(..))
import Cardano.Api.Shelley (ShelleyLedgerEra, TxBody(ShelleyTxBody), TxBodyScriptData(..), fromAlonzoData, toPlutusData)
import Cardano.Ledger.Alonzo.TxWitness (RdmrPtr(..), Redeemers(..), TxDats(..))
import Codec.CBOR.Encoding (encodeBreak, encodeListLenIndef)
import Codec.CBOR.JSON (encodeValue)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad (guard, when)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Control.Monad.Extra (whenJust)
import Data.Aeson (FromJSON, ToJSON(..), decodeFileStrict, encode, encodeFile)
import Data.Bifunctor (first)
import Data.Default (Default(..))
import Data.IORef (IORef, newIORef, readIORef)
import Data.List (nub)
import Data.List.Extra (mconcatMap)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Language.Marlowe.CLI.Sync.Types
  (MarloweAddress(..), MarloweEvent(..), MarloweIn(..), MarloweOut(..), SavedPoint(..))
import Language.Marlowe.CLI.Transaction (querySlotConfig)
import Language.Marlowe.CLI.Types (CliEnv, CliError(..))
import Language.Marlowe.Client (marloweParams)
import Language.Marlowe.Core.V1.Semantics.Types (Contract(..), Input(..), TimeInterval)
import Language.Marlowe.Scripts (MarloweInput, MarloweTxInput(..), marloweValidatorHash, rolePayoutValidatorHash)
import Ledger.Address (toPlutusAddress)
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, toCardanoScriptHash)
import Plutus.Script.Utils.Scripts (dataHash)
import Plutus.V1.Ledger.Api
  ( BuiltinByteString
  , CurrencySymbol(..)
  , Extended(..)
  , FromData
  , Interval(..)
  , LowerBound(..)
  , MintingPolicyHash(..)
  , TokenName(..)
  , UpperBound(..)
  , dataToBuiltinData
  , fromData
  )
import Plutus.V1.Ledger.Slot (Slot(..))
import Plutus.V1.Ledger.SlotConfig (SlotConfig, slotRangeToPOSIXTimeRange)
import System.Directory (doesFileExist, renameFile)
import System.IO (BufferMode(LineBuffering), Handle, IOMode(WriteMode), hClose, hSetBuffering, openFile, stderr, stdout)

import Cardano.Ledger.Era (Era)
import Control.Monad.Reader (MonadReader)
import qualified Data.Aeson as A (Value)
import qualified Data.ByteArray as BA (length)
import qualified Data.ByteString as BS (hPutStr)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (hPutStrLn)
import qualified Data.Map.Strict as M (elems, filter, null, toList)
import qualified Data.Set as S (singleton, toList)


-- | Record the point on the chain.
type Recorder =  BlockInMode CardanoMode  -- ^ The block.
              -> IO ()                    -- ^ Action to record the point.


-- | Record the point on the chain into a file.
savePoint :: ToJSON a
          => a                 -- ^ The additional information to be recorded.
          -> Maybe FilePath    -- ^ The file in which to record the point, if any.
          -> BlockInMode mode  -- ^ The block.
          -> IO ()             -- ^ Action to record the point in the file.
savePoint memory (Just filename) (BlockInMode (Block (BlockHeader slotNo blockHash _) _) _) =
  do
    let
      filename' = filename ++ ".tmp"
    encodeFile filename'
      $ SavedPoint slotNo blockHash memory
    renameFile filename' filename
savePoint _ Nothing _ = return ()


-- | Set the point on the chain.
loadPoint :: Default a
          => FromJSON a
          => MonadIO m
          => Maybe FilePath     -- ^ The file in which the point is recorded, if any.
          -> m (ChainPoint, a)  -- ^ Action to read the point, if possible.
loadPoint (Just filename) =
  liftIO
    $ do
      exists <- doesFileExist filename
      point <-
        if exists
          then decodeFileStrict filename
          else pure Nothing
      pure
        . fromMaybe (ChainPointAtGenesis, def)
        $ do
          SavedPoint{..} <- point
          pure
            (ChainPoint slotNo blockHash, memory)
loadPoint Nothing = pure (ChainPointAtGenesis, def)


-- | Process a block.
type Processor =  BlockInMode CardanoMode  -- ^ The block.
               -> ChainTip                 -- ^ The chain tip.
               -> IO ()                    -- ^ Action to process activity.


-- | Handle a rollback.
type Reverter =  ChainPoint  -- ^ The new chain point.
              -> ChainTip    -- ^ The chain tip.
              -> IO ()       -- ^ Action to handle the rollback.


-- | Peform action when idle.
type Idler = IO Bool  -- ^ Action that returns whether processing should terminate.


-- | Process activity on the blockchain.
walkBlocks :: MonadIO m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection to the node.
           -> ChainPoint                        -- ^ The starting point.
           -> Recorder                          -- ^ Handle chain points.
           -> Idler                             -- ^ Handle idleness.
           -> Maybe Reverter                    -- ^ Handle rollbacks.
           -> Processor                         -- ^ Handle blocks.
           -> m ()                              -- ^ Action to walk the blockchain.
walkBlocks connection start record notifyIdle revertPoint processBlock =
  let
    protocols =
      LocalNodeClientProtocols
      {
        localChainSyncClient    = LocalChainSyncClient
                                    $ client start record notifyIdle revertPoint processBlock
      , localTxSubmissionClient = Nothing
      , localTxMonitoringClient = Nothing
      , localStateQueryClient   = Nothing
      }
  in
    liftIO
      $ connectToLocalNode connection protocols


-- | Chain synchronization client.
client :: ChainPoint                                                           -- ^ Starting point.
       -> Recorder                                                             -- ^ Handle chain points.
       -> Idler                                                                -- ^ Handle idleness.
       -> Maybe Reverter                                                       -- ^ Handle rollbacks.
       -> Processor                                                            -- ^ Handle blocks.
       -> ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO ()  -- ^ The chain synchronization client.
client start record notifyIdle revertPoint processBlock =
  ChainSyncClient
    $ let
        clientStart =
          return
            .  SendMsgFindIntersect [start]
            $ ClientStIntersect
              {
                recvMsgIntersectFound    = \ _ _ -> ChainSyncClient clientStIdle
              , recvMsgIntersectNotFound = \_    -> ChainSyncClient clientStIdle
              }
        clientStIdle =
          return
            . SendMsgRequestNext clientStNext
            $ do
              terminate <- notifyIdle
              if terminate
                then clientDone
                else return clientStNext
        clientStNext =
          ClientStNext
          {
            recvMsgRollForward  = \block tip -> ChainSyncClient $ record block
                                                                >> processBlock block tip
                                                                >> clientStIdle
          , recvMsgRollBackward = \point tip -> ChainSyncClient $  whenJust revertPoint (\f -> f point tip)
                                                                >> clientStIdle
          }
        clientDone =
          return
            $ ClientStNext
              {
                recvMsgRollForward  = \_ _ -> ChainSyncClient . pure $ SendMsgDone ()
              , recvMsgRollBackward = \_ _ -> ChainSyncClient . pure $ SendMsgDone ()
              }
     in
      clientStart


-- | Process a block.
type BlockHandler =  BlockHeader  -- ^ The block header.
                  -> ChainTip     -- ^ The chain tip.
                  -> IO ()        -- ^ Action to process the block.


-- | Process a transaction's output.
type TxHandler =  forall era
               .  IsCardanoEra era
               => BlockHeader  -- ^ The block header.
               -> [Tx era]     -- ^ The transactions.
               -> IO ()        -- ^ Action to process transactions.


-- | Watch transactions on the blockchain. Note that transaction output is reported *before* spent UTxOs.
watchChain :: MonadIO m
           => LocalNodeConnectInfo CardanoMode  -- ^ The connection to the node.
           -> ChainPoint                        -- ^ The starting point.
           -> Recorder                          -- ^ Handle chain points.
           -> Maybe Reverter                    -- ^ Handle rollbacks.
           -> Idler                             -- ^ Handle idleness.
           -> BlockHandler                      -- ^ Handle blocks.
           -> TxHandler                         -- ^ Handle transactions.
           -> m ()                              -- ^ Action to watch transactions.
watchChain connection start record revertPoint notifyIdle blockHandler txHandler =
  walkBlocks connection start record notifyIdle revertPoint
    $ processChain blockHandler txHandler


-- | Process transactions.
processChain :: BlockHandler             -- ^ Handle blocks.
             -> TxHandler                -- ^ Handle transactions.
             -> BlockInMode CardanoMode  -- ^ The block.
             -> ChainTip                 -- ^ The chain tip.
             -> IO ()                    -- ^ Action to process transactions.
processChain blockHandler txHandler (BlockInMode block BabbageEraInCardanoMode ) = processChain' blockHandler txHandler block
processChain blockHandler txHandler (BlockInMode block AlonzoEraInCardanoMode ) = processChain' blockHandler txHandler block
processChain blockHandler txHandler (BlockInMode block MaryEraInCardanoMode   ) = processChain' blockHandler txHandler block
processChain blockHandler txHandler (BlockInMode block AllegraEraInCardanoMode) = processChain' blockHandler txHandler block
processChain blockHandler txHandler (BlockInMode block ShelleyEraInCardanoMode) = processChain' blockHandler txHandler block
processChain blockHandler txHandler (BlockInMode block ByronEraInCardanoMode  ) = processChain' blockHandler txHandler block


-- | Process transactions in a Cardano era.
processChain' :: IsCardanoEra era
              => BlockHandler  -- ^ Handle blocks.
              -> TxHandler     -- ^ Handle transaction.
              -> Block era     -- ^ The block.
              -> ChainTip      -- ^ The chain tip.
              -> IO ()         -- ^ Action to process transactions.
processChain' blockHandler txHandler (Block header txs) tip =
  do
    blockHandler header tip
    txHandler header txs


-- | Watch for Marlowe transactions.
watchMarlowe :: MonadError CliError m
             => MonadIO m
             => MonadReader (CliEnv era) m
             => LocalNodeConnectInfo CardanoMode  -- ^ The local node connection.
             -> Bool                              -- ^ Include non-Marlowe transactions.
             -> Bool                              -- ^ Whether to output CBOR instead of JSON.
             -> Bool                              -- ^ Whether to continue processing when the tip is reached.
             -> Maybe FilePath                    -- ^ The file to restore the chain point from and save it to.
             -> Maybe FilePath                    -- ^ The output file.
             -> m ()                              -- ^ Action for watching for potential Marlowe transactions.
watchMarlowe connection includeAll cbor continue pointFile outputFile =
  do
    hOut <- liftIO $ maybe (pure stdout) (`openFile` WriteMode) outputFile
    liftIO $ hSetBuffering hOut LineBuffering
    liftIO $ hSetBuffering stderr LineBuffering
    printer <-
      if cbor
        then do
               liftIO . BS.hPutStr hOut $ toStrictByteString encodeListLenIndef
               pure $ printCBOR hOut
        else pure $ printJSON hOut
    watchMarloweWithPrinter connection includeAll continue pointFile printer
    when cbor
      . liftIO . BS.hPutStr hOut $ toStrictByteString encodeBreak
    when (isJust outputFile)
      . liftIO
      $ hClose hOut


-- | A printer for Marlowe events.
type MarlowePrinter = MarloweEvent -> IO ()


-- | Print JSON to a file handle.
printJSON :: ToJSON a
          => Handle  -- ^ The handle.
          -> a       -- ^ The JSON item.
          -> IO ()   -- ^ Action to print the JSON.
printJSON hOut =
  LBS8.hPutStrLn hOut
    . encode


-- | Print CBOR to a file handle.
printCBOR :: ToJSON a
          => Handle  -- ^ The handle.
          -> a       -- ^ The JSON item.
          -> IO ()   -- ^ Action to print the CBOR.
printCBOR hOut =
  BS.hPutStr hOut
    . toStrictByteString
    . encodeValue
    . toJSON


-- | Watch for Marlowe transactions.
watchMarloweWithPrinter :: MonadError CliError m
                        => MonadIO m
                        => MonadReader (CliEnv era) m
                        => LocalNodeConnectInfo CardanoMode  -- ^ The local node connection.
                        -> Bool                              -- ^ Include non-Marlowe transactions.
                        -> Bool                              -- ^ Whether to continue processing when the tip is reached.
                        -> Maybe FilePath                    -- ^ The file to restore the chain point from and save it to.
                        -> MarlowePrinter                    -- ^ The printer for Marlowe events.
                        -> m ()                              -- ^ Action for watching for potential Marlowe transactions.
watchMarloweWithPrinter connection includeAll continue pointFile printer =
  do
    slotConfig <- querySlotConfig connection
    (start, state) <- loadPoint pointFile
    stateRef <- liftIO $ newIORef state
    let
      roller rollbackPoint rollbackTip = printer Rollback{..}
      continuer = pure $ not continue
      blocker meBlock _ = printer NewBlock{..}
      saver block =
        do
          state' <- readIORef stateRef
          savePoint state' pointFile block
    watchChain connection start saver (Just roller) continuer blocker
      $ mapM_ . extractMarlowe stateRef printer slotConfig includeAll


-- | Extract potential Marlowe transactions from a block.
extractMarlowe :: IORef ()                 -- ^ State information (unused).
               -> (MarloweEvent -> IO ())  -- ^ The outputter for Marlowe events.
               -> SlotConfig               -- ^ The slot configuration.
               -> Bool                     -- ^ Include non-Marlowe transactions.
               -> BlockHeader              -- ^ The block's header.
               -> Tx era                   -- ^ The transaction.
               -> IO ()                    -- ^ Action to output potential Marlowe transactions.
extractMarlowe _ printer slotConfig includeAll meBlock tx =
  mapM_ printer
    $ classifyMarlowe slotConfig includeAll meBlock (getTxBody tx)


-- | Classify a transaction's Marlowe content.
classifyMarlowe :: SlotConfig      -- ^ The slot configuration.
                -> Bool            -- ^ Include non-Marlowe transactions.
                -> BlockHeader     -- ^ The block's header.
                -> TxBody era          -- ^ The transaction.
                -> [MarloweEvent]  -- ^ Any Marlowe events in the transaction.
classifyMarlowe slotConfig includeAll meBlock txBody =
  let
    TxBody TxBodyContent{..} = txBody
    meTxId = getTxId txBody
    meMetadata = extractMetadata txMetadata
    parameters = makeParameters meBlock meTxId meMetadata <$> extractMints txMintValue
    meIns = classifyInputs txBody
    meInterval = convertSlots slotConfig txValidityRange
    meOuts = classifyOutputs meTxId txOuts
    event = if includeAll || any isMarloweIn meIns || any isMarloweOut meOuts
              then [Transaction{..}]
              else mempty
  in
    parameters <> event


-- | Convert a slot range to a POSIX time range.
convertSlots :: SlotConfig                                            -- ^ The slot configuration.
             -> (TxValidityLowerBound era, TxValidityUpperBound era)  -- ^ The slot range.
             -> Maybe TimeInterval                                    -- ^ The time range, if it was a closed interval.
convertSlots slotConfig (TxValidityLowerBound _ (SlotNo s0), TxValidityUpperBound _ (SlotNo s1)) =
  let
    interval =
      slotRangeToPOSIXTimeRange slotConfig
        $ Interval (LowerBound (Finite . Slot $ toInteger s0) True) (UpperBound (Finite . Slot $ toInteger s1) False)
  in
    case interval of
      Interval (LowerBound (Finite t0) _) (UpperBound (Finite t1) _) -> Just (t0, t1)
      _                                                              -> Nothing
convertSlots _ _ = Nothing


-- | Does an event contain Marlowe content.
isMarloweTransaction :: MarloweEvent  -- ^ The blockchain event.
                     -> Bool          -- ^ Whether the event is a transaction with Marlowe inputs or outputs.
isMarloweTransaction Transaction{..} = any isMarloweIn meIns || any isMarloweOut meOuts
isMarloweTransaction _               = False


-- | Does a transaction input contain Marlowe information?
isMarloweIn :: MarloweIn  -- ^ The transaction input.
            -> Bool       -- ^ Whether there is Marlowe content.
isMarloweIn PlainIn{} = False
isMarloweIn _         = True


-- | Classify transaction inputs' Marlowe content.
classifyInputs :: TxBody era   -- ^ The transaction body.
               -> [MarloweIn]  -- ^ The classified transaction inputs.
classifyInputs txBody@(TxBody TxBodyContent{..}) =
  let
    continuations = inDatums txBody
    continuationRedeemers = inRedeemers txBody
    lookupContinuation i = classifyInput continuations =<< lookup i continuationRedeemers
    payouts = inDatums txBody
    payoutRedeemers = inRedeemers txBody
    lookupRedemption i =
      do
        () <- lookup i payoutRedeemers
        case nub payouts of
          -- FIXME: Also handle the situation where there are multiple datums.
          [(_, name)] -> do
                           guard
                             $ BA.length name <= 32
                           -- FIXME: Also check for presence of role token.
                           pure
                             $ TokenName name
          _           -> Nothing
  in
    [
      case (lookupContinuation i, lookupRedemption i) of
        (Just miInputs, _) -> ApplicationIn{..}
        (_, Just miPayout) -> PayoutIn{..}
        _                  -> PlainIn{..}
    |
      (i, (miTxIn, _)) <- zip [0..] txIns
    ]


-- | Classify a transaction input's Marlowe content.
classifyInput :: [(BuiltinByteString, Contract)]  -- ^ Contract continuations and their hashes.
              -> MarloweInput                     -- ^ The transaction input.
              -> Maybe [Input]                    -- ^ The the Marlowe input, if any.
classifyInput continuations =
  mapM $ unmerkleize continuations


-- | Restore a contract from its merkleization.
unmerkleize :: [(BuiltinByteString, Contract)]  -- ^ Contract continuations and their hashes.
            -> MarloweTxInput                   -- ^ The Marlowe transaction input.
            -> Maybe Input                      -- ^ Marlowe input, if any.
unmerkleize _ (Input content) =
  pure $ NormalInput content
unmerkleize continuations (MerkleizedTxInput content continuationHash) =
  do
    continuation <- continuationHash `lookup` continuations
    pure $ MerkleizedInput content continuationHash continuation


-- | Does a transaction output contain Marlowe information?
isMarloweOut :: MarloweOut  -- ^ The transaction output.
             -> Bool        -- ^ Whether there is Marlowe content.
isMarloweOut PlainOut{} = False
isMarloweOut _          = True


-- | Classify transaction outputs' Marlowe content.
classifyOutputs :: TxId               -- ^ The transaction ID.
                -> [TxOut CtxTx era]  -- ^ The transaction outputs.
                -> [MarloweOut]       -- ^ The classified transaction outputs.
classifyOutputs txId txOuts =
  uncurry classifyOutput
    . first (TxIn txId . TxIx)
    <$> zip [0..] txOuts


-- | Classify a transaction output's Marlowe content.
classifyOutput :: TxIn             -- ^ The transaction output ID.
               -> TxOut CtxTx era  -- ^ The transaction output content.
               -> MarloweOut       -- ^ The classified transaction output.
classifyOutput moTxIn (TxOut address value datum _) = case datum of
    TxOutDatumInTx _ datum' -> case (fromData $ toPlutusData datum', fromData $ toPlutusData datum') of
                              (Just moOutput, _) -> ApplicationOut{..}
                              (_, Just name)     -> if BA.length name <= 32
                                                      then let moPayout = TokenName name in PayoutOut{..}
                                                      else PlainOut{..}
                              _                  -> PlainOut{..}
    _                   -> PlainOut{..}
  where
    moAddress = toPlutusAddress address
    moValue = txOutValueToValue value

-- | Extract metadata from a transaction.
extractMetadata :: TxMetadataInEra era  -- ^ The potential metadata.
                -> Maybe A.Value        -- ^ The JSON metadata, if any.
extractMetadata (TxMetadataInEra _ metadata) = Just $ metadataToJson TxMetadataJsonNoSchema metadata
extractMetadata _                            = Nothing


-- | Lift into a report of an anomaly.
liftAnomaly :: Show e
            => BlockHeader            -- ^ The block's header.
            -> TxId                   -- ^ The transaction ID.
            -> Either e a             -- ^ The potential anomaly.
            -> Either MarloweEvent a  -- ^ The lifted anomaly.
liftAnomaly _ _ (Right x)           = Right x
liftAnomaly meBlock meTxId (Left e) = let meAnomaly = show e in Left Anomaly{..}


-- | Handle anomalies.
runAnomaly :: Either MarloweEvent MarloweEvent  -- ^ The potential anomaly.
           -> MarloweEvent                      -- ^ The Marlowe event.
runAnomaly = either id id


-- | Make Marlowe parameters for a currency value.
makeParameters :: BlockHeader    -- ^ The block's header.
               -> TxId           -- ^ The transaction ID.
               -> Maybe A.Value  -- ^ The metadata, if any.
               -> PolicyId       -- ^ The policy ID.
               -> MarloweEvent   -- ^ The Marlowe event.
makeParameters meBlock meTxId meMetadata policy =
  runAnomaly
    $ do
      let
        anomaly = liftAnomaly meBlock meTxId
        MintingPolicyHash currencyHash = fromCardanoPolicyId policy
        meParams = marloweParams $ CurrencySymbol currencyHash

      meApplicationAddress <-  anomaly $ ApplicationCredential <$> toCardanoScriptHash marloweValidatorHash
      mePayoutAddress <- anomaly $ PayoutCredential <$> toCardanoScriptHash rolePayoutValidatorHash
      pure Parameters{..}


-- | Extract policy IDs from minting.
extractMints :: TxMintValue build era  -- ^ The minted value.
             -> [PolicyId]             -- ^ The policy IDs.
extractMints TxMintNone = mempty
extractMints (TxMintValue _ value _) =
  let
    extractPolicy (ValueNestedBundle policy quantities) = if M.null $ M.filter (> 0) quantities
                                                            then mempty
                                                            else S.singleton policy
    extractPolicy _                                     = mempty
    ValueNestedRep reps = valueToNestedRep value
  in
    S.toList
      $ mconcatMap extractPolicy reps


-- | Find the datums input to scripts in a transaction.
inDatums :: forall a era
          . FromData a
         => TxBody era                -- ^ The transaction.
         -> [(BuiltinByteString, a)]  -- ^ The datums.
inDatums = \case
  ShelleyTxBody ShelleyBasedEraAlonzo _ _ scriptData _ _  -> inDatums' scriptData
  ShelleyTxBody ShelleyBasedEraBabbage _ _ scriptData _ _ -> inDatums' scriptData
  _                                                       -> mempty
  where
    inDatums' :: Era (ShelleyLedgerEra era) => TxBodyScriptData era -> [(BuiltinByteString, a)]
    inDatums' (TxBodyScriptData _ (TxDats datumMap) _) = catMaybes
      [
        (dataHash $ dataToBuiltinData dat, ) <$> fromData dat
      |
        dat <- toPlutusData . fromAlonzoData <$> M.elems datumMap
      ]
    inDatums' _ = mempty


-- | Find the redeemers input to scripts in a transaction.
inRedeemers :: forall era a
             . FromData a
            => TxBody era  -- ^ The transaction.
            -> [(Int, a)]  -- ^ The redeemers for each input.
inRedeemers = \case
  ShelleyTxBody ShelleyBasedEraAlonzo _ _ scriptData _ _  -> inRedeemers' scriptData
  ShelleyTxBody ShelleyBasedEraBabbage _ _ scriptData _ _ -> inRedeemers' scriptData
  _                                                       -> mempty
  where
    inRedeemers' :: Era (ShelleyLedgerEra era) => TxBodyScriptData era -> [(Int, a)]
    inRedeemers' = \case
      TxBodyScriptData _ _ (Redeemers redeemerMap) -> catMaybes
        [
          (fromEnum i, ) <$> fromData (toPlutusData $ fromAlonzoData dat)
        |
          (RdmrPtr _ i, (dat, _)) <- M.toList redeemerMap
        ]
      _                                            -> []
