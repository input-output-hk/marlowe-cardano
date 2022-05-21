{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Marlowe.Runtime.Chain where

import Cardano.Api (AddressInEra (..), Block (..), BlockHeader (..), BlockInMode (..), BlockNo (..), CardanoMode,
                    ChainPoint (..), ChainSyncClient (..), ChainTip (..), EraInMode (..), GenesisParameters (..),
                    LocalChainSyncClient (..), LocalNodeClientProtocols (..), LocalNodeConnectInfo, QueryInEra (..),
                    QueryInMode (..), QueryInShelleyBasedEra (..), ShelleyBasedEra (..), SlotNo (..), Tx, TxBody (..),
                    TxBodyContent (..), TxIn (..), TxIx (..), TxMetadataInEra (..), TxMetadataJsonSchema (..),
                    TxMintValue (..), TxOut (..), TxOutDatum (..), TxValidityLowerBound (..), TxValidityUpperBound (..),
                    ValueNestedBundle (..), ValueNestedRep (..), connectToLocalNode, getTxBody, getTxId, metadataToJson,
                    queryNodeLocalState, toAddressAny, txOutValueToValue, valueToNestedRep)
import Cardano.Api.ChainSync.Client (ClientStIdle (..), ClientStIntersect (..), ClientStNext (..))
import Cardano.Api.Shelley (Hash (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (addUTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Language.Marlowe.Runtime.Chain.Types (MarloweAddress (MarloweAddress), MarloweBlockHeader (..),
                                             MarloweBlockHeaderHash (..), MarloweBlockNo (..), MarloweChainEvent (..),
                                             MarloweChainPoint (MarloweChainPoint, MarloweChainPointAtGenesis),
                                             MarloweChainSyncClient,
                                             MarloweChainTip (MarloweChainTip, MarloweChainTipAtGenesis),
                                             MarlowePolicyId (..), MarloweSlotNo (..), MarloweTx (..), MarloweTxId (..),
                                             MarloweTxIn (..), MarloweTxOut (..), MarloweValidityInterval (..))
import Unlift (MonadUnlift, Unlift (runUnlift), askUnlift)

runMarloweChainSyncClient
  :: MonadIO m
  => LocalNodeConnectInfo CardanoMode
  -> (GenesisParameters -> m (MarloweChainSyncClient IO ()))
  -> m (Either String (m ()))
runMarloweChainSyncClient connectionInfo getClient = do
  mGenesisParams <- liftIO $ queryNodeLocalState connectionInfo Nothing
    $ QueryInEra AlonzoEraInCardanoMode
    $ QueryInShelleyBasedEra  ShelleyBasedEraAlonzo QueryGenesisParameters
  case mGenesisParams of
    Left err -> pure $ Left $ show err
    Right (Left err) -> pure $ Left $ show err
    Right (Right genesisParams) -> do
      client <- getClient genesisParams
      let
        protocols = LocalNodeClientProtocols
          { localChainSyncClient = LocalChainSyncClient client
          , localTxSubmissionClient = Nothing
          , localStateQueryClient = Nothing
          }
      pure $ Right $ liftIO $ connectToLocalNode connectionInfo protocols

stdOutMarloweChainSyncClient
  :: [ChainPoint]
  -> GenesisParameters
  -> IO (MarloweChainSyncClient IO ())
stdOutMarloweChainSyncClient startingPoints genesisParams = marloweChainSyncClient
  startingPoints
  genesisParams
  onStart
  (pure True)
  onEvent
  (putStrLn "Done")
  where
    onStart startedAt tip = do
      putStrLn "Start"
      putStrLn $ "  Following chain from: " <> show startedAt
      putStrLn $ "  Current tip: " <> show tip
    onEvent (MarloweRollForward (MarloweBlockHeader slot blockHash block) txs tip) = do
      putStrLn "Roll forward"
      putStrLn $ "  SlotNo: " <> show slot
      putStrLn $ "  BlockHash: " <> show blockHash
      putStrLn $ "  BlockNo: " <> show block
      putStrLn $ "  Tip: " <> show tip
      for_ (zip [0..] txs) \(i :: Integer, MarloweTx{..}) -> do
        putStrLn $ "  Transaction[" <> show i <> "]: "
        putStrLn $ "    Id: " <> show marloweTx_id
        putStrLn $ "    Policies: " <> show marloweTx_policies
        putStrLn $ "    Interval: " <> show marloweTx_interval
        putStrLn $ "    Metadata: " <> show marloweTx_metadata
        putStrLn $ "    Inputs: " <> show marloweTx_inputs
        for_ (zip [0..] marloweTx_outputs) \(j :: Integer, MarloweTxOut{..}) -> do
          putStrLn $ "    Output[" <> show j <> "]: "
          putStrLn $ "      TxIn: " <> show marloweTxOut_txIn
          putStrLn $ "      Address: " <> show marloweTxOut_address
          putStrLn $ "      Value: " <> show marloweTxOut_value
          putStrLn $ "      Datum: " <> show marloweTxOut_datum
    onEvent (MarloweRollBackward point tip) = do
      putStrLn "Roll backward"
      putStrLn $ "  ToPoint: " <> show point
      putStrLn $ "  Tip: " <> show tip

marloweChainSyncClient
  :: forall b m a
   . MonadUnlift b m
  => [ChainPoint] -- ^ Known starting points
  -> GenesisParameters -- ^ GenesisParameters for the network
  -> (Maybe MarloweChainPoint -> MarloweChainTip -> m ()) -- ^ onStart callback. Receives the starting point and the tip.
  -> m Bool -- ^ onIdle callback. Allows the caller to terminate the client by returning True.
  -> (MarloweChainEvent -> m ()) -- ^ onEvent callback. Receives the new block and the tip
  -> m a -- ^ onDone callback. Allows the caller to yeild a result for the client.
  -> m (MarloweChainSyncClient b a)
marloweChainSyncClient startingPoints genesisParams onStart onIdle onEvent =
  mkChainSyncClient
    startingPoints
    (\mpoint tip -> onStart (extractChainPoint <$> mpoint) (extractChainTip tip))
    onIdle
    onRollForward
    onRollBackward
  where
    onRollForward block tip = onEvent $ handleRollForward genesisParams block $ extractChainTip tip
    onRollBackward point = onEvent . MarloweRollBackward (extractChainPoint point) . extractChainTip

extractChainPoint :: ChainPoint -> MarloweChainPoint
extractChainPoint ChainPointAtGenesis = MarloweChainPointAtGenesis
extractChainPoint (ChainPoint (SlotNo slot) (HeaderHash hash)) =
  MarloweChainPoint (MarloweSlotNo slot) (MarloweBlockHeaderHash hash)

extractChainTip :: ChainTip -> MarloweChainTip
extractChainTip ChainTipAtGenesis = MarloweChainTipAtGenesis
extractChainTip (ChainTip (SlotNo slot) (HeaderHash hash) (BlockNo block)) =
  MarloweChainTip (MarloweSlotNo slot) (MarloweBlockHeaderHash hash) (MarloweBlockNo block)

handleRollForward :: GenesisParameters -> BlockInMode CardanoMode -> MarloweChainTip  -> MarloweChainEvent
handleRollForward genesisParams (BlockInMode (Block header txs) _) =
  MarloweRollForward (extractHeader header) (extractMarloweTx <$> txs)
  where
    extractHeader (BlockHeader (SlotNo slot) (HeaderHash hash) (BlockNo block)) =
      MarloweBlockHeader
        (MarloweSlotNo slot)
        (MarloweBlockHeaderHash hash)
        (MarloweBlockNo block)

    extractMarloweTx :: forall era. Tx era -> MarloweTx
    extractMarloweTx tx =
      let
        txBody@(TxBody TxBodyContent {..}) = getTxBody tx
        txId = getTxId txBody
      in
        MarloweTx
          (MarloweTxId txId)
          (extractMints txMintValue)
          (extractInterval txValidityRange)
          (extractMetadata txMetadata)
          (extractTxIn . fst <$> txIns)
          (uncurry (extractMarloweTxOut txId) <$> zip [0..] txOuts)

    extractTxIn (TxIn tid ix) = MarloweTxIn (MarloweTxId tid) ix

    extractMints TxMintNone = []
    extractMints (TxMintValue _ value _) =
      let
        extractPolicy (ValueNestedBundle policy quantities)
          | Map.null $ Map.filter (> 0) quantities = mempty
          | otherwise = Set.singleton $ MarlowePolicyId policy
        extractPolicy _ = mempty
        ValueNestedRep reps = valueToNestedRep value
      in
        Set.toList $ foldMap extractPolicy reps

    extractInterval :: forall era. (TxValidityLowerBound era, TxValidityUpperBound era) -> Maybe MarloweValidityInterval
    extractInterval (TxValidityLowerBound _ (SlotNo s0), TxValidityUpperBound _ (SlotNo s1)) =
      let
        slotLength = protocolParamSlotLength genesisParams
        lowerOffset = secondsToNominalDiffTime $ fromIntegral s0 * nominalDiffTimeToSeconds slotLength
        upperOffset = secondsToNominalDiffTime $ fromIntegral (s1 + 1) * nominalDiffTimeToSeconds slotLength
        zeroTime = protocolParamSystemStart genesisParams
        intervalStart = addUTCTime lowerOffset zeroTime
        intervalDuration = upperOffset - lowerOffset
      in
        Just $ MarloweValidityInterval intervalStart intervalDuration
    extractInterval _ = Nothing

    extractMetadata (TxMetadataInEra _ metadata) = Just $ metadataToJson TxMetadataJsonNoSchema  metadata
    extractMetadata _                            = Nothing

    extractMarloweTxOut txId ix (TxOut (AddressInEra _ address) value datum) =
      MarloweTxOut
        (MarloweTxIn (MarloweTxId txId) (TxIx ix))
        (MarloweAddress $ toAddressAny address)
        (txOutValueToValue value)
        case datum of
          TxOutDatum _ datum' -> Just datum'
          _                   -> Nothing

mkChainSyncClient
  :: MonadUnlift b m
  => [ChainPoint] -- ^ Known starting points
  -> (Maybe ChainPoint -> ChainTip -> m ()) -- ^ onStart callback. Receives the starting point and the tip.
  -> m Bool -- ^ onIdle callback. Allows the caller to terminate the client by returning True.
  -> (BlockInMode CardanoMode  -> ChainTip -> m ()) -- ^ onRollForward callback. Receives the new block and the tip
  -> (ChainPoint -> ChainTip -> m ()) -- ^ onRollBackward callback. Receives the point to rollback to and the tip
  -> m a -- ^ onDone callback. Allows the caller to yeild a result for the client.
  -> m (MarloweChainSyncClient b a)
mkChainSyncClient startingPoints onStart onIdle onRollForward onRollBackward onDone = do
  u <- askUnlift
  pure $ ChainSyncClient $ pure $ start u
  where
    start u = SendMsgFindIntersect startingPoints $ ClientStIntersect
        { recvMsgIntersectFound = \point tip -> ChainSyncClient $ runUnlift u do
          onStart (Just point) tip
          pure $ idle u
        , recvMsgIntersectNotFound = \tip -> ChainSyncClient $ runUnlift u do
          onStart Nothing tip
          pure $ idle u
        }
    idle u = SendMsgRequestNext (next u) $ runUnlift u do
      terminate <- onIdle
      pure if terminate then done u else next u
    next u = ClientStNext
      { recvMsgRollForward = \block tip -> ChainSyncClient $ runUnlift u $  do
          onRollForward block tip
          pure $ idle u
      , recvMsgRollBackward = \point tip -> ChainSyncClient $ runUnlift u $  do
          onRollBackward point tip
          pure $ idle u
      }
    done u = ClientStNext
      { recvMsgRollForward = \_ _ -> ChainSyncClient $ SendMsgDone <$> runUnlift u onDone
      , recvMsgRollBackward = \_ _ -> ChainSyncClient $ SendMsgDone <$> runUnlift u onDone
      }
