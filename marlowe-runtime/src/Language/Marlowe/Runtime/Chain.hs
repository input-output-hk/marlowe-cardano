{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Marlowe.Runtime.Chain where

import Cardano.Api (Block (..), BlockInMode (..), CardanoMode, ChainPoint, ChainSyncClient (..), ChainTip,
                    EraInMode (..), GenesisParameters (..), SlotNo (..), Tx, TxBody (..), TxBodyContent (..), TxIn (..),
                    TxIx (..), TxMetadataInEra (..), TxMetadataJsonSchema (..), TxMintValue (..), TxOut (..),
                    TxOutDatum (..), TxValidityLowerBound (..), TxValidityUpperBound (..), ValueNestedBundle (..),
                    ValueNestedRep (..), getTxBody, getTxId, metadataToJson, txOutValueToValue, valueToNestedRep)
import Cardano.Api.ChainSync.Client (ClientStIdle (..), ClientStIntersect (..), ClientStNext (..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (addUTCTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Language.Marlowe.Runtime.Chain.Types (MarloweChainEvent (..), MarloweChainSyncClient, MarloweTx (MarloweTx),
                                             MarloweTxOut (MarloweTxOut),
                                             MarloweValidityInterval (MarloweValidityInterval))

marloweChainSyncClient
  :: forall m a
   . Monad m
  => GenesisParameters -- ^ GenesisParameters for the network
  -> [ChainPoint] -- ^ Known starting points
  -> (Maybe ChainPoint -> ChainTip -> m ()) -- ^ onStart callback. Receives the starting point and the tip.
  -> m Bool -- ^ onIdle callback. Allows the caller to terminate the client by returning True.
  -> (forall env. MarloweChainEvent env -> m ()) -- ^ onEvent callback. Receives the new block and the tip
  -> m a -- ^ onDone callback. Allows the caller to yeild a result for the client.
  -> m (MarloweChainSyncClient m a)
marloweChainSyncClient genesisParams startingPoints onStart onIdle onEvent onDone = do
  pure $ mkChainSyncClient
    startingPoints
    onStart
    onIdle
    onRollForward
    onRollBackward
    onDone
  where
    onRollForward (BlockInMode block AlonzoEraInCardanoMode ) = onRollForward' block
    onRollForward (BlockInMode block MaryEraInCardanoMode   ) = onRollForward' block
    onRollForward (BlockInMode block AllegraEraInCardanoMode) = onRollForward' block
    onRollForward (BlockInMode block ShelleyEraInCardanoMode) = onRollForward' block
    onRollForward (BlockInMode block ByronEraInCardanoMode  ) = onRollForward' block

    onRollForward' :: forall era. Block era -> ChainTip -> m ()
    onRollForward' (Block header txns) =
       onEvent . MarloweRollForward header (extractMarloweTx <$> txns)

    onRollBackward point = onEvent . MarloweRollBackward point

    extractMarloweTx :: forall era. Tx era -> MarloweTx era
    extractMarloweTx tx =
      let
        txBody@(TxBody TxBodyContent {..}) = getTxBody tx
        txId = getTxId txBody
      in
        MarloweTx
          txId
          (extractMints txMintValue)
          (extractInterval txValidityRange)
          (extractMetadata txMetadata)
          (fst <$> txIns)
          (uncurry (extractMarloweTxOut txId) <$> zip [0..] txOuts)

    extractMints TxMintNone = []
    extractMints (TxMintValue _ value _) =
      let
        extractPolicy (ValueNestedBundle policy quantities)
          | Map.null $ Map.filter (> 0) quantities = mempty
          | otherwise = Set.singleton policy
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

    extractMarloweTxOut txId ix (TxOut address value datum) =
      MarloweTxOut (TxIn txId (TxIx ix)) address (txOutValueToValue value) case datum of
        TxOutDatum _ datum' -> Just datum'
        _                   -> Nothing

mkChainSyncClient
  :: Monad m
  => [ChainPoint] -- ^ Known starting points
  -> (Maybe ChainPoint -> ChainTip -> m ()) -- ^ onStart callback. Receives the starting point and the tip.
  -> m Bool -- ^ onIdle callback. Allows the caller to terminate the client by returning True.
  -> (BlockInMode CardanoMode  -> ChainTip -> m ()) -- ^ onRollForward callback. Receives the new block and the tip
  -> (ChainPoint -> ChainTip -> m ()) -- ^ onRollBackward callback. Receives the point to rollback to and the tip
  -> m a -- ^ onDone callback. Allows the caller to yeild a result for the client.
  -> MarloweChainSyncClient m a
mkChainSyncClient startingPoints onStart onIdle onRollForward onRollBackward onDone =
  ChainSyncClient $ pure start
  where
    start = SendMsgFindIntersect startingPoints $ ClientStIntersect
        { recvMsgIntersectFound = \point tip -> ChainSyncClient do
          onStart (Just point) tip
          pure idle
        , recvMsgIntersectNotFound = \tip -> ChainSyncClient do
          onStart Nothing tip
          pure idle
        }
    idle = SendMsgRequestNext next do
      terminate <- onIdle
      pure if terminate then done else next
    next = ClientStNext
      { recvMsgRollForward = \block tip -> ChainSyncClient do
          onRollForward block tip
          pure idle
      , recvMsgRollBackward = \point tip -> ChainSyncClient do
          onRollBackward point tip
          pure idle
      }
    done = ClientStNext
      { recvMsgRollForward = \_ _ -> ChainSyncClient $ SendMsgDone <$> onDone
      , recvMsgRollBackward = \_ _ -> ChainSyncClient $ SendMsgDone <$> onDone
      }
