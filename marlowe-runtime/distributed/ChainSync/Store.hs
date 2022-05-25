{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module ChainSync.Store where

import ChainSync.Client (ChainSyncMsg (..), GetBlocks (..), GetIntersectionPoints (..))
import Control.Distributed.Process (Closure, Process, match, receiveWait, send)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Data.Binary (Binary)
import Data.Data (Typeable)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.List (groupBy)
import Data.Maybe (maybeToList)
import Data.Word (Word32)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainEvent (..), MarloweChainPoint (..),
                                             MarloweSlotNo (..))

data ChainSyncStoreConfig = ChainSyncStoreConfig
  { directory :: FilePath
  , batchSize :: Word32
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncStoreConfig

-- The gen_server implementation slowed everything to a crawl...
-- chainSyncStore :: ChainSyncStoreConfig -> Process ()
-- chainSyncStore config = do
--   serve config initialize definition
--   where
--     -- For now, we use an IntMap as an in-memory store
--     initialize _ = pure $ InitOk IntMap.empty NoDelay
--     definition = defaultProcess
--       { apiHandlers =
--           [ handleCast handleMsg
--           , handleCall handleGetIntersectionPoints
--           , handleCall handleGetBlocks
--           ]
--       }
--     handleGetBlocks blocks GetBlocks =
--       pure $ ProcessReply blocks $ ProcessContinue blocks
--     handleGetIntersectionPoints blocks GetIntersectionPoints = do
--       let grouper a b = a `mod` 4000 == b `mod` 4000
--       let intersectionKeys = fmap head $ groupBy grouper $ IntMap.keys blocks
--       let intersector = IntMap.fromDistinctAscList $ zip intersectionKeys $ repeat ()
--       let intersectionBlocks = IntMap.intersection blocks intersector
--       let toPoint (MarloweBlockHeader slot hash _, _) = MarloweChainPoint slot hash
--       let intersectionPoints = toPoint . snd <$> IntMap.toAscList intersectionBlocks
--       pure $ ProcessReply intersectionPoints $ ProcessContinue blocks
--     handleMsg blocks (ChainSyncStart _ _) = do
--       pure $ ProcessContinue blocks
--     handleMsg blocks (ChainSyncEvent (MarloweRollBackward point _)) = do
--       let pointNo = fromIntegral $ getPointNo point
--       let maxPoint = fst . fst <$> IntMap.maxViewWithKey blocks
--       let
--         rolledBackKeys = case maxPoint of
--           Nothing -> IntSet.empty
--           Just mp -> IntSet.fromDistinctAscList [pointNo..mp]
--       pure $ ProcessContinue $ IntMap.withoutKeys blocks rolledBackKeys
--     handleMsg blocks (ChainSyncEvent (MarloweRollForward header txs _)) = do
--       let MarloweBlockHeader (MarloweSlotNo slotNo) _ _ = header
--       pure $ ProcessContinue $ IntMap.insert (fromIntegral slotNo) (header, txs) blocks
--     handleMsg _ ChainSyncDone = do
--       pure $ ProcessStop ExitNormal
--     getPointNo MarloweChainPointAtGenesis              = 0
--     getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n

chainSyncStore :: ChainSyncStoreConfig -> Process ()
-- For now, we use an IntMap as an in-memory store
chainSyncStore _ = do
  go IntMap.empty
  where
    go blocks = receiveWait
      [ match $ handleMsg blocks
      , match $ handleGetBlocks blocks
      , match $ handleGetIntersectionPoints blocks
      ]
    handleGetBlocks blocks (GetBlocks replyTo) = do
      send replyTo blocks
      go blocks
    handleGetIntersectionPoints blocks (GetIntersectionPoints replyTo) = do
      let mMaxItem = traceShowId $ fst <$> IntMap.maxView blocks
      let toPoint (MarloweBlockHeader slot hash _, _) = MarloweChainPoint slot hash
      let intersectionPoints = maybeToList $ toPoint <$> mMaxItem
      send replyTo intersectionPoints
      go blocks
    handleMsg blocks (ChainSyncStart _ _) = do
      go blocks
    handleMsg blocks (ChainSyncEvent (MarloweRollBackward point _)) = do
      let pointNo = fromIntegral $ getPointNo point
      let maxPoint = fst . fst <$> IntMap.maxViewWithKey blocks
      let
        rolledBackKeys = case maxPoint of
          Nothing -> IntSet.empty
          Just mp -> IntSet.fromDistinctAscList [pointNo..mp]
      go $ IntMap.withoutKeys blocks rolledBackKeys
    handleMsg blocks (ChainSyncEvent (MarloweRollForward header txs _)) = do
      let MarloweBlockHeader (MarloweSlotNo slotNo) _ _ = header
      go $ IntMap.insert (fromIntegral slotNo) (header, txs) blocks
    handleMsg _ ChainSyncDone = pure ()
    getPointNo MarloweChainPointAtGenesis              = 0
    getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n

remotable ['chainSyncStore]

process :: ChainSyncStoreConfig -> Closure (Process ())
process = $(mkClosure 'chainSyncStore)
