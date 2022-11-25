{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Discovery.Store
  where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, guard, mfilter, (<=<))
import Data.Aeson (ToJSON)
import Data.Foldable (asum, fold)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, PolicyId, WithGenesis(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.Discovery.Chain (ChainEvent(..))
import Observe.Event (EventBackend, addField, withEvent)
import Observe.Event.DSL (SelectorSpec(SelectorSpec))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))

data ChangesSummary = ChangesSummary
  { headerCount :: !Int
  , rollbackTo :: !(Maybe Chain.ChainPoint)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

compile $ SelectorSpec ["discovery", "store"]
  [ "save" ≔ ''ChangesSummary
  ]

data DiscoveryStoreDependencies r = DiscoveryStoreDependencies
  { chainEvents :: STM ChainEvent
  , eventBackend :: EventBackend IO r DiscoveryStoreSelector
  }

data DiscoveryStore = DiscoveryStore
  { getHeaders :: IO [ContractHeader]
  , getHeadersByRoleTokenCurrency :: PolicyId -> IO [ContractHeader]
  , getNextHeaders :: ChainPoint -> IO (Maybe (Either ChainPoint (BlockHeader, [ContractHeader])))
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  }

data Changes = Changes
  { headers :: !(Map Chain.BlockHeader (Set ContractHeader))
  , rollbackTo :: !(Maybe Chain.ChainPoint)
  } deriving (Show, Eq)

summarizeChanges :: Changes -> ChangesSummary
summarizeChanges Changes{..} = ChangesSummary
  { headerCount = sum $ length <$> headers
  , rollbackTo
  }

instance Semigroup Changes where
  c1 <> Changes{..} = c1' { headers = Map.unionWith (<>) headers1 headers }
    where
      c1'@Changes{headers=headers1} = maybe c1 (flip applyRollback c1) rollbackTo

instance Monoid Changes where
  mempty = Changes Map.empty Nothing

isEmptyChanges :: Changes -> Bool
isEmptyChanges (Changes headers Nothing) = null $ fold headers
isEmptyChanges _ = False

applyRollback :: Chain.ChainPoint -> Changes -> Changes
applyRollback Chain.Genesis _ = Changes mempty $ Just Chain.Genesis
applyRollback (Chain.At blockHeader@Chain.BlockHeader{slotNo}) Changes{..} = Changes
  { headers = headers'
  , rollbackTo = asum @[]
      [ guard (Map.null headers') *> (min (Just (Chain.At blockHeader)) rollbackTo <|> Just (Chain.At blockHeader))
      , rollbackTo
      ]
  }
  where
    headers' = Map.filterWithKey (const . isNotRolledBack) headers
    isNotRolledBack = not . Chain.isAfter slotNo

data BlockData
  = Rollback ChainPoint
  | Block [ContractHeader]

discoveryStore :: Component IO (DiscoveryStoreDependencies r) DiscoveryStore
discoveryStore = proc DiscoveryStoreDependencies{..} -> do
  changes <- eventAggregator -< chainEvents
  worker -< WorkerDependencies{..}

eventAggregator :: Component IO (STM ChainEvent) (STM Changes)
eventAggregator = component \chainEvents -> do
  changesVar <- newTVar mempty
  let
    runAggregator = forever $ atomically do
      chainEvent <- chainEvents
      modifyTVar changesVar (<> eventToChanges chainEvent)
    getChanges = do
      changes <- readTVar changesVar
      writeTVar changesVar mempty
      pure changes
  pure (runAggregator, getChanges)

eventToChanges :: ChainEvent -> Changes
eventToChanges = \case
  RolledForward block headers -> mempty { headers = Map.singleton block headers }
  RolledBackward point -> mempty { rollbackTo = Just point }

data WorkerDependencies r = WorkerDependencies
  { changes :: STM Changes
  , eventBackend :: EventBackend IO r DiscoveryStoreSelector
  }

worker :: Component IO (WorkerDependencies r) DiscoveryStore
worker = component \WorkerDependencies{..} -> do
  blocksVar <- newTVar mempty
  roleTokenIndex <- newTVar mempty
  let
    modifyBlocks f = do
      blocks <- readTVar blocksVar
      let blocks' = f blocks
      writeTVar roleTokenIndex
        $ Map.fromList
        $ fmap (rolesCurrency . head &&& id)
        $ groupBy (on (==) rolesCurrency)
        $ sortOn rolesCurrency
        $ (extractHeaders =<<)
        $ Map.elems blocks'
      writeTVar blocksVar blocks'

    runDiscoveryStore = do
      atomically do
        writeTVar blocksVar mempty
        writeTVar roleTokenIndex mempty
      forever $ withEvent eventBackend Save \ev -> do
        summary <- atomically do
          ch@Changes{..} <- mfilter (not . isEmptyChanges) changes
          modifyBlocks \blocks ->
            let
              blocks' = case rollbackTo of
                Nothing -> blocks
                Just Genesis -> Rollback Genesis <$ blocks
                Just (At blockHeader) ->
                  let
                    (smaller, atBlock, larger) = Map.splitLookup blockHeader blocks
                  in
                    smaller
                      <> foldMap (Map.singleton blockHeader) atBlock
                      <> (Rollback (At blockHeader) <$ larger)
            in
              Map.union blocks' $ fmap (Block . Set.toList) headers
          pure $ summarizeChanges ch
        addField ev summary

  pure (runDiscoveryStore, DiscoveryStore
    { getHeaders = (snd <=< Map.toAscList . fmap extractHeaders) <$> readTVarIO blocksVar
    , getHeadersByRoleTokenCurrency = \policyId -> fold . Map.lookup policyId <$> readTVarIO roleTokenIndex
    , getNextHeaders = \point -> do
        blocks <- readTVarIO blocksVar
        pure case point of
          Genesis -> do
            (blockHeader, blockData) <- fst <$> Map.minViewWithKey blocks
            pure case blockData of
              Rollback point' -> Left point'
              Block headers -> Right (blockHeader, headers)
          At blockHeader -> case Map.lookup blockHeader blocks of
            Nothing -> Just $ Left Genesis
            Just blockData ->
              case blockData of
                Rollback point' -> pure $ Left point'
                _ -> Right <$> do
                  let (_, blocks') = Map.split blockHeader blocks
                  let
                    filtered = flip Map.mapMaybe blocks' \case
                      Block hs -> Just hs
                      _ -> Nothing
                  fst <$> Map.minViewWithKey filtered
    , getIntersect = \headers -> fmap fst
        . Set.maxView
        . Set.intersection (Set.fromList headers)
        . Map.keysSet
        . Map.filter \case
            Block _ -> True
            _ -> False
        <$> readTVarIO blocksVar
    })

extractHeaders :: BlockData -> [ContractHeader]
extractHeaders = \case
  Block hs -> hs
  _ -> []
