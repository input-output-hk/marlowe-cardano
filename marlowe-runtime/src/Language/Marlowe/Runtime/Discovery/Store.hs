module Language.Marlowe.Runtime.Discovery.Store
  where

import Control.Arrow ((&&&))
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever, mfilter, (<=<))
import Data.Foldable (fold)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, PolicyId, WithGenesis(..))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.Discovery.Chain (Changes(..), isEmptyChanges)

newtype DiscoveryStoreDependencies = DiscoveryStoreDependencies
  { changes :: STM Changes
  }

data DiscoveryStore = DiscoveryStore
  { runDiscoveryStore :: IO Void
  , getHeaders :: IO [ContractHeader]
  , getHeadersByRoleTokenCurrency :: PolicyId -> IO [ContractHeader]
  , getNextHeaders :: ChainPoint -> IO (Maybe (Either ChainPoint (BlockHeader, [ContractHeader])))
  , getIntersect :: [BlockHeader] -> IO (Maybe BlockHeader)
  }

data BlockData
  = Rollback ChainPoint
  | Block [ContractHeader]

mkDiscoveryStore :: DiscoveryStoreDependencies -> STM DiscoveryStore
mkDiscoveryStore DiscoveryStoreDependencies{..} = do
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
  pure DiscoveryStore
    { runDiscoveryStore = do
        atomically do
          writeTVar blocksVar mempty
          writeTVar roleTokenIndex mempty
        forever $ atomically do
          Changes{..} <- mfilter (not . isEmptyChanges) changes
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

    , getHeaders = (snd <=< Map.toAscList . fmap extractHeaders) <$> readTVarIO blocksVar
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
    }

for_ :: Maybe ChainPoint -> (WithGenesis a0 -> t0) -> STM a1
for_ = error "not implemented"

extractHeaders :: BlockData -> [ContractHeader]
extractHeaders = \case
  Block hs -> hs
  _ -> []
