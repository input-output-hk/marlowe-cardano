{-# LANGUAGE GADTs #-}

module Data.Marlowe.LiveContracts (
  LiveContracts (securityParameter),
  latestPoints,
  create,
  currentLive,
  allContracts,
  rollBackward,
  rollForward,
  validate,
  shrinkLiveContracts,
) where

import Control.Category ((>>>))
import Control.Monad (guard, when)
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
import qualified Data.Map as Map
import Data.Map.Internal (Map (..))
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  BlockNo,
  TxOutRef (..),
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.Core.Api (
  MarloweVersion (..),
  Transaction (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
 )
import Language.Marlowe.Runtime.History.Api (
  CreateStep (..),
  MarloweApplyInputsTransaction (..),
  MarloweBlock (..),
  MarloweCreateTransaction (..),
  SomeCreateStep (..),
  UnspentContractOutput (..),
 )

-- | Records a history of live contracts over a window of history.
data LiveContracts a = LiveContracts
  { securityParameter :: Int
  -- ^ The number of blocks to retain in case of rollback.
  , history :: History a -- Map BlockNo (Map TxOutRef Contract)
  }
  deriving (Show, Eq, Ord)

data History a
  = EmptyHistory (WithGenesis BlockNo)
  | NonEmptyHistory
      (Map TxOutRef a)
      -- ^ Initial snapshot of live contracts
      BlockNo
      -- ^ most recent recorded block
      (Map TxOutRef a)
      -- ^ snapshot at most recent block
      BlockNo
      -- ^ most recent recorded tip
      (Map BlockNo (Map TxOutRef a))
      -- ^ snapshots of less recently recorded points after security window
  deriving (Show, Eq, Ord)

-- | Create a new live contracts collection.
create
  :: Int
  -- ^ The protocol security parameter (the number of blocks before a rollback is guaranteed not to happen).
  -> LiveContracts a
create securityParameter =
  LiveContracts
    { securityParameter
    , history = EmptyHistory Genesis
    }

-- | Get the live contracts as of the latest recorded block.
currentLive :: LiveContracts a -> Map TxOutRef a
currentLive LiveContracts{..} = case history of
  EmptyHistory _ -> mempty
  NonEmptyHistory _ _ snapshot _ _ -> snapshot

latestPoints :: LiveContracts a -> Either (WithGenesis BlockNo) (BlockNo, BlockNo)
latestPoints LiveContracts{..} = case history of
  EmptyHistory tip -> Left tip
  NonEmptyHistory _ point _ tip _ -> Right (point, tip)

-- | Get all live contracts within the history window.
allContracts :: LiveContracts a -> Map TxOutRef a
allContracts LiveContracts{..} = case history of
  EmptyHistory _ -> mempty
  NonEmptyHistory _ _ snapshot _ snapshots -> snapshot <> fold snapshots

-- | Add a new marlowe block to a live contracts collection.
rollForward
  :: forall a
   . BlockNo
  -- ^ The current block number of the chain tip.
  -> (Contract -> a)
  -- ^ Project the stored data from a contract
  -> MarloweBlock
  -- ^ The block to add.
  -> LiveContracts a
  -> LiveContracts a
rollForward tipBlockNo f MarloweBlock{..} LiveContracts{..}
  | tipBlockNo < blockNo = error "rollForward: new block is after tip"
  | otherwise =
      LiveContracts
        { history = case history of
            EmptyHistory _ ->
              NonEmptyHistory
                mempty
                blockNo
                (updateLiveContracts mempty)
                tipBlockNo
                mempty
            NonEmptyHistory initial prevBlockNo prevSnapshot _ prevHistory
              | blockNo <= prevBlockNo -> error "rollForward: non-increasing block number"
              | otherwise ->
                  let (initial', nextHistory) =
                        trimHistory securityParameter tipBlockNo initial $
                          Map.insert prevBlockNo prevSnapshot prevHistory
                   in NonEmptyHistory
                        initial'
                        blockNo
                        (updateLiveContracts prevSnapshot)
                        tipBlockNo
                        nextHistory
        , ..
        }
  where
    BlockHeader{..} = blockHeader

    updateLiveContracts = consumeUpdatedContracts . addNewContracts

    addNewContracts = Map.union (getNewContracts createTransactions)

    consumeUpdatedContracts = flip (foldl' consumeUpdatedContract) applyInputsTransactions

    getNewContracts :: [MarloweCreateTransaction] -> Map TxOutRef a
    getNewContracts = foldMap \MarloweCreateTransaction{..} ->
      Map.mapKeysMonotonic (TxOutRef txId) $
        newContracts <&> \case
          SomeCreateStep MarloweV1 CreateStep{createOutput = TransactionScriptOutput{datum = MarloweData{..}}} ->
            f marloweContract

    consumeUpdatedContract :: Map TxOutRef a -> MarloweApplyInputsTransaction -> Map TxOutRef a
    consumeUpdatedContract
      liveContracts
      (MarloweApplyInputsTransaction MarloweV1 UnspentContractOutput{..} Transaction{blockHeader = _, ..}) =
        case scriptOutput output of
          Nothing -> Map.delete txOutRef liveContracts
          Just TransactionScriptOutput{..} ->
            Map.insert utxo (f $ marloweContract datum) $
              Map.delete txOutRef liveContracts

trimHistory
  :: Int
  -> BlockNo
  -> Map TxOutRef a
  -> Map BlockNo (Map TxOutRef a)
  -> (Map TxOutRef a, Map BlockNo (Map TxOutRef a))
trimHistory securityParameter tip = go
  where
    go initial =
      Map.minViewWithKey >>> \case
        -- Empty history - stop trimming
        Nothing -> (initial, mempty)
        Just ((k, a), h)
          -- History entry is before security window - trim it
          | k < (tip - fromIntegral securityParameter) -> go a h
          -- History entry is within security window - stop trimming
          | otherwise -> (initial, Map.insert k a h)

-- | Revert a live contracts collection to a previous point.
rollBackward
  :: WithGenesis BlockNo
  -- ^ The point to which to roll back.
  -> WithGenesis BlockNo
  -- ^ The new tip.
  -> LiveContracts a
  -> LiveContracts a
-- Rolling back to genesis just clears all history.
rollBackward Genesis tip liveContracts = liveContracts{history = EmptyHistory tip}
rollBackward _ Genesis _ = error "rollBackward: new point must be genesis when new tip is genesis"
rollBackward (At blockNo) (At tip) LiveContracts{..}
  | blockNo > tip = error "rollBackward: new point is ahead of new tip"
  | otherwise =
      LiveContracts
        { history = case history of
            EmptyHistory _ -> EmptyHistory $ At tip
            NonEmptyHistory initial prevBlockNo prevSnapshot prevTip prevHistory
              | fromIntegral blockNo < fromIntegral @_ @Integer prevTip - fromIntegral securityParameter ->
                  error "rollBackward: new point is before security window"
              | blockNo == prevBlockNo ->
                  NonEmptyHistory
                    initial
                    prevBlockNo
                    prevSnapshot
                    tip
                    prevHistory
              | blockNo > prevBlockNo ->
                  error "rollBackward: new point is ahead of current point"
              | otherwise -> case rollbackHistory $ Map.insert prevBlockNo prevSnapshot prevHistory of
                  Nothing -> NonEmptyHistory initial blockNo initial tip mempty
                  Just ((newRollbackBlock, newSnapshot), newHistory) ->
                    NonEmptyHistory
                      initial
                      newRollbackBlock
                      newSnapshot
                      tip
                      newHistory
        , ..
        }
  where
    rollbackHistory snapshots = do
      ((latestBlockNo, snapshot), snapshots') <- Map.maxViewWithKey snapshots
      if latestBlockNo <= blockNo
        then pure ((latestBlockNo, snapshot), snapshots')
        else rollbackHistory snapshots'

-- | For testing
validate :: LiveContracts a -> IO ()
validate LiveContracts{..} = do
  assertIO (securityParameter >= 0) "securityParameter < 0"
  case history of
    EmptyHistory{} -> pure ()
    NonEmptyHistory _ blockNo _ tip snapshots -> do
      assertIO
        (Map.null snapshots || blockNo > fst (Map.findMax snapshots))
        "snapshot history contains blocks larger than latest"
      assertIO (tip >= blockNo) "tip < blockNo"
      when (tip > fromIntegral securityParameter) do
        let minBlockNo = tip - fromIntegral securityParameter
        assertIO
          (Map.null snapshots || minBlockNo <= fst (Map.findMin snapshots))
          "snapshot history contains blocks older than security window"

-- | For testing
shrinkLiveContracts :: (Map TxOutRef a -> [Map TxOutRef a]) -> LiveContracts a -> [LiveContracts a]
shrinkLiveContracts shrinkContractMap LiveContracts{..} = maybe id (:) shrunkBySecurityParameter shrunkByHistory
  where
    shrunkBySecurityParameter = do
      guard (securityParameter > 0)
      let securityParameter' = securityParameter - 1
      pure $ LiveContracts securityParameter' case history of
        EmptyHistory tip -> EmptyHistory tip
        NonEmptyHistory initial blockNo snapshot tip snapshots ->
          case trimHistory securityParameter' tip initial snapshots of
            (initial', snapshots') ->
              NonEmptyHistory initial' blockNo snapshot tip snapshots'

    shrunkByHistory =
      LiveContracts securityParameter <$> case history of
        EmptyHistory Genesis -> []
        EmptyHistory (At tip) -> EmptyHistory Genesis : (EmptyHistory . At <$> shrinkBlockNo tip)
        NonEmptyHistory initial blockNo snapshot tip snapshots ->
          EmptyHistory (At tip)
            : fold
              [ [NonEmptyHistory initial' blockNo snapshot tip snapshots | initial' <- shrinkContractMap initial]
              , [ NonEmptyHistory
                  initial
                  blockNo'
                  snapshot
                  tip
                  (Map.filterWithKey (\k _ -> k < blockNo') snapshots)
                | blockNo' <- shrinkBlockNo blockNo
                ]
              , [NonEmptyHistory initial blockNo snapshot' tip snapshots | snapshot' <- shrinkContractMap snapshot]
              , [ NonEmptyHistory
                  initial
                  (min blockNo tip')
                  snapshot
                  tip'
                  (Map.filterWithKey (\k _ -> k < min blockNo tip') snapshots)
                | tip' <- shrinkBlockNo tip
                ]
              , [ case trimHistory securityParameter tip initial snapshots' of
                  (initial', snapshots'') ->
                    NonEmptyHistory
                      initial'
                      blockNo
                      snapshot
                      tip
                      snapshots''
                | snapshots' <- shrinkMap shrinkBlockNo shrinkContractMap snapshots
                ]
              ]

    shrinkBlockNo 0 = []
    shrinkBlockNo blockNo = [blockNo - 1]

    -- all vendored from https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/src/Test.QuickCheck.Arbitrary.html
    shrinkMap shrinkKey shrinkValue =
      fmap Map.fromList . shrinkList (shrinkTuple shrinkKey shrinkValue) . Map.toList

    shrinkTuple shrinkFst shrinkSnd (a, b) =
      fold
        [ (,b) <$> shrinkFst a
        , (a,) <$> shrinkSnd b
        ]

    shrinkList shr xs =
      fold
        [ fold [removes k n xs | k <- takeWhile (> 0) (iterate (`div` 2) n)]
        , shrinkOne xs
        ]
      where
        n = length xs

        shrinkOne [] = []
        shrinkOne (x : xs') =
          [x' : xs' | x' <- shr x]
            ++ [x : xs'' | xs'' <- shrinkOne xs']

        removes k i xs'
          | k > i = []
          | null xs2 = [[]]
          | otherwise = xs2 : map (xs1 ++) (removes k (i - k) xs2)
          where
            xs1 = take k xs'
            xs2 = drop k xs'

assertIO :: Bool -> String -> IO ()
assertIO True _ = pure ()
assertIO False msg = fail msg
