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
data LiveContracts = LiveContracts
  { securityParameter :: Int
  -- ^ The number of blocks to retain in case of rollback.
  , history :: History -- Map BlockNo (Map TxOutRef Contract)
  }
  deriving (Show, Eq, Ord)

data History
  = EmptyHistory (WithGenesis BlockNo)
  | NonEmptyHistory
      (Map TxOutRef Contract)
      -- ^ Initial snapshot of live contracts
      BlockNo
      -- ^ most recent recorded block
      (Map TxOutRef Contract)
      -- ^ snapshot at most recent block
      BlockNo
      -- ^ most recent recorded tip
      (Map BlockNo (Map TxOutRef Contract))
      -- ^ snapshots of less recently recorded points after security window
  deriving (Show, Eq, Ord)

-- | Create a new live contracts collection.
create
  :: Int
  -- ^ The protocol security parameter (the number of blocks before a rollback is guaranteed not to happen).
  -> LiveContracts
create securityParameter =
  LiveContracts
    { securityParameter
    , history = EmptyHistory Genesis
    }

-- | Get the live contracts as of the latest recorded block.
currentLive :: LiveContracts -> Map TxOutRef Contract
currentLive LiveContracts{..} = case history of
  EmptyHistory _ -> mempty
  NonEmptyHistory _ _ snapshot _ _ -> snapshot

latestPoints :: LiveContracts -> Either (WithGenesis BlockNo) (BlockNo, BlockNo)
latestPoints LiveContracts{..} = case history of
  EmptyHistory tip -> Left tip
  NonEmptyHistory _ point _ tip _ -> Right (point, tip)

-- | Get all live contracts within the history window.
allContracts :: LiveContracts -> Map TxOutRef Contract
allContracts LiveContracts{..} = case history of
  EmptyHistory _ -> mempty
  NonEmptyHistory _ _ snapshot _ snapshots -> snapshot <> fold snapshots

-- | Add a new marlowe block to a live contracts collection.
rollForward
  :: BlockNo
  -- ^ The current block number of the chain tip.
  -> MarloweBlock
  -- ^ The block to add.
  -> LiveContracts
  -> LiveContracts
rollForward tipBlockNo MarloweBlock{..} LiveContracts{..}
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

    addNewContracts = Map.union $ getNewContracts createTransactions

    consumeUpdatedContracts = flip (foldl' consumeUpdatedContract) applyInputsTransactions

    getNewContracts :: [MarloweCreateTransaction] -> Map TxOutRef Contract
    getNewContracts = foldMap \MarloweCreateTransaction{..} ->
      Map.mapKeysMonotonic (TxOutRef txId) $
        newContracts <&> \case
          SomeCreateStep MarloweV1 CreateStep{createOutput = TransactionScriptOutput{datum = MarloweData{..}}} ->
            marloweContract

    consumeUpdatedContract :: Map TxOutRef Contract -> MarloweApplyInputsTransaction -> Map TxOutRef Contract
    consumeUpdatedContract
      liveContracts
      (MarloweApplyInputsTransaction MarloweV1 UnspentContractOutput{..} Transaction{blockHeader = _, ..})
        | Map.member txOutRef liveContracts =
            case scriptOutput output of
              Nothing -> Map.delete txOutRef liveContracts
              Just TransactionScriptOutput{..} ->
                Map.insert utxo (marloweContract datum) $
                  Map.delete txOutRef liveContracts
        | otherwise = error $ "rollForward: block consumes non-existent output " <> show txOutRef

trimHistory
  :: Int
  -> BlockNo
  -> Map TxOutRef Contract
  -> Map BlockNo (Map TxOutRef Contract)
  -> (Map TxOutRef Contract, Map BlockNo (Map TxOutRef Contract))
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
  -> LiveContracts
  -> LiveContracts
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
              | otherwise -> case fmap Map.maxViewWithKey
                  . trimHistory securityParameter tip initial
                  . Map.filterWithKey (\k _ -> k <= blockNo)
                  . Map.insert prevBlockNo prevSnapshot
                  $ prevHistory of
                  (initial', Nothing) -> NonEmptyHistory initial' blockNo initial' tip mempty
                  (initial', Just ((k, v), m)) -> NonEmptyHistory initial' k v tip m
        , ..
        }

-- | For testing
validate :: LiveContracts -> IO ()
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
shrinkLiveContracts :: (Map TxOutRef Contract -> [Map TxOutRef Contract]) -> LiveContracts -> [LiveContracts]
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
              , do
                  blockNo' <- shrinkBlockNo blockNo
                  let snapshots' = Map.insert blockNo' snapshot $ Map.filterWithKey (\k _ -> k < blockNo') snapshots
                  pure case Map.maxViewWithKey <$> trimHistory securityParameter tip initial snapshots' of
                    (initial', Nothing) -> NonEmptyHistory initial' blockNo' initial' tip mempty
                    (initial', Just ((k, v), m)) -> NonEmptyHistory initial' k v tip m
              , [NonEmptyHistory initial blockNo snapshot' tip snapshots | snapshot' <- shrinkContractMap snapshot]
              , do
                  tip' <- shrinkBlockNo tip
                  pure case Map.maxViewWithKey $ Map.filterWithKey (\k _ -> k <= tip') $ Map.insert blockNo snapshot snapshots of
                    Nothing -> NonEmptyHistory initial (min tip' blockNo) initial tip' mempty
                    Just ((k, v), m) -> NonEmptyHistory initial k v tip' m
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
