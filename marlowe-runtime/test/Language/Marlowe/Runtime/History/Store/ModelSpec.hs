{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Marlowe.Runtime.History.Store.ModelSpec
  (spec
  , genFindCreateStepArgs
  , genFindNextStepArgs
  , modelFromScript
  , genFindIntersectionArgs
  ) where

import Data.Bifunctor (Bifunctor(first))
import Data.List (foldl')
import qualified Data.Set as Set
import Data.Some (withSome)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader(..), ChainPoint, TxIx(..), TxOutRef(..), WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..), SomeMarloweVersion(..), Transaction(..))
import Language.Marlowe.Runtime.History.Api (ContractStep(..), SomeCreateStep(..))
import Language.Marlowe.Runtime.History.Script (HistoryScript(..), HistoryScriptEvent(..), genTxId)
import Language.Marlowe.Runtime.History.Store (FindNextStepsResponse(..), Intersection(..), SomeContractSteps(..))
import Language.Marlowe.Runtime.History.Store.Model
  ( HistoryRoot(..)
  , HistoryStoreModel(..)
  , addContract
  , addSteps
  , emptyHistoryStore
  , findCreateStep
  , findIntersection
  , findNextSteps
  , getAllBlocks
  , getBlocks
  , getRootBlocks
  , getRoots
  , getTip
  , rollback
  )
import Test.Hspec (Spec)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(..), Gen, chooseInt, discard, elements, frequency, oneof, sublistOf, suchThat, (===))
import Test.QuickCheck.Property ((==>))

instance Arbitrary HistoryStoreModel where
  arbitrary = modelFromScript <$> arbitrary

modelFromScript :: HistoryScript -> HistoryStoreModel
modelFromScript = snd
  . foldl' (\store event -> withSome event mkStep store) ([], emptyHistoryStore)
  . unHistoryScript

pickRoot :: HistoryStoreModel -> Gen (ContractId, HistoryRoot)
pickRoot = elements . getRoots

genIntersectionBlocks :: HistoryStoreModel -> HistoryRoot -> Gen [BlockHeader]
genIntersectionBlocks store root = oneof
  [ sublistOf $ getAllBlocks store
  , do
      let contractBlocks = Set.toAscList $ getRootBlocks root
      n <- chooseInt (0, length contractBlocks)
      pure $ take n contractBlocks
  ]

genPoint :: HistoryStoreModel -> Gen ChainPoint
genPoint = elements . (Genesis :) . fmap At . getBlocks

spec :: Spec
spec = do
  -- This property specifies that after a rollback, the tip of the store will
  -- be equal to the rollback point
  prop "getTip / rollback" \store -> do
    point <- genPoint store
    pure $ getTip (rollback point store) === point

  -- This property specifies that performing two rollbacks in a row is
  -- equivalent to rolling back to the second point directly.
  prop "rollback composition" \store -> do
    point1 <- genPoint store
    point2 <- genPoint store `suchThat` (<= point1)
    pure $ rollback point2 (rollback point1 store) === rollback point2 store

  -- This property specifies that performing a rollback to a specific block is
  -- an idempotent operation.
  prop "rollback idempotent" \store -> do
    point <- genPoint store
    pure $ rollback point (rollback point store) === rollback point store

  -- This property specifies that the result of calling findIntersection
  -- can never return a point bigger than the tip.
  prop "find intersection constraint" \store -> not (null $ getRoots store) ==> do
    (contractId, root) <- pickRoot store
    intersectionPoints <- genIntersectionBlocks store root
    let intersect = findIntersection contractId intersectionPoints store
    pure case intersect of
      Nothing                     -> discard
      Just (Intersection _ block) -> At block <= getTip store

  -- This property specifies that rolling back to the tip is a no-op.
  prop "rollback tip" \store ->
    rollback (getTip store) store === store

  -- This property specifies that finding a contract that exists succeeds.
  prop "getRoot / findCreateStep" \store -> not (null $ getRoots store) ==> do
    (contractId, HistoryRoot version block createStep _ _ _) <- pickRoot store
    pure $ findCreateStep contractId store === Just (block, SomeCreateStep version createStep)

  -- This property specifies that finding the next steps from genesis for a contract that exists succeeds.
  prop "getRoot / getNextSteps Genesis" \store -> not (null $ getRoots store) ==> do
    (contractId, HistoryRoot version block _ steps _ _) <- pickRoot store
    pure case steps of
      [] -> discard
      _  -> findNextSteps contractId Genesis store === FindNext block (SomeContractSteps version steps)

  -- This property specifies that when next steps are found, the block must be
  -- bigger than the block provided.
  prop "findNextSteps block increasing" \store -> not (null $ getRoots store) ==> do
    (contractId, _, point) <- genFindNextStepArgs store
    pure case findNextSteps contractId point store of
      FindNext block _ -> At block > point
      _                -> discard

  -- This property specifies that findIntersection can only return blocks that
  -- are in the store and are not rolled back.
  prop "findIntersection exists" \store -> not (null $ getRoots store) ==> do
    (contractId, _, blocks) <- genFindIntersectionArgs store
    pure case findIntersection contractId blocks store of
      Just (Intersection _ block) -> Set.member block $ Set.fromDistinctAscList $ getBlocks store
      _                           -> discard

mkStep :: HistoryScriptEvent a -> ([BlockHeader], HistoryStoreModel) -> ([BlockHeader], HistoryStoreModel)
mkStep = \case
  RollForward slotOffset hash -> first \case
    []                            -> [BlockHeader slotOffset hash 0]
    headers@(BlockHeader{..} : _) -> BlockHeader (slotNo + slotOffset) hash (blockNo + 1) : headers
  RollBackward n -> \(headers, store) -> case drop (fromIntegral n) headers of
    []                   -> ([], rollback Genesis store)
    headers'@(block : _) -> (headers', rollback (At block) store)
  CreateContract contractId version createStep -> \case
    ([], _) -> error "Create in empty chain"
    (headers@(block : _), store) ->
      case addContract contractId version block createStep store of
        Left msg     -> error msg
        Right store' -> (headers, store')
  ApplyInputs version transaction@Transaction{..} -> \case
    ([], _) -> error "ApplyInputs in empty chain"
    (headers@(block : _), store) ->
      case addSteps contractId version block [ApplyTransaction transaction] store of
        Left msg     -> error msg
        Right store' -> (headers, store')
  Withdraw contractId version redeemStep -> \case
    ([], _) -> error "ApplyInputs in empty chain"
    (headers@(block : _), store) ->
      case addSteps contractId version block [RedeemPayout redeemStep] store of
        Left msg     -> error msg
        Right store' -> (headers, store')

genFindCreateStepArgs :: HistoryStoreModel -> Gen ContractId
genFindCreateStepArgs store = frequency
  [ (5, fst <$> pickRoot store)
  , (1, ContractId <$> (TxOutRef <$> genTxId <*> (TxIx . fromIntegral <$> chooseInt (0, 10))))
  ]

genFindNextStepArgs :: HistoryStoreModel -> Gen (ContractId, SomeMarloweVersion, ChainPoint)
genFindNextStepArgs store = do
  (contractId, HistoryRoot version _ _ _ _ _) <- pickRoot store
  point <- elements $ Genesis : (At <$> getAllBlocks store)
  pure (contractId, SomeMarloweVersion version, point)

genFindIntersectionArgs :: HistoryStoreModel -> Gen (ContractId, SomeMarloweVersion, [BlockHeader])
genFindIntersectionArgs store = do
  (contractId, root@(HistoryRoot version _ _ _ _ _)) <- pickRoot store
  blocks <- genIntersectionBlocks store root
  pure (contractId, SomeMarloweVersion version, blocks)
