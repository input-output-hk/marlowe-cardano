{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE RankNTypes         #-}

module Language.Marlowe.Runtime.History.Store.Model where

import Control.Monad (guard, (<=<))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Min(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
import Data.Type.Equality (TestEquality(testEquality), type (:~:)(Refl))
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweVersion(..))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep, SomeCreateStep(SomeCreateStep))
import Language.Marlowe.Runtime.History.Follower (ContractChanges(..), SomeContractChanges(..))
import Language.Marlowe.Runtime.History.FollowerSupervisor (UpdateContract(..))
import Language.Marlowe.Runtime.History.Store
  (FindNextStepsResponse(..), Intersection(Intersection), SomeContractSteps(..))

-- | The history store is modelled as a collection of contract history traces.
-- Keyed by contract ID.
newtype HistoryStoreModel = HistoryStoreModel
  { unHistoryStoreModel :: Map ContractId HistoryRoot
  } deriving (Eq, Show)

-- | The root of a history tree.
data HistoryRoot where
  HistoryRoot
    :: MarloweVersion v
    -> BlockHeader -- The block associated with this node.
    -> CreateStep v -- The creation step of the contract
    -> [ContractStep v] -- Steps that occurred in the same block
    -> Set BlockHeader -- The blocks that used to grow as branches from this tree.
    -> Maybe (HistoryTree v) -- The rest of the history tree.
    -> HistoryRoot

instance Eq HistoryRoot where
  (==)
    (HistoryRoot MarloweV1 block1 create1 steps1 rolledBackBlocks1 next1)
    (HistoryRoot MarloweV1 block2 create2 steps2 rolledBackBlocks2 next2) =
      block1 == block2
      && create1 == create2
      && steps1 == steps2
      && rolledBackBlocks1 == rolledBackBlocks2
      && next1 == next2

instance Show HistoryRoot where
  showsPrec p (HistoryRoot version block create steps rolledBackBlocks next) =
    showParen (p >= 11) $ fold $ intersperse showSpace
      [ showString "HistoryRoot"
      , showsPrec 11 version
      , showsPrec 11 block
      , case version of
          MarloweV1 -> showsPrec 11 create
      , case version of
          MarloweV1 -> showsPrec 11 steps
      , showsPrec 11 rolledBackBlocks
      , case version of
          MarloweV1 -> showsPrec 11 next
      ]

-- | A tree of contract history branches.
data HistoryTree v where
  HistoryTree
    :: BlockHeader -- The block associated with this node.
    -> [ContractStep v] -- Steps that occurred at this block
    -> Set BlockHeader -- The blocks that used to grow as branches from this tree.
    -> Maybe (HistoryTree v) -- The rest of the history tree.
    -> HistoryTree v

instance IsMarloweVersion v => Eq (HistoryTree v) where
  (==)
    (HistoryTree block1 steps1 rolledBackBlocks1 next1)
    (HistoryTree block2 steps2 rolledBackBlocks2 next2) =
      block1 == block2
      && case marloweVersion @v of
          MarloweV1 -> steps1 == steps2
      && rolledBackBlocks1 == rolledBackBlocks2
      && next1 == next2

instance IsMarloweVersion v => Show (HistoryTree v) where
  showsPrec p (HistoryTree block steps rolledBackBlocks next) =
    showParen (p >= 11) $ fold $ intersperse showSpace
      [ showString "HistoryTree"
      , showsPrec 11 block
      , case marloweVersion @v of
          MarloweV1 -> showsPrec 11 steps
      , showsPrec 11 rolledBackBlocks
      , showsPrec 11 next
      ]

-- | Initialize an empty store model
emptyHistoryStore :: HistoryStoreModel
emptyHistoryStore = HistoryStoreModel mempty

-- | Add a new contract to the store. Returns 'Nothing' if the given
-- 'ContractId' is already present in the store.
addContract
  :: ContractId
  -> MarloweVersion v
  -> BlockHeader
  -> CreateStep v
  -> HistoryStoreModel
  -> Either String HistoryStoreModel
addContract contractId version block create (HistoryStoreModel store) = do
  guard $ not $ Map.member contractId store
  pure
    $ HistoryStoreModel
    $ flip (Map.insert contractId) store
    $ HistoryRoot version block create [] mempty Nothing

-- | Remove a contract from the store. Returns 'Nothing' if the store does not
-- contain the given 'ContractId'.
removeContract
  :: ContractId
  -> HistoryStoreModel
  -> Either String HistoryStoreModel
removeContract contractId (HistoryStoreModel store) = do
  guard $ Map.member contractId store
  pure $ HistoryStoreModel $ Map.delete contractId store

-- | Add new steps to a contract in the store. Returns 'Nothing' if the given
-- 'BlockHeader' is earlier than the current tip of the contract.
addSteps
  :: forall v
   . ContractId
  -> MarloweVersion v
  -> BlockHeader
  -> [ContractStep v]
  -> HistoryStoreModel
  -> Either String HistoryStoreModel
addSteps contractId version block steps (HistoryStoreModel store) = do
  root <- note "addSteps: Contract not found" $ Map.lookup contractId store
  root' <- addStepsToRoot version block steps root
  pure $ HistoryStoreModel $ Map.insert contractId root' store

-- | Add new steps to a 'HistoryRoot'. Returns 'Nothing' if the given
-- 'BlockHeader' is earlier than the current tip of the contract.

note :: String -> Maybe a -> Either String a
note msg = maybe (Left msg) Right

addStepsToRoot
  :: forall v
   . MarloweVersion v
  -> BlockHeader
  -> [ContractStep v]
  -> HistoryRoot
  -> Either String HistoryRoot
addStepsToRoot version block steps (HistoryRoot version' rootBlock create rootSteps rolledBackBlocks next) = first ("addStepsToRoot: " <>) do
  Refl <- note "version mismatch" $ testEquality version version'
  result <- extendTree rootBlock next
  pure case result of
    Left rootSteps' -> HistoryRoot version' rootBlock create (rootSteps <> rootSteps') rolledBackBlocks Nothing
    Right next'     -> HistoryRoot version' rootBlock create rootSteps rolledBackBlocks $ Just next'
  where
    extendTree :: BlockHeader -> Maybe (HistoryTree v) -> Either String (Either [ContractStep v] (HistoryTree v))
    extendTree fromBlock = \case
      Nothing -> case compare block fromBlock of
        LT -> Left "adding steps to previous block"
        EQ -> Right $ Left steps
        GT -> Right $ Right $ HistoryTree block steps mempty Nothing
      Just (HistoryTree fromBlock' steps' rolledBackBlocks' next') -> case extendTree fromBlock' next' of
        Left msg -> Left msg
        Right (Left steps'') ->
          Right $ Right $ HistoryTree fromBlock' (steps' <> steps'') rolledBackBlocks' Nothing
        Right (Right next'') ->
          Right $ Right $ HistoryTree fromBlock' steps' rolledBackBlocks' $ Just next''

-- | Roll back all contracts in the store to the given point.
rollback :: ChainPoint -> HistoryStoreModel -> HistoryStoreModel
rollback = \case
  Genesis    -> const emptyHistoryStore
  At toBlock -> HistoryStoreModel . Map.mapMaybe (rollbackRoot toBlock) . unHistoryStoreModel

-- | Roll back a 'HistoryRoot' to the given point.
rollbackRoot :: BlockHeader -> HistoryRoot -> Maybe HistoryRoot
rollbackRoot toBlock (HistoryRoot version block create steps rolledBackBlocks next) = do
  guard $ toBlock >= block
  pure case rollbackTree toBlock <$> next of
    Nothing -> HistoryRoot version block create steps rolledBackBlocks Nothing
    Just (Left rolledBackBlocks') -> HistoryRoot version block create steps (rolledBackBlocks <> rolledBackBlocks') Nothing
    Just (Right next') -> HistoryRoot version block create steps rolledBackBlocks $ Just next'

-- | Roll back a 'HistoryTree' to a given point. If the rollback point is after
-- this tree, returns the set of 'BlockHeaders' referenced by the tree and its
-- descendants.
rollbackTree :: BlockHeader -> HistoryTree v -> Either (Set BlockHeader) (HistoryTree v)
rollbackTree toBlock (HistoryTree block steps rolledBackBlocks next) = case nextRolledBack of
  Left rolledBackBlocks'
    | toBlock >= block -> Right $ HistoryTree block steps rolledBackBlocks' Nothing
    | otherwise -> Left rolledBackBlocks'
  Right next' -> Right $ HistoryTree block steps rolledBackBlocks $ Just next'
  where
    nextRolledBack = first (rolledBackBlocks <>) case rollbackTree toBlock <$> next of
      Nothing                       -> Left mempty
      Just (Left rolledBackBlocks') -> Left rolledBackBlocks'
      Just (Right next')            -> Right next'

newtype EndoKleisli m a = EndoKleisli { runEndoKleisli :: a -> m a }

instance Monad m => Semigroup (EndoKleisli m a) where
  a <> b = EndoKleisli $ runEndoKleisli b <=< runEndoKleisli a

instance Monad m => Monoid (EndoKleisli m a) where
  mempty = EndoKleisli pure

commitChanges :: Map ContractId UpdateContract -> HistoryStoreModel -> Either String HistoryStoreModel
commitChanges updates = runEndoKleisli $ fold
  [ foldMap (uncurry handleUpdate . swap) $ Map.toList updates
  , EndoKleisli $ Right . handleRollbacks updates
  ]

handleUpdate :: UpdateContract -> ContractId -> EndoKleisli (Either String) HistoryStoreModel
handleUpdate = \case
  RemoveContract -> EndoKleisli . removeContract
  UpdateContract (SomeContractChanges version ContractChanges{..}) -> \contractId ->
    fold
      [ fold $ uncurry (handleCreate contractId version) <$> create
      , fold $ uncurry (handleSteps contractId version) <$> Map.toAscList steps
      ]

handleRollbacks :: Map ContractId UpdateContract -> HistoryStoreModel -> HistoryStoreModel
handleRollbacks updates =
  let
    rollbackPoint = getMin <$> foldMap getRollback updates
  in
    maybe id rollback rollbackPoint

getRollback :: UpdateContract -> Maybe (Min ChainPoint)
getRollback = \case
  RemoveContract                                             -> Nothing
  UpdateContract (SomeContractChanges _ ContractChanges{..}) -> Min <$> rollbackTo

-- TODO hoist rollback handling to be done over all contracts instead of
-- per-contract
rollbackContract
  :: ContractId
  -> BlockHeader
  -> Map ContractId HistoryRoot
  -> Either String (Map ContractId HistoryRoot)
rollbackContract contractId toBlock store = do
  root <- note "rollbackContract: contract not found" $ Map.lookup contractId store
  pure case rollbackRoot toBlock root of
    Nothing    -> Map.delete contractId store
    Just root' -> Map.insert contractId root' store

handleCreate
  :: ContractId
  -> MarloweVersion v
  -> BlockHeader
  -> CreateStep v
  -> EndoKleisli (Either String) HistoryStoreModel
handleCreate contractId version createBlock createStep =
  EndoKleisli $ addContract contractId version createBlock createStep

handleSteps
  :: ContractId
  -> MarloweVersion v
  -> BlockHeader
  -> [ContractStep v]
  -> EndoKleisli (Either String) HistoryStoreModel
handleSteps contractId version block steps = EndoKleisli
  $ addSteps contractId version block steps

findCreateStep
  :: ContractId
  -> HistoryStoreModel
  -> Maybe (BlockHeader, SomeCreateStep)
findCreateStep contractId (HistoryStoreModel store) = do
  HistoryRoot version block create _ _ _ <- Map.lookup contractId store
  pure (block, SomeCreateStep version create)

findIntersection
  :: ContractId
  -> [BlockHeader]
  -> HistoryStoreModel
  -> Maybe Intersection
findIntersection contractId blocks (HistoryStoreModel store) = do
  root@(HistoryRoot version _ _ _ _ _) <- Map.lookup contractId store
  fmap (Intersection version . fst)
    $ Set.maxView
    $ Set.intersection (Set.fromList blocks)
    $ getRootBlocks root

findNextSteps
  :: ContractId
  -> ChainPoint
  -> HistoryStoreModel
  -> FindNextStepsResponse
findNextSteps contractId afterPoint (HistoryStoreModel store) = case Map.lookup contractId store of
  Nothing -> FindRollback Genesis
  Just (HistoryRoot version block _ steps rolledBackBlocks next) -> case (trimPoint block afterPoint, steps) of
    (Genesis, [])      -> findNextSteps' block version $ HistoryTree block steps rolledBackBlocks next
    (Genesis, _)       -> FindNext block (SomeContractSteps version steps)
    (At afterBlock, _) -> findNextSteps' afterBlock version $ HistoryTree block steps rolledBackBlocks next
  where
    trimPoint block point
      | point < At block = Genesis
      | otherwise = point
    findNextSteps' afterBlock version (HistoryTree block steps rolledBackBlocks next)
      | afterBlock < block = FindNext block (SomeContractSteps version steps)
      | Set.member afterBlock rolledBackBlocks = FindRollback $ At block
      | otherwise = maybe (FindWait block) (findNextSteps' afterBlock version) next

getRoots :: HistoryStoreModel -> [(ContractId, HistoryRoot)]
getRoots = Map.toList . unHistoryStoreModel

getAllBlocks :: HistoryStoreModel -> [BlockHeader]
getAllBlocks = Set.toAscList . foldMap getAllRootBlocks . unHistoryStoreModel

getBlocks :: HistoryStoreModel -> [BlockHeader]
getBlocks = Set.toAscList . foldMap getRootBlocks . unHistoryStoreModel

getAllRootBlocks :: HistoryRoot -> Set BlockHeader
getAllRootBlocks (HistoryRoot _ block _ _ rolledBackBlocks next) =
  Set.insert block $ rolledBackBlocks <> foldMap getAllTreeBlocks next

getRootBlocks :: HistoryRoot -> Set BlockHeader
getRootBlocks (HistoryRoot _ block _ _ _ next) = Set.insert block $ foldMap getTreeBlocks next

getAllTreeBlocks :: HistoryTree v -> Set BlockHeader
getAllTreeBlocks (HistoryTree block _ rolledBackBlocks next) =
  Set.insert block $ rolledBackBlocks <> foldMap getTreeBlocks next

getTreeBlocks :: HistoryTree v -> Set BlockHeader
getTreeBlocks (HistoryTree block _ _ next) = Set.insert block $ foldMap getTreeBlocks next

getTip :: HistoryStoreModel -> ChainPoint
getTip = foldr (max . getRootTip) Genesis . unHistoryStoreModel

getRootTip :: HistoryRoot -> ChainPoint
getRootTip (HistoryRoot _ block _ _ _ Nothing) = At block
getRootTip (HistoryRoot _ _ _ _ _ (Just next)) = getTreeTip next

getTreeTip :: HistoryTree v -> ChainPoint
getTreeTip (HistoryTree block _ _ Nothing) = At block
getTreeTip (HistoryTree _ _ _ (Just next)) = getTreeTip next
