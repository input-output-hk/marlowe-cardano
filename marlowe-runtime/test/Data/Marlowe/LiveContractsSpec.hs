{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}

module Data.Marlowe.LiveContractsSpec where

import Control.Arrow (Arrow (..))
import Control.Monad (guard, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, evalStateT, state)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Marlowe.LiveContracts
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Float (sqrtDouble)
import Language.Marlowe.Core.V1.Semantics (MarloweData (..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), BlockNo, TxId (..), TxOutRef (..), WithGenesis (..))
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweVersion (..),
  SomeMarloweVersion (..),
  Transaction (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
  emptyMarloweTransactionMetadata,
 )
import Language.Marlowe.Runtime.History.Api (
  CreateStep (..),
  MarloweApplyInputsTransaction (..),
  MarloweBlock (..),
  MarloweCreateTransaction (..),
  SomeCreateStep (SomeCreateStep),
  UnspentContractOutput (..),
 )
import Language.Marlowe.Runtime.History.Gen ()
import Language.Marlowe.Util (dataHash)
import PlutusLedgerApi.V1 (BuiltinByteString, fromBuiltin, toBuiltin)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  prop "arbitrary generates valid" validate
  prop "shrink valid is valid" $ traverse_ validate . shrink

  describe "create" do
    prop "returns valid on valid" $ validate . create . abs

  describe "rollBackward" do
    prop "returns valid on valid" \liveContracts ->
      case getValidRollbackState liveContracts of
        Nothing -> discard
        Just validState -> forAll (genRollBackwardArgs validState) \(point, tip) ->
          validate $ rollBackward point tip liveContracts

    prop "rollBackward currentPoint lastTip = id" \liveContracts ->
      let (currentPoint, tip) = case latestPoints liveContracts of
            Left t -> (Genesis, t)
            Right (point, t) -> (At point, At t)
       in areValidRollbackArgs liveContracts currentPoint tip ==>
            rollBackward currentPoint tip liveContracts
              === liveContracts

    prop "rollBackward . rollForward preserves current live" \liveContracts ->
      let (currentPoint, currentTip) = case latestPoints liveContracts of
            Left t -> (Genesis, t)
            Right (point, t) -> (At point, At t)
       in canRollForward liveContracts ==>
            forAll (genRollForwardArgs liveContracts) \(tip, block) ->
              let liveContracts' = rollForward tip block liveContracts
               in areValidRollbackArgs liveContracts' currentPoint (At tip) ==>
                    currentLive liveContracts
                      === currentLive (rollBackward currentPoint currentTip liveContracts')

    prop "rollBackward Genesis empties history" \liveContracts ->
      rollBackward Genesis Genesis liveContracts === create (securityParameter liveContracts)

  describe "rollForward" do
    prop "returns valid on valid" \liveContracts ->
      canRollForward liveContracts ==>
        forAll (genRollForwardArgs liveContracts) \(tip, block) ->
          validate $ rollForward tip block liveContracts

    prop "rollForward contains all added contracts" \liveContracts ->
      canRollForward liveContracts ==>
        forAll (genRollForwardArgs liveContracts) \(tip, block) ->
          let rolledForward = rollForward tip block liveContracts
           in counterexample (show rolledForward) $
                Set.difference (blockAddedContracts block) (membersSet $ currentLive rolledForward)
                  === mempty

    prop "rollForward only removes consumed contracts" \liveContracts ->
      canRollForward liveContracts ==>
        forAll (genRollForwardArgs liveContracts) \(tip, block) -> do
          let rolledForward = rollForward tip block liveContracts
          let oldLiveKeys = Map.keysSet $ currentLive liveContracts
          let newLiveKeys = Map.keysSet $ currentLive rolledForward
          let consumedFromOld = Set.intersection oldLiveKeys $ blockConsumedContracts block
          counterexample (show rolledForward) $
            Set.difference oldLiveKeys newLiveKeys === consumedFromOld

    prop "rollForward past security parameter removes all live contracts" \liveContracts ->
      canRollForwardBy (fromIntegral $ securityParameter liveContracts) liveContracts ==>
        forAll (genPointBeyondSecurityParam liveContracts) \point -> do
          let blockHeader = BlockHeader (fromIntegral point) "" point
          let applies = do
                txOutRef <- Map.keys $ currentLive liveContracts
                pure
                  MarloweApplyInputsTransaction
                    { marloweVersion = MarloweV1
                    , marloweInput =
                        UnspentContractOutput
                          { marloweVersion = SomeMarloweVersion MarloweV1
                          , txOutRef
                          , marloweAddress = ""
                          , payoutValidatorHash = ""
                          }
                    , marloweTransaction =
                        Transaction
                          { transactionId = ""
                          , contractId = ContractId txOutRef
                          , metadata = emptyMarloweTransactionMetadata
                          , blockHeader
                          , validityLowerBound = posixSecondsToUTCTime 0
                          , validityUpperBound = posixSecondsToUTCTime 0
                          , inputs = []
                          , output = TransactionOutput mempty Nothing
                          }
                    }
          let block = MarloweBlock blockHeader [] applies []
          let rolledForward = rollForward point block liveContracts
          counterexample (show rolledForward) $
            allContracts rolledForward === mempty

genPointBeyondSecurityParam :: LiveContracts -> Gen BlockNo
genPointBeyondSecurityParam lc = case latestPoints lc of
  Left _ -> chooseBoundedIntegral (fromIntegral (securityParameter lc), maxBound)
  Right (point, _) -> chooseBoundedIntegral (point + fromIntegral (securityParameter lc), maxBound)

blockConsumedContracts :: MarloweBlock -> Set TxOutRef
blockConsumedContracts = foldMap (Set.singleton . txOutRef . marloweInput) . applyInputsTransactions

blockAddedContracts :: MarloweBlock -> Set (TxOutRef, Contract)
blockAddedContracts block =
  Set.fromDistinctAscList
    . Map.toAscList
    . flip Map.withoutKeys (blockConsumedContracts block)
    . Map.union (blockNewApplyContracts block)
    . blockNewCreateContracts
    $ block

blockNewApplyContracts :: MarloweBlock -> Map TxOutRef Contract
blockNewApplyContracts = foldMap extractNewContracts . applyInputsTransactions
  where
    extractNewContracts :: MarloweApplyInputsTransaction -> Map TxOutRef Contract
    extractNewContracts (MarloweApplyInputsTransaction MarloweV1 _ Transaction{..}) =
      case output of
        TransactionOutput _ (Just (TransactionScriptOutput _ _ utxo (MarloweData _ _ contract))) ->
          Map.singleton utxo contract
        _ -> mempty

blockNewCreateContracts :: MarloweBlock -> Map TxOutRef Contract
blockNewCreateContracts = foldMap extractNewContracts . createTransactions
  where
    extractNewContracts MarloweCreateTransaction{..} = Map.fromDistinctAscList @TxOutRef @Contract do
      (txIx, SomeCreateStep MarloweV1 CreateStep{..}) <- Map.toAscList newContracts
      pure (TxOutRef txId txIx, marloweContract $ datum createOutput)

membersSet :: Map.Map k v -> Set (k, v)
membersSet = Set.fromDistinctAscList . Map.toAscList

instance Arbitrary LiveContracts where
  arbitrary = genLiveContracts
  shrink = shrinkLiveContracts shrink

genLiveContracts :: Gen LiveContracts
genLiveContracts = sized \size ->
  if size == 0
    then go size
    else do
      let sqrtSize = round $ sqrtDouble $ fromIntegral size
      noSteps <- if size == 0 then pure 0 else chooseInt (1, sqrtSize)
      let stepSize
            | size == 0 = 0
            | otherwise = size `div` noSteps
      resize stepSize $ go noSteps
  where
    go 0 = create <$> chooseInt (1, 256)
    go remaining = do
      liveContracts <- go $ remaining - 1
      oneof $
        catMaybes
          [ Just $ pure liveContracts
          , guard (canRollForward liveContracts) $> do
              (tip, block) <- genRollForwardArgs liveContracts
              pure $ rollForward tip block liveContracts
          , do
              validState <- getValidRollbackState liveContracts
              pure $ do
                (point, tip) <- genRollBackwardArgs validState
                pure $ rollBackward point tip liveContracts
          ]

genRollForwardArgs :: LiveContracts -> Gen (BlockNo, MarloweBlock)
genRollForwardArgs liveContracts = do
  let maxTipChange = max 1 $ fromIntegral (securityParameter liveContracts) * 2
  (nextBlockNo, nextTip) <- case latestPoints liveContracts of
    Left Genesis -> do
      nextTip <- chooseBoundedIntegral (0, maxTipChange)
      nextBlockNo <- chooseBoundedIntegral (0, nextTip)
      pure (nextBlockNo, nextTip)
    Left (At tip) -> do
      nextTip <- chooseBoundedIntegral (tip + 1, tip + maxTipChange)
      nextBlockNo <- chooseBoundedIntegral (0, nextTip)
      pure (nextBlockNo, nextTip)
    Right (point, tip) -> do
      nextTip <- chooseBoundedIntegral (tip + 1, tip + maxTipChange)
      nextBlockNo <- chooseBoundedIntegral (point + 1, nextTip)
      pure (nextBlockNo, nextTip)
  nextBlockHeader <- BlockHeader (fromIntegral nextBlockNo) <$> arbitrary <*> pure nextBlockNo
  size <- getSize
  let sqrtSize = round $ sqrtDouble $ fromIntegral size
  noCreates <- if size == 0 then pure 0 else chooseInt (1, sqrtSize)
  let createContractSize
        | size == 0 = 0
        | otherwise = size `div` noCreates
  let prevTxOuts = Map.keysSet $ currentLive liveContracts
  let utxoSeed = dataHash $ hashTxOutRef <$> Set.toAscList prevTxOuts
  createTxs <- resize createContractSize $ evalStateT (replicateM noCreates genCreateTransaction) utxoSeed
  let txOutsAfterCreates = Set.union prevTxOuts $ foldMap createTxOutRefs createTxs
  block <-
    MarloweBlock nextBlockHeader createTxs
      <$> genApplyInputsTransactions nextBlockHeader txOutsAfterCreates
      <*> arbitrary
  pure (nextTip, block)

genCreateTransaction :: StateT BuiltinByteString Gen MarloweCreateTransaction
genCreateTransaction = do
  MarloweCreateTransaction{..} <- lift arbitrary
  nextSeed <- state \seed ->
    (id &&& id) $
      dataHash @(_, [MarloweData])
        ( seed
        , do
            SomeCreateStep MarloweV1 CreateStep{..} <- Map.elems newContracts
            pure $ datum createOutput
        )
  let txId' = TxId $ fromBuiltin nextSeed
  pure $
    MarloweCreateTransaction
      { txId = txId'
      , newContracts = flip Map.mapWithKey newContracts \txIx -> \case
          SomeCreateStep MarloweV1 CreateStep{..} ->
            SomeCreateStep
              MarloweV1
              CreateStep
                { createOutput = case createOutput of
                    TransactionScriptOutput{..} ->
                      TransactionScriptOutput
                        { utxo = TxOutRef txId' txIx
                        , ..
                        }
                , ..
                }
      }

hashTxOutRef :: TxOutRef -> BuiltinByteString
hashTxOutRef TxOutRef{..} = dataHash (toBuiltin $ unTxId txId, fromIntegral @_ @Integer txIx)

genRollBackwardArgs :: ValidRollbackState -> Gen (WithGenesis BlockNo, WithGenesis BlockNo)
genRollBackwardArgs ValidRollbackState{..} = do
  nextPoint <- case rollbackPointBounds of
    RollbackGenesisGenesis -> pure Genesis
    RollbackGenesisBlock maxPoint ->
      frequency
        [ (1, pure Genesis)
        , (fromIntegral maxPoint, At <$> chooseBoundedIntegral (0, maxPoint))
        ]
    RollbackBlockBlock minPoint maxPoint -> At <$> chooseBoundedIntegral (minPoint, maxPoint)
  nextTip <- chooseBoundedIntegral (minTip, minTip + 10)
  pure (nextPoint, At nextTip)

genApplyInputsTransactions :: BlockHeader -> Set TxOutRef -> Gen [MarloweApplyInputsTransaction]
genApplyInputsTransactions currentBlockHeader utxo = do
  size <- getSize
  let sqrtSize = round $ sqrtDouble $ fromIntegral size
  noTxs <- if size == 0 then pure 0 else choose (1, sqrtSize)
  let txSize
        | size == 0 = 0
        | otherwise = size `div` noTxs
  resize txSize $ go utxo noTxs
  where
    go utxo' remaining
      | Set.null utxo' || remaining == 0 = pure []
      | otherwise = do
          MarloweApplyInputsTransaction MarloweV1 UnspentContractOutput{..} Transaction{..} <- arbitrary
          let utxoSeed = dataHash $ hashTxOutRef <$> Set.toAscList utxo'
          inputTxOutRef <- elements $ Set.toList utxo'
          let (newTxOutRef, newOutput, transactionId') = case output of
                TransactionOutput{scriptOutput = Just TransactionScriptOutput{utxo = TxOutRef{txIx}, ..}, ..} -> do
                  let txId = TxId $ fromBuiltin $ dataHash (utxoSeed, Just datum)
                  let txOutRef' =
                        TxOutRef
                          txId
                          txIx
                  ( Just txOutRef'
                    , TransactionOutput
                        { scriptOutput = Just TransactionScriptOutput{utxo = txOutRef', ..}
                        , ..
                        }
                    , txId
                    )
                _ ->
                  (Nothing, output, TxId $ fromBuiltin $ dataHash (utxoSeed, Nothing @MarloweData))
          let tx =
                MarloweApplyInputsTransaction
                  MarloweV1
                  UnspentContractOutput{txOutRef = inputTxOutRef, ..}
                  Transaction
                    { blockHeader = currentBlockHeader
                    , output = newOutput
                    , transactionId = transactionId'
                    , ..
                    }
          let utxo'' = maybe id Set.insert newTxOutRef $ Set.delete inputTxOutRef utxo'
          (tx :) <$> go utxo'' (remaining - 1)

createTxOutRefs :: MarloweCreateTransaction -> Set TxOutRef
createTxOutRefs MarloweCreateTransaction{..} = Set.mapMonotonic (TxOutRef txId) $ Map.keysSet newContracts

canRollForward :: LiveContracts -> Bool
canRollForward = canRollForwardBy 1

canRollForwardBy :: BlockNo -> LiveContracts -> Bool
canRollForwardBy delta lc = case latestPoints lc of
  Left _ -> True
  Right (point, _) -> maxBound - point >= delta

data RollbackPointBounds
  = RollbackGenesisGenesis
  | RollbackGenesisBlock BlockNo
  | RollbackBlockBlock BlockNo BlockNo

data ValidRollbackState = ValidRollbackState
  { rollbackPointBounds :: RollbackPointBounds
  , minTip :: BlockNo
  }

getValidRollbackState :: LiveContracts -> Maybe ValidRollbackState
getValidRollbackState lc = case latestPoints lc of
  Left _ -> Nothing
  Right (point, tip) -> do
    let minRollbackPointInteger = fromIntegral tip - fromIntegral (securityParameter lc)
    guard $ minRollbackPointInteger < fromIntegral point
    let rollbackPointBounds =
          if minRollbackPointInteger < 0
            then
              if point == 0
                then RollbackGenesisGenesis
                else RollbackGenesisBlock $ point - 1
            else RollbackBlockBlock (fromInteger minRollbackPointInteger) $ point - 1
    let minTip = tip
    pure ValidRollbackState{..}

areValidRollbackArgs :: LiveContracts -> WithGenesis BlockNo -> WithGenesis BlockNo -> Bool
areValidRollbackArgs _ _ Genesis = False
areValidRollbackArgs _ Genesis _ = True
areValidRollbackArgs lc (At point) (At tip) = case latestPoints lc of
  Left Genesis -> False
  Left _ -> True
  Right (point', _) ->
    point' <= point
      && fromIntegral @_ @Integer point' > fromIntegral tip - fromIntegral (securityParameter lc)
