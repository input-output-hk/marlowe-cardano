{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}

module Language.Marlowe.Runtime.History.Script where

import Control.Monad (foldM, replicateM, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT (..), get, put)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as BS
import Data.Data (type (:~:) (Refl))
import Data.Foldable (fold)
import Data.GADT.Show (GShow (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Some (Some (Some), withSome)
import Data.Type.Equality (testEquality)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), Assets (..), BlockHeader (..), BlockHeaderHash (..),
                                               Lovelace (..), PolicyId (..), SlotNo (..), TokenName (..), Tokens (..),
                                               TxId (..), TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as P
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Arbitrary (arbitraryValidStep)
import Test.QuickCheck (Gen, arbitrary, frequency, getSize, resize)
import Test.QuickCheck.Gen (chooseWord64)

-- An event in a history test script
data HistoryScriptEvent v
  -- Extend the chain with a new block
  = RollForward SlotNo BlockHeaderHash
  -- Retract the given number of blocks from the chain.
  | RollBackward Natural
  -- Create a new contract in the current block
  | CreateContract ContractId (MarloweVersion v) (CreateStep v)
  -- Apply Inputs to a contract in the current block
  | ApplyInputs ContractId TxId (MarloweVersion v) P.POSIXTime P.POSIXTime (Redeemer v)
  -- Withdraw funds from a contract in the current block
  | Withdraw ContractId (MarloweVersion v) (RedeemStep v)

deriving instance Show (HistoryScriptEvent 'V1)
deriving instance Eq (HistoryScriptEvent 'V1)

instance GShow HistoryScriptEvent where
  gshowsPrec p = \case
    RollForward s b                             -> showsPrec p (RollForward s b :: HistoryScriptEvent 'V1)
    RollBackward n                              -> showsPrec p (RollBackward n :: HistoryScriptEvent 'V1)
    CreateContract cid MarloweV1 cs             -> showsPrec p $ CreateContract cid MarloweV1 cs
    ApplyInputs cid txId MarloweV1 vLow vHigh r -> showsPrec p $ ApplyInputs cid txId MarloweV1 vLow vHigh r
    Withdraw cid MarloweV1 rs                   -> showsPrec p $ Withdraw cid MarloweV1 rs

newtype HistoryScript = HistoryScript { unHistoryScript :: [Some HistoryScriptEvent] }
  deriving (Show)

-- A list of state snapshots in descending block order
type HistoryScriptState = [HistoryScriptBlockState]

-- An aggregate state snapshot at a particular block
data HistoryScriptBlockState = HistoryScriptBlockState
  { contractStates :: Map ContractId HistoryScriptContractState
  , block          :: BlockHeader
  }

-- A state snapshot for a particular contract
data HistoryScriptContractState = forall v. HistoryScriptContractState
  { contractVersion   :: MarloweVersion v
  , statePayouts      :: Map TxOutRef (Payout v)
  , stateScriptOutput :: Maybe (TransactionScriptOutput v)
  }

data ReductionError
  = EmptyChain
  | ContractExists ContractId
  | ContractNotFound ContractId
  | ContractClosed ContractId
  | PayoutNotFound ContractId TxOutRef
  | forall v1 v2. VersionMismatch ContractId (MarloweVersion v1) (MarloweVersion v2)
  | V1TransactionError V1.TransactionError

foldScript :: HistoryScript -> Either ReductionError HistoryScriptState
foldScript = foldM (\state (Some event) -> reduceScriptEvent event state) [] . unHistoryScript

reduceScriptEvent :: HistoryScriptEvent v -> HistoryScriptState -> Either ReductionError HistoryScriptState
reduceScriptEvent = \case
  RollForward slotOffset headerHash -> \case
    states@(HistoryScriptBlockState{..} : _) ->
      let
        BlockHeader{slotNo,blockNo} = block
        blockState = HistoryScriptBlockState
          { contractStates
          , block = BlockHeader
            { slotNo = slotOffset + slotNo
            , headerHash
            , blockNo = blockNo + 1
            }
          }
      in
        Right $ blockState : states
    [] -> Right [HistoryScriptBlockState mempty $ BlockHeader slotOffset headerHash 0]
  RollBackward n -> Right . drop (fromIntegral n)
  CreateContract contractId version CreateStep{..} ->
    \case
      []                              -> Left EmptyChain
      state@HistoryScriptBlockState{..} : states
        | Map.member contractId contractStates -> Left $ ContractExists contractId
        | otherwise ->
            let
              contractState = HistoryScriptContractState
                { contractVersion = version
                , statePayouts = mempty
                , stateScriptOutput = Just TransactionScriptOutput
                    { utxo = unContractId contractId
                    , datum
                    }
                }
              state' = state { contractStates = Map.insert contractId contractState contractStates }
            in
              Right $ state' : states
  ApplyInputs contractId txId version vLow vHigh redeemer ->
    \case
      []                              -> Left EmptyChain
      state@HistoryScriptBlockState{..} : states -> case Map.lookup contractId contractStates of
        Nothing -> Left $ ContractNotFound contractId
        Just HistoryScriptContractState{..} -> case testEquality version contractVersion of
          Nothing   -> Left $ VersionMismatch contractId version contractVersion
          Just Refl -> do
            datum <- case stateScriptOutput of
              Nothing                          -> Left $ ContractClosed contractId
              Just TransactionScriptOutput{..} -> Right datum
            TransactionOutput{..} <- applyInputs version vLow vHigh txId redeemer datum
            let
              state' = state
                { contractStates = Map.insert
                    contractId
                    HistoryScriptContractState
                      { contractVersion
                      , statePayouts = Map.union statePayouts payouts
                      , stateScriptOutput = scriptOutput
                      }
                    contractStates
                }
            pure $ state' : states
  Withdraw contractId version RedeemStep{..} ->
    \case
      []                              -> Left EmptyChain
      state@HistoryScriptBlockState{..} : states -> case Map.lookup contractId contractStates of
        Nothing -> Left $ ContractNotFound contractId
        Just HistoryScriptContractState{..} -> case testEquality version contractVersion of
          Nothing   -> Left $ VersionMismatch contractId version contractVersion
          Just Refl -> do
            case Map.lookup utxo statePayouts of
              Nothing -> Left $ PayoutNotFound contractId utxo
              Just _ ->
                let
                  contractState' = HistoryScriptContractState
                    { contractVersion
                    , stateScriptOutput
                    , statePayouts = Map.delete utxo statePayouts
                    }
                  state' = state { contractStates = Map.insert contractId contractState' contractStates }
                in
                  Right $ state' : states

applyInputs :: MarloweVersion v -> P.POSIXTime -> P.POSIXTime -> TxId -> Redeemer v -> Datum v -> Either ReductionError (TransactionOutput v)
applyInputs = \case
  MarloweV1 -> applyInputsV1

applyInputsV1 :: P.POSIXTime -> P.POSIXTime -> TxId -> [V1.Input] -> V1.MarloweData -> Either ReductionError (TransactionOutput 'V1)
applyInputsV1 vLow vHigh txId txInputs V1.MarloweData{..} =
  case V1.computeTransaction V1.TransactionInput{..} marloweState marloweContract of
    V1.Error err -> Left $V1TransactionError err
    V1.TransactionOutput{..} -> Right $ TransactionOutput
      { payouts = Map.fromList $ zip (TxOutRef txId <$> [1..]) $ mapMaybe paymentToPayout txOutPayments
      , scriptOutput = case txOutContract of
          V1.Close -> Nothing
          _ -> Just TransactionScriptOutput
            { utxo = TxOutRef txId 0
            , datum = V1.MarloweData txOutState txOutContract
            }
      }
  where
    txInterval = (vLow, vHigh)
    paymentToPayout :: V1.Payment -> Maybe (Payout 'V1)
    paymentToPayout (V1.Payment _ (V1.Party (V1.Role role)) money) = Just $ Payout
      { assets = moneyToAssets money
      , datum = fromPlutusTokenName role
      }
    paymentToPayout _                                              = Nothing

    moneyToAssets :: V1.Money -> Assets
    moneyToAssets = Assets <$> moneyToLovelace <*> moneyToTokens

    moneyToLovelace :: V1.Money -> Lovelace
    moneyToLovelace = Lovelace . maybe 0 fromIntegral . (AM.lookup "" <=< AM.lookup "") . P.getValue

    moneyToTokens :: V1.Money -> Tokens
    moneyToTokens = Tokens
      . Map.fromList
      . fmap
          ( bimap (uncurry AssetId . bimap fromPlutusCurrencySymbol fromPlutusTokenName) fromIntegral
          . assocLeft
          )
      . (traverse AM.toList <=< AM.toList)
      . AM.delete ""
      . P.getValue

    assocLeft (a, (b, c)) = ((a, b), c)

    fromPlutusTokenName :: P.TokenName -> TokenName
    fromPlutusTokenName = TokenName . P.fromBuiltin . P.unTokenName

    fromPlutusCurrencySymbol :: P.CurrencySymbol -> PolicyId
    fromPlutusCurrencySymbol = PolicyId . P.fromBuiltin . P.unCurrencySymbol

genHistoryScript :: StateT HistoryScriptState Gen [Some HistoryScriptEvent]
genHistoryScript = do
  states <- get
  event <- lift case states of
    []        -> genRollForward
    state : _ -> genHistoryScriptEvent state
  put case withSome event $ flip reduceScriptEvent states of
    Left _        -> error "Failed to reduce script event while generating script"
    Right states' -> states'
  (event :) <$> StateT \state -> do
    size <- getSize
    resize (size - 1) $ runStateT genHistoryScript state

genHistoryScriptEvent :: HistoryScriptBlockState -> Gen (Some HistoryScriptEvent)
genHistoryScriptEvent HistoryScriptBlockState{..} = frequency $ fold
  [ blockEvents
  , (fmap (2,) . uncurry contractEvents) =<< Map.toList contractStates
  , [(5, genCreateContract slotNo)]
  ]
  where
    BlockHeader{..} = block
    blockEvents = [(5, genRollForward), (1, genRollBackward)]
    contractEvents contractId HistoryScriptContractState{..} = (fmap . fmap) Some $ fold
      [ maybeToList $ genApplyInputs contractVersion slotNo contractId <$> stateScriptOutput
      , uncurry (genWithdraw contractVersion contractId) <$> Map.toList statePayouts
      ]

genRollForward :: Gen (Some HistoryScriptEvent)
genRollForward = do
  slots <- SlotNo <$> chooseWord64 (1, 120)
  header <- BlockHeaderHash . BS.pack <$> replicateM 64 arbitrary
  pure $ Some $ RollForward slots header

genRollBackward :: Gen (Some HistoryScriptEvent)
genRollBackward = Some . RollBackward <$> frequency
  [ (1, pure 7)
  , (1, pure 6)
  , (2, pure 5)
  , (3, pure 4)
  , (5, pure 3)
  , (8, pure 2)
  , (13, pure 1)
  ]

genCreateContract :: SlotNo -> Gen (Some HistoryScriptEvent)
genCreateContract slot = do
  contractId <- ContractId . flip TxOutRef 0 <$> genTxId
  datum <- V1.MarloweData (V1.emptyState $ fromIntegral slot) <$> arbitrary
  let scriptAddress = "7062c56ccfc6217aff5692e1d3ebe89c21053d31fc11882cb21bfdd307"
  let payoutValidatorHash = "a2c56ccfc6217aff5692e1d3ebe89c21053d31fc11882cb21bfdd307"
  pure $ Some $ CreateContract contractId MarloweV1 CreateStep{..}

genTxId :: Gen TxId
genTxId = TxId . BS.pack <$> replicateM 64 arbitrary

genApplyInputs :: MarloweVersion v -> SlotNo -> ContractId -> TransactionScriptOutput v -> Gen (HistoryScriptEvent v)
genApplyInputs = \case
  MarloweV1 -> genApplyInputsV1

genApplyInputsV1 :: SlotNo -> ContractId -> TransactionScriptOutput 'V1 -> Gen (HistoryScriptEvent 'V1)
genApplyInputsV1 slot contractId TransactionScriptOutput{..} = do
  let V1.MarloweData{..} = datum
  let minTime = fromIntegral slot
  V1.TransactionInput (vLow, vHigh) inputs <- arbitraryValidStep marloweState {V1.minTime=minTime} marloweContract
  txId <- genTxId
  pure $ ApplyInputs contractId txId MarloweV1 vLow vHigh inputs

genWithdraw :: MarloweVersion v -> ContractId -> TxOutRef -> Payout v -> Gen (HistoryScriptEvent v)
genWithdraw = \case
  MarloweV1 -> genWithdrawV1

genWithdrawV1 :: ContractId -> TxOutRef -> Payout 'V1 -> Gen (HistoryScriptEvent 'V1)
genWithdrawV1 contractId utxo Payout{..} = do
  redeemingTx <- genTxId
  pure $ Withdraw contractId MarloweV1 RedeemStep{..}
