{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.History.Script
  where

import Control.Monad (foldM, replicateM, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT, get, put)
import Data.Bifunctor (Bifunctor(bimap))
import qualified Data.ByteString as BS
import Data.Data (type (:~:)(Refl))
import Data.Foldable (fold)
import Data.GADT.Compare (GEq, geq)
import Data.GADT.Show (GShow(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Some (Some(Some), withSome)
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Type.Equality (testEquality)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , AssetId(..)
  , Assets(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , Lovelace(..)
  , PolicyId(..)
  , ScriptHash(..)
  , SlotNo(..)
  , TokenName(..)
  , Tokens(..)
  , TxId(..)
  , TxOutRef(..)
  )
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as P
import qualified PlutusTx.AssocMap as AM
import Spec.Marlowe.Semantics.Arbitrary (arbitraryValidStep)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, frequency, getSize, resize)
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
  | ApplyInputs (MarloweVersion v) (Transaction v)
  -- Withdraw funds from a contract in the current block
  | Withdraw ContractId (MarloweVersion v) (RedeemStep v)

deriving instance Show (HistoryScriptEvent 'V1)
deriving instance Eq (HistoryScriptEvent 'V1)

instance GShow HistoryScriptEvent where
  gshowsPrec p = \case
    RollForward s b                 -> showsPrec p (RollForward s b :: HistoryScriptEvent 'V1)
    RollBackward n                  -> showsPrec p (RollBackward n :: HistoryScriptEvent 'V1)
    CreateContract cid MarloweV1 cs -> showsPrec p $ CreateContract cid MarloweV1 cs
    ApplyInputs MarloweV1 tx        -> showsPrec p $ ApplyInputs MarloweV1 tx
    Withdraw cid MarloweV1 rs       -> showsPrec p $ Withdraw cid MarloweV1 rs

newtype HistoryScript = HistoryScript { unHistoryScript :: [Some HistoryScriptEvent] }
  deriving (Show)

instance Arbitrary HistoryScript where
  arbitrary = HistoryScript <$> evalStateT genHistoryScript []

-- A list of state snapshots in descending block order
type HistoryScriptState = [HistoryScriptBlockState]

-- An aggregate state snapshot at a particular block
data HistoryScriptBlockState = HistoryScriptBlockState
  { contractStates :: Map ContractId (Some HistoryScriptContractState)
  , block          :: BlockHeader
  }
  deriving (Eq, Show)

-- A state snapshot for a particular contract
data HistoryScriptContractState v = HistoryScriptContractState
  { contractVersion   :: MarloweVersion v
  , statePayoutAddress :: Address
  , stateScriptAddress :: Address
  , statePayouts      :: Map TxOutRef (Payout v)
  , stateScriptOutput :: Maybe (TransactionScriptOutput v)
  }

deriving instance Eq (HistoryScriptContractState 'V1)
deriving instance Show (HistoryScriptContractState 'V1)

instance GEq HistoryScriptContractState where
  geq
    s1@HistoryScriptContractState{contractVersion=MarloweV1}
    s2@HistoryScriptContractState{contractVersion=MarloweV1}
      | s1 == s2 = Just Refl
      | otherwise = Nothing

instance GShow HistoryScriptContractState where
  gshowsPrec p s@HistoryScriptContractState{contractVersion=MarloweV1} = showsPrec p s

data ReductionError
  = EmptyChain
  | ContractExists ContractId
  | ContractNotFound ContractId
  | ContractClosed ContractId
  | PayoutNotFound ContractId TxOutRef
  | VersionMismatch ContractId SomeMarloweVersion SomeMarloweVersion
  | V1TransactionError V1.TransactionError
  deriving (Eq, Show)

foldHistoryScript :: HistoryScript -> Either ReductionError HistoryScriptState
foldHistoryScript = foldM (\state (Some event) -> reduceScriptEvent event state) [] . unHistoryScript

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
                , stateScriptOutput = Just createOutput
                , stateScriptAddress = let TransactionScriptOutput address _ _ _ = createOutput in address
                , statePayoutAddress = Address $ unAddress "70" <> unScriptHash payoutValidatorHash
                }
              state' = state { contractStates = Map.insert contractId (Some contractState) contractStates }
            in
              Right $ state' : states
  ApplyInputs version Transaction{..} ->
    \case
      []                              -> Left EmptyChain
      state@HistoryScriptBlockState{..} : states -> case Map.lookup contractId contractStates of
        Nothing -> Left $ ContractNotFound contractId
        Just (Some HistoryScriptContractState{..}) -> case testEquality version contractVersion of
          Nothing   -> Left $ VersionMismatch contractId (SomeMarloweVersion version) (SomeMarloweVersion contractVersion)
          Just Refl -> do
            let
              TransactionOutput{..} = output
              state' = state
                { contractStates = Map.insert
                    contractId
                    (Some HistoryScriptContractState
                      { contractVersion
                      , statePayouts = Map.union statePayouts payouts
                      , stateScriptOutput = scriptOutput
                      , stateScriptAddress
                      , statePayoutAddress
                      }
                    )
                    contractStates
                }
            pure $ state' : states
  Withdraw contractId version RedeemStep{..} ->
    \case
      []                              -> Left EmptyChain
      state@HistoryScriptBlockState{..} : states -> case Map.lookup contractId contractStates of
        Nothing -> Left $ ContractNotFound contractId
        Just (Some HistoryScriptContractState{..}) -> case testEquality version contractVersion of
          Nothing   -> Left $ VersionMismatch contractId (SomeMarloweVersion version) (SomeMarloweVersion contractVersion)
          Just Refl -> do
            case Map.lookup utxo statePayouts of
              Nothing -> Left $ PayoutNotFound contractId utxo
              Just _ ->
                let
                  contractState' = HistoryScriptContractState
                    { contractVersion
                    , stateScriptOutput
                    , statePayouts = Map.delete utxo statePayouts
                      , stateScriptAddress
                      , statePayoutAddress
                    }
                  state' = state { contractStates = Map.insert contractId (Some contractState') contractStates }
                in
                  Right $ state' : states

applyInputsV1 :: Address -> Address -> P.POSIXTime -> P.POSIXTime -> TxId -> [V1.Input] -> V1.MarloweData -> Maybe (TransactionOutput 'V1)
applyInputsV1 marloweAddress payoutAddress vLow vHigh txId txInputs V1.MarloweData{..} =
  case V1.computeTransaction V1.TransactionInput{..} marloweState marloweContract of
    V1.Error _ -> Nothing
    V1.TransactionOutput{..} -> Just $ TransactionOutput
      { payouts = Map.fromList $ zip (TxOutRef txId <$> [1..]) $ mapMaybe paymentToPayout txOutPayments
      , scriptOutput = case txOutContract of
          V1.Close -> Nothing
          _ -> Just TransactionScriptOutput
            { address = marloweAddress
            , assets = moneyToAssets $ V1.totalBalance $ V1.accounts txOutState
            , utxo = TxOutRef txId 0
            , datum = V1.MarloweData marloweParams txOutState txOutContract
            }
      }
  where
    txInterval = (vLow, vHigh)
    paymentToPayout :: V1.Payment -> Maybe (Payout 'V1)
    paymentToPayout payment@(V1.Payment _ (V1.Party (V1.Role role)) _ _) = Just $ Payout
      { address = payoutAddress
      , assets = moneyToAssets $ V1.paymentMoney payment
      , datum = AssetId (fromPlutusCurrencySymbol rolesCurrency) (fromPlutusTokenName role)
      }
    paymentToPayout _                                              = Nothing

    V1.MarloweParams { rolesCurrency } = marloweParams

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
    state : _ ->
      let
        go = maybe go pure =<< genHistoryScriptEvent state (length states)
      in
        go
  case withSome event $ flip reduceScriptEvent states of
    Left _        -> pure []
    Right states' -> do
      put states'
      (event :) <$> StateT \state -> do
        size <- getSize
        case size of
          0 -> pure ([], state)
          _ -> resize (max 0 $ size - 1) $ runStateT genHistoryScript state

genHistoryScriptEvent :: HistoryScriptBlockState -> Int -> Gen (Maybe (Some HistoryScriptEvent))
genHistoryScriptEvent HistoryScriptBlockState{..} n = frequency $ fold
  [ (fmap . fmap) Just <$> blockEvents
  , do
     (contractId, Some contractState) <- Map.toList contractStates
     genEvent <- contractEvents contractId contractState
     pure (2, fmap Some <$> genEvent)
  , [(5, Just <$> genCreateContract slotNo)]
  ]
  where
    BlockHeader{..} = block
    blockEvents = (5, genRollForward) : ((1,) <$> genRollBackward n)
    contractEvents :: ContractId -> HistoryScriptContractState v -> [Gen (Maybe (HistoryScriptEvent v))]
    contractEvents contractId HistoryScriptContractState{..} = fold
      [ maybeToList $ genApplyInputs contractVersion statePayoutAddress block contractId <$> stateScriptOutput
      , fmap Just . uncurry (genWithdraw contractVersion contractId) <$> Map.toList statePayouts
      ]

genRollForward :: Gen (Some HistoryScriptEvent)
genRollForward = do
  slots <- SlotNo <$> chooseWord64 (1, 120)
  header <- BlockHeaderHash . BS.pack <$> replicateM 8 arbitrary
  pure $ Some $ RollForward slots header

genRollBackward :: Int -> [Gen (Some HistoryScriptEvent)]
genRollBackward n
  | n <= 1 = []
  | otherwise = pure $ Some . RollBackward <$> frequency
  ( fmap pure <$> filter
      ((< n) . fromIntegral . snd)
      [ (1, 7)
      , (1, 6)
      , (2, 5)
      , (3, 4)
      , (5, 3)
      , (8, 2)
      , (13, 1)
      ]
  )

genCreateContract :: SlotNo -> Gen (Some HistoryScriptEvent)
genCreateContract slot = do
  contractId <- ContractId . flip TxOutRef 0 <$> genTxId
  marloweParams <- V1.MarloweParams <$> arbitrary
  datum <- V1.MarloweData marloweParams (V1.emptyState $ fromIntegral slot) <$> arbitrary
  let scriptAddress = "7062c56ccfc6217aff5692e1d3ebe89c21053d31fc11882cb21bfdd307"
  let payoutValidatorHash = "a2c56ccfc6217aff5692e1d3ebe89c21053d31fc11882cb21bfdd307"
  let createOutput = TransactionScriptOutput scriptAddress mempty (unContractId contractId) datum
  pure $ Some $ CreateContract contractId MarloweV1 CreateStep{..}

genTxId :: Gen TxId
genTxId = TxId . BS.pack <$> replicateM 8 arbitrary

genApplyInputs :: MarloweVersion v -> Address -> BlockHeader -> ContractId -> TransactionScriptOutput v -> Gen (Maybe (HistoryScriptEvent v))
genApplyInputs = \case
  MarloweV1 -> genApplyInputsV1

genApplyInputsV1 :: Address -> BlockHeader -> ContractId -> TransactionScriptOutput 'V1 -> Gen (Maybe (HistoryScriptEvent 'V1))
genApplyInputsV1 payoutAddress blockHeader contractId TransactionScriptOutput{..} = do
  let V1.MarloweData{..} = datum
  let minTime = fromIntegral $ slotNo blockHeader
  V1.TransactionInput (vLow, vHigh) inputs <- arbitraryValidStep marloweState {V1.minTime=minTime} marloweContract
  transactionId <- genTxId
  pure do
    output <- applyInputsV1 address payoutAddress vLow vHigh transactionId inputs datum
    pure $ ApplyInputs MarloweV1 $ Transaction
      { contractId
      , metadata = mempty
      , transactionId
      , blockHeader
      , validityLowerBound = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromIntegral vLow / 1000
      , validityUpperBound = posixSecondsToUTCTime $ secondsToNominalDiffTime $ fromIntegral vLow / 1000
      , inputs
      , output
      }

genWithdraw :: MarloweVersion v -> ContractId -> TxOutRef -> Payout v -> Gen (HistoryScriptEvent v)
genWithdraw = \case
  MarloweV1 -> genWithdrawV1

genWithdrawV1 :: ContractId -> TxOutRef -> Payout 'V1 -> Gen (HistoryScriptEvent 'V1)
genWithdrawV1 contractId utxo Payout{..} = do
  redeemingTx <- genTxId
  pure $ Withdraw contractId MarloweV1 RedeemStep{..}
