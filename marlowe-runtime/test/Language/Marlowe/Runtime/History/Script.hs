{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}

module Language.Marlowe.Runtime.History.Script where

import Control.Monad (foldM, (<=<))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Data (type (:~:) (Refl))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Type.Equality (testEquality)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), Assets (..), BlockHeader (..), BlockHeaderHash (..),
                                               Lovelace (..), PolicyId (..), SlotNo, TokenName (..), Tokens (..), TxId,
                                               TxOutRef (..))
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Numeric.Natural (Natural)
import qualified Plutus.V1.Ledger.Api as P
import qualified PlutusTx.AssocMap as AM

-- An event in a history test script
data HistoryScriptEvent
  -- Extend the chain with a new block
  = RollForward SlotNo BlockHeaderHash
  -- Retract the given number of blocks from the chain.
  | RollBackward Natural
  -- Create a new contract in the current block
  | forall v. CreateContract ContractId (MarloweVersion v) (CreateStep v)
  -- Apply Inputs to a contract in the current block
  | forall v. ApplyInputs ContractId TxId (MarloweVersion v) (Redeemer v)
  -- Withdraw funds from a contract in the current block
  | forall v. Withdraw ContractId (MarloweVersion v) (RedeemStep v)

newtype HistoryScript = HistoryScript { unHistoryScript :: [HistoryScriptEvent] }

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
foldScript = foldM (flip reduceScriptEvent) [] . unHistoryScript

reduceScriptEvent :: HistoryScriptEvent -> HistoryScriptState -> Either ReductionError HistoryScriptState
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
  ApplyInputs contractId txId version redeemer ->
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
            let BlockHeader{slotNo} = block
            TransactionOutput{..} <- applyInputs version slotNo txId redeemer datum
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

applyInputs :: MarloweVersion v -> SlotNo -> TxId -> Redeemer v -> Datum v -> Either ReductionError (TransactionOutput v)
applyInputs = \case
  MarloweV1 -> applyInputsV1

applyInputsV1 :: SlotNo -> TxId -> [V1.Input] -> V1.MarloweData -> Either ReductionError (TransactionOutput 'V1)
applyInputsV1 slot txId txInputs V1.MarloweData{..} =
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
    intervalMax = \case
      V1.Close         -> P.POSIXTime 9999999999
      V1.Pay _ _ _ _ c -> intervalMax c
      V1.If _ c1 c2    -> min (intervalMax c1) (intervalMax c2)
      V1.When _ t c
        | t <= fromIntegral slot -> intervalMax c
        | otherwise -> t
      V1.Let _ _ c     -> intervalMax c
      V1.Assert _ c    -> intervalMax c
    txInterval = (fromIntegral slot, intervalMax marloweContract)

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
