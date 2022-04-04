-- | Extraction of Marlowe contracts and history from transaction data.


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}


module Language.Marlowe.Client.History (
-- * Types
  History(..)
, MarloweTxOut
, MarloweTxOutRef
-- * Contract History
, marloweHistory
, marloweHistoryFrom
, marloweUtxoStatesAt
, marloweStatesFrom
, toMarloweState
-- * History Queriies
, histories
, history
, historyFrom
, creationTxOut
, filterOutputs
, txMarloweData
, txDatums
, txInputs
, txRedeemers
, toMarlowe
) where


import Cardano.Api.Shelley (ShelleyBasedEra (ShelleyBasedEraAlonzo), Tx (ShelleyTx))
import Cardano.Binary (toCBOR, toStrictByteString)
import Control.Lens ((^.))
import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (second)
import Data.List (nub)
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)
import Data.Tuple.Extra (secondM)
import GHC.Generics (Generic)
import Language.Marlowe.Scripts (SmallTypedValidator, TypedMarloweValidator, smallUntypedValidator)
import Language.Marlowe.Semantics (MarloweData, MarloweParams (..), TransactionInput (TransactionInput))
import Ledger (ChainIndexTxOut (..), ciTxOutAddress, toTxOut)
import Ledger.TimeSlot (slotRangeToPOSIXTimeRange)
import Ledger.Tx.CardanoAPI (SomeCardanoApiTx (SomeTx))
import Ledger.Typed.Scripts (validatorAddress)
import Ledger.Typed.Tx (TypedScriptTxOut (..), TypedScriptTxOutRef (..))
import Plutus.ChainIndex.Tx (ChainIndexTx, ChainIndexTxOutputs (..), citxCardanoTx, citxData, citxInputs, citxOutputs,
                             citxScripts, citxTxId, citxValidRange)
import Plutus.Contract (Contract)
import Plutus.Contract.Error (AsContractError)
import Plutus.Contract.Logging (logInfo)
import Plutus.Contract.Request (txsAt, utxosTxOutTxAt, utxosTxOutTxFromTx)
import Plutus.V1.Ledger.Address (scriptHashAddress)
import Plutus.V1.Ledger.Api (Address (..), CurrencySymbol (..), Datum (..), Extended (..), Interval (..),
                             LowerBound (..), Redeemer (..), TxId (..), TxOut (..), TxOutRef (..), UpperBound (..),
                             dataToBuiltinData, fromBuiltinData, toBuiltin)
import Plutus.V1.Ledger.Scripts (ScriptHash (..))
import Plutus.V1.Ledger.Tx (txInRef)

import qualified Cardano.Ledger.Alonzo.Data as Alonzo (Data (..))
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo (ScriptPurpose (Spending), ValidatedTx (..), body, indexedRdmrs,
                                                     inputs)
import qualified Cardano.Ledger.TxIn as Cardano (TxIn (..))
import qualified Data.ByteString as BS (drop)
import qualified Data.Map.Strict as M (Map, keys, lookup, toList)
import qualified Data.Set as S (toList)
import Plutus.Contract.Unsafe (unsafeGetSlotConfig)


-- | A transaction-output reference specific to Marlowe.
type MarloweTxOut = TypedScriptTxOut TypedMarloweValidator


-- | A transaction output specific to Marlowe.
type MarloweTxOutRef = TypedScriptTxOutRef TypedMarloweValidator


-- | History of a Marlowe contract.
data History =
    -- | The contract was created.
    Created
    {
      historyTxOutRef :: TxOutRef          -- ^ The UTxO that created the contract.
    , historyData     :: MarloweData       -- ^ The Marlowe data attached to the UTxO.
    , historyNext     :: Maybe History     -- ^ The next step in the history, if known.
    }
    -- | Input was applied to the contract.
  | InputApplied
    {
      historyInput    :: TransactionInput  -- ^ The Marlowe input that was applied.
    , historyTxOutRef :: TxOutRef          -- ^ The UTxO that resulted from the input being applied.
    , historyData     :: MarloweData       -- ^ The Marlowe data attached to the UTxO.
    , historyNext     :: Maybe History     -- ^ The next step in the history, if known.
    }
    -- | The contract was closed.
  | Closed
    {
      historyInput :: TransactionInput  -- ^ The Marlowe input that was applied.
    , historyTxId  :: TxId              -- ^ The transaction that resulted from the input being applied.
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON, FromJSON)


-- | Retrieve the history of a role-based Marlowe contract.
marloweHistory :: AsContractError e
               => MarloweParams                   -- ^ The Marlowe validator parameters.
               -> Contract w s e (Maybe History)  -- ^ The original contract and the sequence of redemptions, if any.
marloweHistory params =
  do
    let address = validatorAddress $ smallUntypedValidator params
    logInfo $ "[DEBUG:marloweHistory] address = " <> show address
    -- The script address contains transactions that have datum.
    addressTxns <- txsAt address
    logInfo $ "[DEBUG:marloweHistory] length addressTxns = " <> show (length addressTxns)
    -- When a contract closes, there may be a UTxO at the role address.
    roleTxns <- txsAt . scriptHashAddress $ rolePayoutValidatorHash params
    logInfo $ "[DEBUG:marloweHistory] length roleTxns = " <> show (length roleTxns)
{-
    FIXME: Commented out in order to stress the PAB/CI lesss, just as a workaround for PAB queries freezing.
    -- When a contract closes, there may be a UTxO at the owner's public key hash address.
    pkhTxns <- txsAt . flip Address Nothing . PubKeyCredential . unPaymentPubKeyHash =<< ownPaymentPubKeyHash
    logInfo $ "[DEBUG:marloweHistory] length pkhTxns = " <> show (length pkhTxns)
-}
    -- TODO: Extract all PKHs from the contract, and query these addresses, too.
    pure
      . history params address
      . nub
      $ addressTxns <> roleTxns {- <> pkhTxns -}


-- | Retrieve the history of a role-based Marlowe contract.
history :: MarloweParams   -- ^ The Marlowe validator parameters.
        -> Address         -- ^ The Marlowe validator address.
        -> [ChainIndexTx]  -- ^ The transactions at the Marlowe validator and role validator addresses.
        -> Maybe History   -- ^ The original contract and the sequence of redemptions, if any.
history params address citxs =
  case histories params address citxs of
    -- If role tokens are minted by the "create" endpoint, then there should only ever be on contract at the address.
    [history'] -> Just history'
    -- Either there is no contract yet, or role tokens have been reused for multiple contracts.
    _          -> Nothing


-- | Retrieve the histories of a role-based Marlowe contract.
histories :: MarloweParams   -- ^ The Marlowe validator parameters.
          -> Address         -- ^ The Marlowe validator address.
          -> [ChainIndexTx]  -- ^ The transactions at the Marlowe validator and role validator addresses.
          -> [History]       -- ^ The original contracts and the sequence of redemptions.
histories params address citxs =
  [
    Created (tyTxOutRefRef creation) (toMarloweState creation)
      $ historyFrom address citxs creation
  |
    creation <- creationTxOut params address `mapMaybe` citxs
  ]


-- | Construct the sequence of redemptions following from a particular Marlowe transactions.
historyFrom :: Address             -- ^ The Marlowe validator address.
            -> [ChainIndexTx]      -- ^ The transactions at the Marlowe validator and role validator addresses.
            -> MarloweTxOutRef     -- ^ The Marlowe transaction to start from.
            -> Maybe History       -- ^ The sequence of subsequent redemptions.
historyFrom address citxs consumed =
  let
    consumed' = tyTxOutRefRef consumed
    anyInputsConsumed citx =
      let
        inputs = S.toList $ citx ^. citxInputs
      in
        any (\txIn -> consumed' == txInRef txIn) inputs
  in
    -- The next redemption must consume the input.
    case filter anyInputsConsumed citxs of
      -- Only one transaction can consume the output of the previous step.
      [citx] -> -- Find the redeemer
                case lookup consumed' $ txInputs citx of
                  -- The output of the previous step was consumed with a redeemer.
                  Just inputs -> -- Determine whether there is output to the script.
                                 case filterOutputs address citx of
                                   [mo] -> -- There was datum UTxO to the script, so the contract continues.
                                           Just
                                             . InputApplied inputs (tyTxOutRefRef mo) (toMarloweState mo)
                                             $ historyFrom address citxs mo
                                   _    -> -- There was no datum UTxO to the script, so the contract is now closed.
                                           Just
                                             . Closed inputs
                                             $ citx ^. citxTxId
                  -- The output of the previous step couldn't have been consumed without a redeemer.
                  _           -> Nothing
      -- The output of the previous step hasn't been consumed, so the contract is still in progress.
      _      -> Nothing

-- | Retrieve the states in UTxOs at the validator address.
marloweUtxoStatesAt :: AsContractError e
                    => SmallTypedValidator                                                 -- ^ The Marlowe validator.
                    -> Contract w s e ([MarloweTxOutRef], M.Map TxOutRef ChainIndexTxOut)  -- ^ Action for finding the Marlowe UTxOs at the validator.
marloweUtxoStatesAt validator =
  do
    utxos <- utxosTxOutTxAt $ validatorAddress validator
    let
      toMarlowe' (txOutRef, (citxOut, citx)) = toMarlowe citx (toTxOut citxOut) txOutRef
    pure
      (
        mapMaybe toMarlowe' $ M.toList utxos
      , fst <$> utxos
      )


-- | Retrieve the Marlowe history from a transaction.
marloweHistoryFrom :: AsContractError e
                   => SmallTypedValidator       -- ^ The Marlowe validator.
                   -> ChainIndexTx              -- ^ The transaction.
                   -> Contract w s e [History]  -- ^ Action for finding the history for the transaction.
marloweHistoryFrom validator citx =
  do
    let
      valAddress = validatorAddress validator
      toHistory (txOutRef, (citxOut, citx')) =
        let
          inputs = txInputs citx'
          toAddress = valAddress == (citxOut ^. ciTxOutAddress)
          extractDatum (ScriptChainIndexTxOut _ _ (Right dat) _) = fromBuiltinData $ getDatum dat
          extractDatum _                                         = Nothing
          noDatum = null $ txMarloweData citx'
        in
          case (toAddress, inputs, extractDatum citxOut, noDatum) of
            -- The address received the UTxO, no contract input was consumed, and datum is in the UTxO.
            (True,  []          , Just md, _   ) -> Just $ Created txOutRef md Nothing
            -- The address received the UTxO, contract input was consumed, and the datum is in the UTxO.
            (True,  [(_, input)], Just md, _   ) -> Just $ InputApplied input txOutRef md Nothing
            -- Another address received the UTxO, the contract input was consumed, but no datum is in the UTxO.
            (False, [(_, input)], _      , True) -> Just $ Closed input $ citx' ^. citxTxId
            -- An unexpected UTxO was found, violating the constraints on the script.
            _                                    -> Nothing
    nub
      . mapMaybe toHistory
      . nub
      <$> utxosTxOutTxFromTx citx


-- | Retrieve the states in UTxOs of a transaction, optionally filtering them by the validator address.
marloweStatesFrom :: AsContractError e
                  => Maybe SmallTypedValidator         -- ^ The Marlowe validator, if UTxOs are to be filtered by its address.
                  -> ChainIndexTx                      -- ^ The transaction.
                  -> Contract w s e [MarloweTxOutRef]  -- ^ Action for finding the UTxOs in the transaction, optionally filtered by the validator address.
marloweStatesFrom validator citx =
  do
    let
      valAddress = validatorAddress <$> validator
      addressFilter :: MarloweTxOutRef -> Maybe MarloweTxOutRef
      addressFilter out =
         do
          guard
            $ isNothing valAddress
            || valAddress == Just (txOutAddress . tyTxOutTxOut $ tyTxOutRefOut out)
          pure out
      toMarlowe' (txOutRef, (citxOut, citx')) =
        addressFilter
          =<< toMarlowe citx' (toTxOut citxOut) txOutRef
    mapMaybe toMarlowe'
      <$> utxosTxOutTxFromTx citx


-- | Extract the Marlowe state from a Marlowe-specific output.
toMarloweState :: MarloweTxOutRef  -- ^ The Marlowe-specific output.
               -> MarloweData      -- ^ The Marlowe data.
toMarloweState = tyTxOutData . tyTxOutRefOut

-- | Test whether a transaction created a Marlowe contract.
creationTxOut :: MarloweParams          -- ^ The Marlowe validator parameters.
              -> Address                -- ^ The Marlowe validator address.
              -> ChainIndexTx           -- ^ The transaction to be checked.
              -> Maybe MarloweTxOutRef  -- ^ The creation-transaction output and the contract, if any.
creationTxOut MarloweParams{..} address citx =
  do
    -- Ensure that the transaction minted the role currency.
    guard
      . elem (ScriptHash $ unCurrencySymbol rolesCurrency)
      . M.keys
      $ citx ^. citxScripts
    -- Find the output to the script address, if any.
    case filterOutputs address citx of
      [creation] -> Just creation
      _          -> Nothing


-- | Find the Marlowe outputs at an address.
filterOutputs :: Address            -- ^ The Marlowe address.
              -> ChainIndexTx       -- ^ The transaction.
              -> [MarloweTxOutRef]  -- ^ The outputs to the Marlowe address.
filterOutputs address =
  filter ((address ==) . txOutAddress . tyTxOutTxOut . tyTxOutRefOut)
    . txMarloweData


-- | Extract Marlowe data from a transaction.
txMarloweData :: ChainIndexTx       -- ^ The transaction.
              -> [MarloweTxOutRef]  -- ^ The outputs that have Marlowe data.
txMarloweData citx =
  catMaybes
    $ uncurry (toMarlowe citx)
    <$> txDatums citx


-- | Make an output specific to Marlowe.
toMarlowe :: ChainIndexTx           -- ^ The transaction.
          -> TxOut                  -- ^ The output.
          -> TxOutRef               -- ^ The output reference.
          -> Maybe MarloweTxOutRef  -- ^ The Marlowe-specific output, if any.
toMarlowe citx txOut txOutRef =
  do
    dh <- txOutDatumHash txOut
    dat <- M.lookup dh $ citx ^. citxData
    md <- fromBuiltinData $ getDatum dat
    pure
      . TypedScriptTxOutRef txOutRef
      $ TypedScriptTxOut txOut md


-- | Extract transaction outputs with datum.
txDatums :: ChainIndexTx         -- ^ The transaction.
         -> [(TxOut, TxOutRef)]  -- ^ The outputs that have datum.
txDatums citx =
  case citx ^. citxOutputs of
    ValidTx txOuts -> filter (\(txOut, _) -> isJust $ txOutDatumHash txOut)
                      $ second (\i -> TxOutRef (citx ^. citxTxId) i)
                      <$> zip txOuts [0..]
    InvalidTx      -> []


-- | Extract Marlowe input from a transaction.
txInputs :: ChainIndexTx                    -- ^ The transaction.
         -> [(TxOutRef, TransactionInput)]  -- ^ The inputs that have Marlowe inputs.
txInputs citx =
  case slotRangeToPOSIXTimeRange slotConfig $ citx ^. citxValidRange of
    Interval (LowerBound (Finite l) True) (UpperBound (Finite h) False) ->
      let
        slots = (l, h)
      in
        catMaybes
          $ secondM (fmap (TransactionInput slots) . fromBuiltinData . getRedeemer)
          <$> txRedeemers citx
    _ -> []  -- TODO: Should this instead throw an error in an error monad?
  where
    slotConfig = unsafeGetSlotConfig

-- | Extract transaction inputs with redeemers.
txRedeemers :: ChainIndexTx            -- ^ The transaction.
            -> [(TxOutRef, Redeemer)]  -- ^ The inputs that have redeemers.
txRedeemers citx =
  case citx ^. citxCardanoTx of
    Just (SomeTx (ShelleyTx ShelleyBasedEraAlonzo tx@Alonzo.ValidatedTx{}) _) ->
      catMaybes
        [
          do
            (Alonzo.Data dat, _) <- Alonzo.indexedRdmrs tx $ Alonzo.Spending txin
            pure (TxOutRef{..}, Redeemer . dataToBuiltinData $ dat)
        |
          txin@(Cardano.TxIn txid txix) <- S.toList . Alonzo.inputs . Alonzo.body $ tx
        , let txOutRefId = TxId . toBuiltin . BS.drop 2 . toStrictByteString . toCBOR $ txid  -- TODO: Find a pre-existing function to convert Alonzo to Plutus TxIds.
              txOutRefIdx = fromIntegral txix
        ]
    _ -> []
