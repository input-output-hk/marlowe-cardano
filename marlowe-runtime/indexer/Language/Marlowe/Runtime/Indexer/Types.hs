{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.Types
  where

import Cardano.Api (CardanoMode, EraHistory, SystemStart)
import Control.Monad (guard, mfilter)
import Data.Bifunctor (bimap)
import Data.Either (lefts, rights)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , BlockHeader
  , Credential(..)
  , ScriptHash
  , Transaction(..)
  , TransactionInput(..)
  , TransactionOutput(..)
  , TxId
  , TxOutRef(..)
  , paymentCredential
  )
import Language.Marlowe.Runtime.Core.Api (ContractId(..))
import qualified Language.Marlowe.Runtime.Core.Api as Core
import Language.Marlowe.Runtime.History.Api
  ( CreateStep(..)
  , ExtractCreationError
  , ExtractMarloweTransactionError
  , SomeCreateStep(..)
  , extractCreation
  , extractMarloweTransaction
  )

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , transactions :: NonEmpty MarloweTransaction
  , invalidCreationTxs :: Map TxOutRef ExtractCreationError
  , invalidApplyInputsTxs :: Map TxId ExtractMarloweTransactionError
  } deriving (Eq, Show, Generic)

data MarloweTransaction
  = CreateTransaction MarloweCreateTransaction
  | ApplyInputsTransaction MarloweApplyInputsTransaction
  | WithdrawTransaction MarloweWithdrawTransaction
  deriving (Eq, Show, Generic)

newtype MarloweCreateTransaction = MarloweCreateTransaction
  { newContracts :: Map ContractId SomeCreateStep
  } deriving (Eq, Show, Generic)

data MarloweApplyInputsTransaction = forall v. MarloweApplyInputsTransaction
  { marloweVersion :: Core.MarloweVersion v
  , marloweTransaction :: Core.Transaction v
  }

instance Eq MarloweApplyInputsTransaction where
  MarloweApplyInputsTransaction Core.MarloweV1 txA == MarloweApplyInputsTransaction Core.MarloweV1 txB = txA == txB

instance Show MarloweApplyInputsTransaction where
  showsPrec p (MarloweApplyInputsTransaction Core.MarloweV1 tx) =
    showParen (p >= 11)
      ( showString "MarloweApplyInputsTransaction"
      . showSpace
      . showsPrec 11 Core.MarloweV1
      . showSpace
      . showsPrec 11 tx
      )

data MarloweWithdrawTransaction = MarloweWithdrawTransaction
  { contractId :: ContractId
  , consumedPayouts :: Set TxOutRef
  , consumingTx :: TxId
  } deriving (Eq, Show, Generic)


-- | The global Marlowe UTxO set
data MarloweUTxO = MarloweUTxO
  { unspentContractOutputs :: Map ContractId UnspentContractOutput
    -- ^ The UTxO set to the marlowe validators keyed by the contract ID

  , unspentPayoutOutputs :: Map ContractId (Set TxOutRef)
    -- ^ The UTxO set to the payout validators keyed by the contract Id
  } deriving (Eq, Show, Generic)

-- | Information about an unspent contract transaction output.
data UnspentContractOutput = forall v. UnspentContractOutput
  { marloweVersion :: Core.MarloweVersion v
  -- ^ The version of the contract.

  , txOutRef :: TxOutRef
  -- ^ The unspent output.

  , marloweAddress :: Address
  -- ^ The address of the marlowe validator.

  , payoutValidatorHash :: ScriptHash
  -- ^ The hash of the payout validator.
  }


instance Eq UnspentContractOutput where
  UnspentContractOutput Core.MarloweV1 refA addrA pHashA == UnspentContractOutput Core.MarloweV1 refB addrB pHashB =
    refA == refB && addrA == addrB && pHashA == pHashB


instance Show UnspentContractOutput where
  showsPrec p (UnspentContractOutput Core.MarloweV1 ref addr pHash) =
    showParen (p >= 11)
      ( showString "UnspentContractOutput"
      . showSpace
      . showsPrec 11 Core.MarloweV1
      . showSpace
      . showsPrec 11 ref
      . showSpace
      . showsPrec 11 addr
      . showSpace
      . showsPrec 11 pHash
      )


-- Extracts a MarloweBlock from Cardano Block information. Returns an updated
-- MarloweUTxO.
extractMarloweBlock
  :: SystemStart
  -> EraHistory CardanoMode
  -> Set ScriptHash
  -- ^ All known Marlowe script hashes.

  -> BlockHeader
  -- ^ The BlockHeader of the block.

  -> Set Transaction
  -- ^ The set of transactions in the block.

  -> MarloweUTxO
  -- ^ The current MarloweUTxO.

  -> Maybe (MarloweUTxO, MarloweBlock)
extractMarloweBlock systemStart eraHistory marloweScriptHashes blockHeader txs utxo =
  let
    -- Extract all contract creation transactions first.
    (invalidCreationTxs, createTxs) = sequence
      $ mapMaybe (extractCreateTx marloweScriptHashes)
      $ Set.toList txs

    creationTxs = Set.map (\TxOutRef{..} -> txId)
      $ fold
      $ Map.keysSet invalidCreationTxs : (Set.map unContractId . Map.keysSet . newContracts <$> createTxs)

    -- Update the MarloweUTxO with the successful creation transactions.
    utxo' = foldr updateMarloweUTxOForCreateTx utxo createTxs

    -- Remove the creation transactions from the candidates of other transaction types.
    txs' = Set.filter ((`Set.notMember` creationTxs) . \Transaction{..} -> txId) txs

    (utxo'', invalidApplyInputsTxs, applyInputsTxs, withdrawTxs) = extractApplyInputsAndWithdrawTxs systemStart eraHistory blockHeader txs' utxo'

    transactions = fold
      [ CreateTransaction <$> createTxs
      , ApplyInputsTransaction <$> applyInputsTxs
      , WithdrawTransaction <$> withdrawTxs
      ]
  in
    case transactions of
      [] -> Nothing
      x : xs -> Just
        ( utxo''
        , MarloweBlock
            { blockHeader
            , transactions = x :| xs
            , invalidCreationTxs
            , invalidApplyInputsTxs
            }
        )


-- | Extracts apply inputs and withdraw transactions from a set of chain
-- transactions recursively. The recursion is to handle the case when apply
-- inputs transactions produce outputs which are spend by other transactions in
-- the same block.
extractApplyInputsAndWithdrawTxs
  :: SystemStart
  -> EraHistory CardanoMode
  -> BlockHeader
  -> Set Transaction
  -> MarloweUTxO
  -> (MarloweUTxO, Map TxId ExtractMarloweTransactionError, [MarloweApplyInputsTransaction], [MarloweWithdrawTransaction])
extractApplyInputsAndWithdrawTxs systemStart eraHistory blockHeader txs marloweUTxO@MarloweUTxO{..} =
  let
    txList = Set.toList txs

    -- Extract all apply inputs transactions for the current UTxO.
    applyInputsTxResults = mapMaybe (extractApplyInputsTx systemStart eraHistory blockHeader unspentContractOutputs) txList

    -- Track which txs were apply inputs txs to avoid repeatedly examining them.
    applyInputsTxIds = Set.fromList $ applyInputsTxResults <&> \case
      Left (txId, _) -> txId
      Right MarloweApplyInputsTransaction{marloweTransaction = Core.Transaction{transactionId}} -> transactionId

    -- Remove the apply inputs transactions from the list of txs.
    txList' = filter ((`Set.notMember` applyInputsTxIds) . \Transaction{..} -> txId) txList

    -- Get the successful apply inputs transactions.
    applyInputsTxs = rights applyInputsTxResults

    -- Get the failed apply inputs transactions.
    invalidApplyInputsTxs = Map.fromList $ lefts applyInputsTxResults

    -- Extract all withdraw transactions for the current UTxO.
    withdrawTxs = mapMaybe (extractWithdrawTx unspentPayoutOutputs) txList'

    -- Track which txs were withdraw txs to avoid repeatedly examining them.
    withdrawTxIds = Set.fromList $ consumingTx <$> withdrawTxs

    -- Remove the payout transactions from the list of txs.
    txList'' = filter ((`Set.notMember` withdrawTxIds) . \Transaction{..} -> txId) txList'
  in
    case (applyInputsTxs, withdrawTxs) of
      -- No apply inputs or withdraws were successfully extracted, return empty-handed.
      ([], []) -> (MarloweUTxO{..}, invalidApplyInputsTxs, [], [])

      -- Some new transactions were extracted. Update the MarloweUTxO and run recursively to find more.
      _ ->
        let
          -- Update the marloweUTxO with the extracted transactions
          marloweUTxO' = foldr
            updateMarloweUTxOForWithdrawTx
            (foldr updateMarloweUTxOForApplyInputsTx marloweUTxO applyInputsTxs)
            withdrawTxs
        in
          case txList'' of
            -- No more transactions to inspect.
            [] -> (marloweUTxO', invalidApplyInputsTxs, applyInputsTxs, withdrawTxs)
            _ ->
              let
                -- Recursive call with new MarloweUTxO and remaining transactions (NOTE not tail-call optimized)
                (marloweUTxO'', invalidApplyInputsTxs', applyInputsTxs', withdrawTxs') = extractApplyInputsAndWithdrawTxs systemStart eraHistory blockHeader (Set.fromList txList'') marloweUTxO'
              in
                ( marloweUTxO''
                , invalidApplyInputsTxs <> invalidApplyInputsTxs'
                , applyInputsTxs <> applyInputsTxs'
                , withdrawTxs <> withdrawTxs'
                )


-- | Extracts a MarloweCreateTransaction from a Chain transaction. A single
-- transaction can create multiple Marlowe contracts, and this function returns
-- a map of outputs that it failed to extract as well as the map of contracts
-- it successfully extracted.
extractCreateTx
  :: Set ScriptHash
  -- ^ All known Marlowe script hashes.

  -> Transaction
  -> Maybe (Map TxOutRef ExtractCreationError, MarloweCreateTransaction)
extractCreateTx marloweScriptHashes Transaction{..} =
  let
    -- Find all outputs that create a new Marlowe contract
    contractIds = mapMaybe (uncurry $ extractContractId marloweScriptHashes)
      $ zip (TxOutRef txId <$> [0..]) outputs

    -- Try to extract a creation step for each prospective contract ID
    extractCreationResults = contractIds <&> \contractId ->
      bimap (unContractId contractId,) (contractId,) $ extractCreation contractId Transaction{..}
  in
    -- Return nothing if there are no results
    case extractCreationResults of
      [] -> Nothing
      _ -> Just
        -- Note which contract creations failed extraction.
        ( Map.fromList $ lefts extractCreationResults

        -- Build a transaction from the contracts that succeeded.
        , MarloweCreateTransaction $ Map.fromList $ rights extractCreationResults
        )


-- | Extracts a ContractId from a transaction output if it is a Marlowe contract output.
extractContractId
  :: Set ScriptHash
  -- ^ All known Marlowe script hashes.

  -> TxOutRef
  -- ^ The txOutRef of the transaction output.

  -> TransactionOutput
  -- ^ The transaction output.

  -> Maybe ContractId
extractContractId marloweScriptHashes txOutRef TransactionOutput{..} = do
  -- Extract the payment credential from the address.
  credential <- paymentCredential address

  -- The output is a Marlowe output if the credential is a script credential whose script hash is one of the known Marlowe script hashes.
  case credential of
    ScriptCredential hash -> ContractId txOutRef <$ guard (Set.member hash marloweScriptHashes)
    _ -> Nothing


-- | Extracts an apply inputs transaction from a chain transaction. Returns
-- nothing if the transaction does not apply an input to any unspent contract
-- output.
extractApplyInputsTx
  :: SystemStart
  -> EraHistory CardanoMode
  -> BlockHeader
  -> Map ContractId UnspentContractOutput
  -- ^ The current unspent contract transaction outputs.

  -> Transaction
  -- ^ The transaction to extract an apply inputs tx from.

  -> Maybe (Either (TxId, ExtractMarloweTransactionError) MarloweApplyInputsTransaction)
extractApplyInputsTx systemStart eraHistory blockHeader unspentContractOutputs tx@Transaction{inputs, txId = txId'} = do
  -- Convert the transaction's inputs to a set of tx out refs.
  let inputTxOutRefs = Set.map (\TransactionInput{..} -> TxOutRef{..}) inputs

  -- Find an unspent contract output that the transaction spends.
  (contractId, UnspentContractOutput{..}) <- find (flip Set.member inputTxOutRefs . txOutRef . snd) $ Map.toList unspentContractOutputs

  -- Extract a Marlowe transaction of the correct version.
  Just case extractMarloweTransaction marloweVersion systemStart eraHistory contractId marloweAddress payoutValidatorHash txOutRef blockHeader tx of
    Left err -> Left (txId', err)
    Right marloweTransaction -> Right MarloweApplyInputsTransaction
      { marloweVersion
      , marloweTransaction
      }


-- | Extracts a withdraw transaction from a chain transaction. Returns nothing
-- if the transaction does not withdraw contract payouts.
extractWithdrawTx :: Map ContractId (Set TxOutRef) -> Transaction -> Maybe MarloweWithdrawTransaction
extractWithdrawTx unspentPayoutOutputs Transaction{inputs, txId = consumingTx} = do
  -- Convert the transaction's inputs to a set of tx out refs.
  let inputTxOutRefs = Set.map (\TransactionInput{..} -> TxOutRef{..}) inputs

  -- Find an unspent payout output set that the transaction spends.
  (contractId, consumedPayouts) <- find (not . Set.null . snd) $ fmap (Set.intersection inputTxOutRefs) <$> Map.toList unspentPayoutOutputs
  pure MarloweWithdrawTransaction{..}


-- | Updates a MarloweUTxO with information from a MarloweCreateTransaction.
updateMarloweUTxOForCreateTx :: MarloweCreateTransaction -> MarloweUTxO -> MarloweUTxO
updateMarloweUTxOForCreateTx MarloweCreateTransaction{..} MarloweUTxO{..} = MarloweUTxO
    -- Pass the payout outputs through unchanged.
  { unspentPayoutOutputs
    -- New unspent contract outputs for the new contracts.
  , unspentContractOutputs = unspentContractOutputs <> (createStepToUnspentContractOutput <$> newContracts)
  }
  where
    createStepToUnspentContractOutput (SomeCreateStep marloweVersion CreateStep{..}) =
      let
        Core.TransactionScriptOutput{..} = createOutput
        txOutRef = utxo
        marloweAddress = address
      in
        UnspentContractOutput{..}


-- Updates a MarloweUTxO with information from a MarloweApplyInputsTransaction (payouts and contract output).
updateMarloweUTxOForApplyInputsTx :: MarloweApplyInputsTransaction -> MarloweUTxO -> MarloweUTxO
updateMarloweUTxOForApplyInputsTx MarloweApplyInputsTransaction{..} MarloweUTxO{..} =
  let
    -- Extract the contractId, the new contract output, and any payouts from the transaction
    Core.Transaction{contractId, output = Core.TransactionOutput{..}} = marloweTransaction

    -- A helper function which will either update or remove the unspent
    -- contract output for this contract, depending on whether or not the
    -- transaction produces a new script output.
    updateUnspentContractOutput UnspentContractOutput{marloweAddress, payoutValidatorHash} =
      scriptOutput <&> \Core.TransactionScriptOutput{..} -> UnspentContractOutput
        { marloweVersion
        , txOutRef = utxo
        , marloweAddress
        , payoutValidatorHash
        }

  in
    MarloweUTxO
      -- Add new payouts to the unspentPayoutOutputs
      { unspentPayoutOutputs = Map.unionWith (<>) unspentPayoutOutputs $ Map.singleton contractId $ Map.keysSet payouts

      -- If there is a new output from this transaction, update the current
      -- output for this contract. Otherwise, remove it from the UTxO.
      , unspentContractOutputs = Map.alter (>>= updateUnspentContractOutput) contractId unspentContractOutputs
      }


-- Updates a MarloweUTxO with information from a MarloweWithdrawTransaction (removes payouts).
updateMarloweUTxOForWithdrawTx :: MarloweWithdrawTransaction -> MarloweUTxO -> MarloweUTxO
updateMarloweUTxOForWithdrawTx MarloweWithdrawTransaction{..} MarloweUTxO{..} =
  MarloweUTxO
    -- Remove payouts from the unspentPayoutOutputs
    { unspentPayoutOutputs = Map.alter (>>= removePayout) contractId unspentPayoutOutputs

    -- Pass through the unspentContractOutputs
    , unspentContractOutputs
    }
  where
    -- Remove the consumed payouts for the contract and remove the contractId from the
    -- map if there are no more payouts left afterward.
    removePayout = mfilter (not . Set.null) . Just . (`Set.difference` consumedPayouts)
