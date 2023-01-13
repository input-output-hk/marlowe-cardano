{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.Types
  where

import Cardano.Api (CardanoMode, EraHistory, SystemStart)
import Control.Monad (guard, mfilter, unless, when)
import Control.Monad.Except (runExceptT, withExceptT)
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (except)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Writer (WriterT, execWriterT)
import Control.Monad.Writer.Class (tell)
import Data.Foldable (for_)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple (swap)
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
import Witherable (wither)

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , transactions :: NonEmpty MarloweTransaction
  } deriving (Eq, Show, Generic)

data MarloweTransaction
  = CreateTransaction MarloweCreateTransaction
  | ApplyInputsTransaction MarloweApplyInputsTransaction
  | WithdrawTransaction MarloweWithdrawTransaction
  | InvalidCreateTransaction ContractId ExtractCreationError
  | InvalidApplyInputsTransaction TxId ExtractMarloweTransactionError
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
  -- ^ The current MarloweUTxO
  -> Maybe (MarloweUTxO, MarloweBlock)
extractMarloweBlock systemStart eraHistory marloweScriptHashes blockHeader txs =
  sequenceA . swap . runState do
    transactions <- execWriterT $ for_ txs \tx -> do
      extractCreateTx marloweScriptHashes tx
      extractApplyInputsTx systemStart eraHistory blockHeader tx
      extractWithdrawTx tx
    pure case transactions of
      [] -> Nothing
      x : xs -> Just MarloweBlock { blockHeader, transactions = x :| xs }

-- | Extracts a MarloweCreateTransaction from a Chain transaction. A single
-- transaction can create multiple Marlowe contracts, and this function returns
-- a map of outputs that it failed to extract as well as the map of contracts
-- it successfully extracted.
extractCreateTx
  :: Set ScriptHash
  -- ^ All known Marlowe script hashes.

  -> Transaction
  -> WriterT [MarloweTransaction] (State MarloweUTxO) ()
extractCreateTx marloweScriptHashes Transaction{..} = do
  -- Creation transactions cannot consume outputs from other Marlowe contracts.
  when noMarloweInputs do
    let
      -- Find all outputs that create a new Marlowe contract
      contractIds = mapMaybe (uncurry $ extractContractId marloweScriptHashes)
        $ zip (TxOutRef txId <$> [0..]) outputs

    -- Try to extract a creation step for each prospective contract ID, reporting
    -- any errors found.
    newContracts <- Map.fromList <$> flip wither contractIds \contractId ->
      case extractCreation contractId Transaction{..} of
        Left err -> do
          tell [InvalidCreateTransaction contractId err]
          pure Nothing
        Right creationStep -> pure $ Just (contractId, creationStep)

    -- Prevent the creation of empty create transactions.
    unless (null newContracts) do

      -- Add the new contract outputs to the MarloweUTxO
      let newUnspentContractOutputs = createStepToUnspentContractOutput <$> newContracts
      modify \utxo -> utxo { unspentContractOutputs = unspentContractOutputs utxo <> newUnspentContractOutputs }

      tell [CreateTransaction MarloweCreateTransaction{..}]
  where
    noMarloweInputs = not $ any isMarloweInput inputs
    isMarloweInput TransactionInput{address} = case paymentCredential address of
      Just (ScriptCredential scriptHash) -> Set.member scriptHash marloweScriptHashes
      _ -> False

    createStepToUnspentContractOutput (SomeCreateStep marloweVersion CreateStep{..}) =
      let
        Core.TransactionScriptOutput{..} = createOutput
        txOutRef = utxo
        marloweAddress = address
      in
        UnspentContractOutput{..}


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

  -> Transaction
  -- ^ The transaction to extract an apply inputs tx from.

  -> WriterT [MarloweTransaction] (State MarloweUTxO) ()
extractApplyInputsTx systemStart eraHistory blockHeader tx@Transaction{inputs, txId = txId'} = do
  mTransaction <- runMaybeT $ runExceptT $ withExceptT (txId',) do
    -- Convert the transaction's inputs to a set of tx out refs.
    let inputTxOutRefs = Set.map (\TransactionInput{..} -> TxOutRef{..}) inputs

    -- Get the unspentContractOutputs fro the MarloweUTxO
    contractUTxO <- gets unspentContractOutputs

    -- Find an unspent contract output that the transaction spends.
    (contractId, UnspentContractOutput{..}) <- lift $ hoistMaybe $ find (flip Set.member inputTxOutRefs . txOutRef . snd) $ Map.toList contractUTxO

    -- Update the MarloweUTxO to remove the unspent contract output.
    modify \utxo -> utxo { unspentContractOutputs = Map.delete contractId $ unspentContractOutputs utxo }

    -- Extract a Marlowe transaction of the correct version.
    marloweTransaction <- except $ extractMarloweTransaction marloweVersion systemStart eraHistory contractId marloweAddress payoutValidatorHash txOutRef blockHeader tx

    -- Add new payouts to the unspentPayoutOutputs and update the MarloweUTxO to add the new unspent contract output if one was produced.
    modify \MarloweUTxO{..} -> MarloweUTxO
      { unspentPayoutOutputs = Map.unionWith (<>) unspentPayoutOutputs
          $ Map.singleton contractId
          $ Map.keysSet
          $ Core.payouts
          $ Core.output marloweTransaction
      , unspentContractOutputs = case Core.scriptOutput $ Core.output marloweTransaction of
          Nothing -> unspentContractOutputs
          Just scriptOutput ->
            let
              newOutput = UnspentContractOutput
                { marloweVersion
                , txOutRef = Core.utxo scriptOutput
                , marloweAddress
                , payoutValidatorHash
                }
            in
              Map.insert contractId newOutput unspentContractOutputs
      }

    pure MarloweApplyInputsTransaction
      { marloweVersion
      , marloweTransaction
      }

  for_ mTransaction \case
    Left (txId, err) -> tell [InvalidApplyInputsTransaction txId err]
    Right transaction -> tell [ApplyInputsTransaction transaction]


-- | Extracts a withdraw transaction from a chain transaction. Returns nothing
-- if the transaction does not withdraw contract payouts. Removes payouts from
-- the Marlowe UTxO.
extractWithdrawTx :: Transaction -> WriterT [MarloweTransaction] (State MarloweUTxO) ()
extractWithdrawTx Transaction{inputs, txId = consumingTx} = do
  -- Convert the transaction's inputs to a set of tx out refs.
  let inputTxOutRefs = Set.map (\TransactionInput{..} -> TxOutRef{..}) inputs

  -- Get the unspentPayoutOutputs fro the MarloweUTxO
  unspentPayoutOutputs <- gets unspentPayoutOutputs

  -- Find an unspent payout output set that the transaction spends.
  let mPayouts = find (not . Set.null . snd) $ fmap (Set.intersection inputTxOutRefs) <$> Map.toList unspentPayoutOutputs
  for_ mPayouts \(contractId, consumedPayouts) -> do

    -- Update the MarloweUTxO to remove the unspentPayoutOutputs.
    modify \utxo -> utxo { unspentPayoutOutputs = Map.alter (>>= removePayout consumedPayouts) contractId unspentPayoutOutputs }

    tell [WithdrawTransaction MarloweWithdrawTransaction{..}]
  where
    -- Remove the consumed payouts for the contract and remove the contractId from the
    -- map if there are no more payouts left afterward.
    removePayout consumedPayouts = mfilter (not . Set.null) . Just . (`Set.difference` consumedPayouts)


hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
