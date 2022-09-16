{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Transaction.Constraints
  where

import qualified Cardano.Api as Cardano
import qualified Cardano.Api.Shelley as Cardano
import qualified Data.Aeson as Aeson
import Data.Function (on)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (PaymentKeyHash)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Transaction.SystemStart as Cardano
import qualified Plutus.V2.Ledger.Api as P

-- | Describes a set of Marlowe-specific conditions that a transaction must satisfy.
data TxConstraints v = TxConstraints
  { inputConstraint :: InputConstraint v
  -- ^ The constraint on the script input of the transaction
  , roleTokenConstraints :: Set RoleTokenConstraint
  -- ^ Constraints related to role tokens
  , outputConstraints :: Set (OutputConstraint v)
  -- ^ Constraints on the outputs of a transaction
  , signatureConstraints :: Set PaymentKeyHash
  -- ^ Extra payment key hashes that require signatures.
  --
  -- Rules to check (for each key in the set):
  --   1. If none of the inputs specify the key as a witness, then the
  --      txExtraKeyWits of the body content contain it.
  --   2. If theExtraKeyWits of the body content does not contain the key, then
  --      at least one input does.
  , metadataConstraints :: Map Int Aeson.Value
  -- ^ Metadata to add to the transaction.
  --
  -- Rules to check:
  --   1. forall (i, value) in the constraints, the transaction contains value
  --     at index i of its metadata.
  } deriving (Eq, Ord, Show)

-- | A constraint related to a role token.
data RoleTokenConstraint
  = MintRoleToken Chain.AssetId Chain.Address
  -- ^ Specifies that the transaction must mint one role token with the given
  -- AssetId and send it to the given address, along with the min UTXO ADA.
  --
  -- Rules to check:
  --   1. The transaction mints one token of the given assetID
  --   2. The transaction sends one token of the given assetID to the given address.
  --   3. The output in rule 2 covers the min UTXO requirement.
  --   4. The output in rule 2 does not contain any other tokens.
  | SpendRoleToken Chain.AssetId
  -- ^ Specifies that the transaction must spend a UTXO containing 1 role token
  -- of the given assetId. Furthermore, the transaction must send an output
  -- back to the same address with the all the assets, including the role
  -- token, as the spent UTXO.
  --
  -- Rules to check:
  --   1. The transaction consumes a UTXO that contains the necessary role
  --      token.
  --   2. The transaction sends the role token back to the address from which
  --      the UTXO in rule 1 was spent in a single output.
  --   3. The output in rule 2 includes all other assets contained in the UTXO
  --      in rule 1.
  --   4. The output in rule 2 covers the min UTXO requirement.
  deriving (Eq, Ord, Show)

-- | Require the transaction to mint 1 role token of the specified assetID and
-- send it to the given address, along with the min UTXO ADA.
mustMintRoleToken :: Core.IsMarloweVersion v => Chain.AssetId -> Chain.Address -> TxConstraints v
mustMintRoleToken assetId address = mempty { roleTokenConstraints = Set.singleton $ MintRoleToken assetId address }

-- | Require the transaction to spend a UTXO with 1 role token of the specified
-- assetID. It also needs to send an identical output (same assets) to the
-- address that held the spent UTXO.
mustSpendRoleToken :: Core.IsMarloweVersion v => Chain.AssetId -> TxConstraints v
mustSpendRoleToken assetId = mempty { roleTokenConstraints = Set.singleton $ SpendRoleToken assetId }

-- | Constraints on the outputs of the transaction.
data OutputConstraint v
  = PayAssets Chain.Assets Chain.Address
  -- ^ Specifies that the transaction must send the given assets to the given
  -- address.
  --
  -- Rules to check:
  --   1. The total assets sent to the address cover at least the specified amount.
  --   2. Let t be the transaction body, c be the constraint set, and p be the
  --      PayAssets constraint from c under consideration. If t satisfies c, then
  --      t' satisfies c', where t' is t with the required assets of p removed
  --      from the total output to the address of p and c' is c with p removed.
  | SendToMarloweScript Chain.Assets (Core.Datum v)
  -- ^ Specifies that the transaction must send the given assets and the given
  --   datum to a Marlowe script address of the correct version.
  --
  -- Rules to check:
  --   1. The transaction sends an output with the given assets and datum to a
  --      script address.
  --   2. The script address in rule 1 is in the script address set for the
  --      corresponding Marlowe version.
  | SendToPayoutScript Chain.Assets (Core.PayoutDatum v)
  -- ^ Specifies that the transaction must send the given assets and the given
  --   datum to the Payout script address associated with the Marlowe script
  --   address of the contract.
  --
  -- Rules to check:
  --   1. The transaction sends an output with the given assets and datum to a
  --      script address.
  --   2. The transaction consumes an output from a marlowe script address.
  --   2. The script address in rule 1 is the payout script address associated
  --      with the marlowe script address in rule 2.

-- | Require the transaction to send the specified assets to the address.
mustPayToAddress :: Core.IsMarloweVersion v => Chain.Assets -> Chain.Address -> TxConstraints v
mustPayToAddress assets address = mempty { outputConstraints = Set.singleton $ PayAssets assets address }

-- | Require the transaction to send an output to the marlowe script address
-- with the given assets and the given datum.
mustSendMarloweOutput :: Core.IsMarloweVersion v => Chain.Assets -> Core.Datum v -> TxConstraints v
mustSendMarloweOutput assets datum = mempty { outputConstraints = Set.singleton $ SendToMarloweScript assets datum }

-- | Require the transaction to send an output to the payout script address
-- with the given assets and the given datum.
mustSendPayoutOutput :: Core.IsMarloweVersion v => Chain.Assets -> Core.PayoutDatum v -> TxConstraints v
mustSendPayoutOutput assets datum = mempty { outputConstraints = Set.singleton $ SendToPayoutScript assets datum }

data InputConstraint v
  = NoInput
  -- ^ Specifies that the transaction consumes no marlowe or payout script
  -- inputs. Corresponds to the create transaction type.
  --
  -- Rules to check:
  --   1. All inputs of the transaction do not come from any of the script
  --      addresses (marlowe or payout) associated with marlowe version v.
  | MarloweInput Chain.TxOutRef P.POSIXTime P.POSIXTime (Core.Redeemer v)
  -- ^ Specifies that the transaction consumes the Marlowe UTXO with the given
  -- TxOutRef.
  --
  -- Rules to check:
  --   1. An input that matches the given TxOutRef is consumed.
  --   2. The input in rule 1 includes the given redeemer.
  --   3. The validity range of the transaction matches the given min and max
  --      validity bounds (converted to slots).
  --   4. The input in rule 1 comes from one of the marlowe script addresses
  --      associated with marlowe version v.
  --   5. All other inputs do not come from any of the marlowe or payout script
  --      addresses associated with marlowe version v.
  --   6. If there are any outputs to a script address associated with
  --      version v, then there must be only 1 such output and the address
  --      must match the address of the input from rule 1.
  | PayoutInputs (Core.PayoutDatum v)
  -- ^ Specifies that the transaction consumes all UTXOs from the given payout
  -- validator address with the given datum.
  --
  -- Rules to check:
  --   1. At least one UTXO is consumed that bears the correct payout datum.
  --   2. All such inputs that satisfy rule 1 come from the same address.
  --   3. The address from rule 2 exists in the set of payout validator
  --      addresses associated with marlowe version v.
  --   4. For all inputs i that do not satisfy rule 1, the address of i does
  --      not exist in the set of payout script addresses associated with
  --      Marlowe version v.
  --   5. For all inputs i, the address of i does not exist in the set of
  --      Marlowe script addresses associated with Marlowe version v.

-- | Require the transaction to consume an input from the Marlowe script with
-- the given validity interval and redeemer (input). Used for apply-inputs.
mustConsumeMarloweOutput :: Core.IsMarloweVersion v => Chain.TxOutRef -> P.POSIXTime -> P.POSIXTime -> Core.Redeemer v -> TxConstraints v
mustConsumeMarloweOutput utxo invalidBefore invalidHereafter inputs = mempty { inputConstraint = MarloweInput utxo invalidBefore invalidHereafter inputs }

-- | Require the transaction to consume any input from the payout script that
-- bear the given datum.
mustConsumePayouts :: Core.IsMarloweVersion v => Core.PayoutDatum v -> TxConstraints v
mustConsumePayouts payoutDatum = mempty { inputConstraint = PayoutInputs payoutDatum }

instance Core.IsMarloweVersion v => Show (OutputConstraint v) where
  showsPrec = case Core.marloweVersion @v of
    Core.MarloweV1 -> showsPrec

instance Core.IsMarloweVersion v => Eq (OutputConstraint v) where
  (==) = case Core.marloweVersion @v of
    Core.MarloweV1 -> (==)

instance Core.IsMarloweVersion v => Ord (OutputConstraint v) where
  compare = case Core.marloweVersion @v of
    Core.MarloweV1 -> compare

instance Core.IsMarloweVersion v => Show (InputConstraint v) where
  showsPrec = case Core.marloweVersion @v of
    Core.MarloweV1 -> showsPrec

instance Core.IsMarloweVersion v => Eq (InputConstraint v) where
  (==) = case Core.marloweVersion @v of
    Core.MarloweV1 -> (==)

instance Core.IsMarloweVersion v => Ord (InputConstraint v) where
  compare = case Core.marloweVersion @v of
    Core.MarloweV1 -> compare

instance Semigroup (InputConstraint v) where
  a <> NoInput = a
  _ <> b = b

instance Monoid (InputConstraint v) where
  mempty = NoInput

instance Core.IsMarloweVersion v => Semigroup (TxConstraints v) where
  a <> b = TxConstraints
    { inputConstraint = on (<>) inputConstraint a b
    , roleTokenConstraints = on (<>) roleTokenConstraints a b
    , outputConstraints = on (<>) outputConstraints a b
    , signatureConstraints = on (<>) signatureConstraints a b
    , metadataConstraints = on (<>) metadataConstraints a b
    }

instance Core.IsMarloweVersion v => Monoid (TxConstraints v) where
  mempty = TxConstraints
    { inputConstraint = mempty
    , roleTokenConstraints = mempty
    , outputConstraints = mempty
    , signatureConstraints = mempty
    , metadataConstraints = mempty
    }

-- | Determines if the given transaction body satisfies the given constraints.
satisfiesConstraints
  :: Cardano.ProtocolParameters
  -> Cardano.TxBody era
  -> TxConstraints v
  -> Bool
satisfiesConstraints = error "not implemented"

-- | Errors that can occur when trying to solve the constraints.
data UnsolvableConstraintsError

-- | Data from a wallet needed to solve the constraints.
data WalletContext = WalletContext
  { availableUtxos :: Map Chain.TxOutRef Chain.TransactionOutput
  -- ^ The UTXO set of the wallet that can be used for coin selection and
  -- satisfying constraints.
  , collateralUtxos :: Set Chain.TxOutRef
  -- ^ The subset of keys in 'availableUtxos' that may be used for collateral.
  , changeAddress :: Chain.Address
  -- ^ The change address of the wallet.
  }

-- | Given a set of constraints and the context of a wallet, produces a
-- balanced, unsigned transaction that satisfies the constraints.
--
-- law: satisfiesConstraints (solveConstraints ... constraints) constraints == True
-- law: makeTransactionBodyAutoBalance should return a balanced transaction on
-- the result.
solveConstraints
  :: Cardano.ScriptDataSupportedInEra era
  -> Cardano.SystemStart
  -> Cardano.EraHistory Cardano.CardanoMode
  -> Cardano.ProtocolParameters
  -> WalletContext
  -> TxConstraints v
  -> Either UnsolvableConstraintsError (Cardano.TxBody era)
solveConstraints = error "not implemented"
