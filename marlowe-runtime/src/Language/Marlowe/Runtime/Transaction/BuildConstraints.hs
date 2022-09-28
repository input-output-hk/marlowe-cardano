{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Transaction.BuildConstraints
  ( buildApplyInputsConstraints
  , buildCreateConstraints
  , buildWithdrawConstraints
  ) where

import qualified Data.Aeson as Aeson
import Data.Map (Map)
import Data.Time (UTCTime)
import Language.Marlowe.Runtime.ChainSync.Api (Address, AssetId, SlotConfig, TokenName)
import Language.Marlowe.Runtime.Core.Api
  (Contract, MarloweVersion(MarloweV1), MarloweVersionTag(V1), PayoutDatum, Redeemer, TransactionScriptOutput)
import Language.Marlowe.Runtime.Transaction.Api (ApplyInputsError, CreateError, WithdrawError)
import Language.Marlowe.Runtime.Transaction.Constraints (TxConstraints(..))
import qualified Language.Marlowe.Runtime.Transaction.Constraints as TxConstraints

-- | Creates a set of Tx constraints that are used to build a transaction that
-- instantiates a contract.
buildCreateConstraints
  :: MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> Map TokenName Address -- ^ The initial distribution of the role tokens.
  -> Map Int Aeson.Value -- ^ Extra metadata to add to the transaction.
  -> Contract v -- ^ The contract being instantiated.
  -> Either (CreateError v) (TxConstraints v)
buildCreateConstraints = error "not implemented"

-- | Creates a set of Tx constraints that are used to build a transaction that
-- applies an input to a contract.
buildApplyInputsConstraints
  :: SlotConfig -- ^ The slot config used to convert the validity interval to slots.
  -> MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> TransactionScriptOutput v -- ^ The previous script output for the contract
  -> Maybe UTCTime -- ^ The minimum bound of the validity interval (inclusive).
                   -- If not specified, the current time is used.
  -> Maybe UTCTime -- ^ The maximum bound of the validity interval (exclusive).
                   -- If not specified, this is computed from the the timeouts
                   -- in the contract.
  -> Redeemer v -- ^ The inputs to apply to the contract.
  -> Either (ApplyInputsError v) (TxConstraints v)
buildApplyInputsConstraints = error "not implemented"

-- | Creates a set of Tx constraints that are used to build a transaction that
-- withdraws payments from a payout validator.
buildWithdrawConstraints
  :: MarloweVersion v -- ^ The Marlowe version to build the transaction for.
  -> PayoutDatum v -- ^ The role token from which to withdraw funds.
  -> Either (WithdrawError v) (TxConstraints v)
buildWithdrawConstraints = \case
  MarloweV1 -> Right . buildWithdrawConstraintsV1
  where
    buildWithdrawConstraintsV1 :: AssetId -> TxConstraints 'V1
    buildWithdrawConstraintsV1 =
      TxConstraints.mustConsumePayouts <> TxConstraints.mustSpendRoleToken
