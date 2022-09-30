module Language.Marlowe.Runtime.Discovery.Api
  where

import Data.Map (Map)
import Data.Word (Word64)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Core.Api

-- | A Marlowe contract header is a compact structure that contains all the
-- significant metadata related to a Marlowe Contract on chain.
data ContractHeader = ContractHeader
  { contractId :: ContractId
  -- ^ The ID of the Marlowe contract instance.
  , rolesCurrency :: PolicyId
  -- ^ The ID of the minting policy used to mint the role tokens.
  , metadata :: Map Word64 Metadata
  -- ^ Any custom metadata attached to the contract's creation transaction.
  , marloweScriptHash :: ScriptHash
  -- ^ The hash of the validator script.
  , marloweScriptAddress :: Address
  -- ^ The address of the validator script.
  , payoutScriptHash :: ScriptHash
  -- ^ The hash of the payout validator script.
  , payoutScriptAddress :: Address
  -- ^ The address of the payout validator script.
  , marloweVersion :: SomeMarloweVersion
  -- ^ The version of the contract.
  }
