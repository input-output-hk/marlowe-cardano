{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Discovery.Api
  where

import Data.Aeson (ToJSON)
import Data.Binary (Binary)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (Address, BlockHeader, PolicyId, ScriptHash)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweTransactionMetadata, SomeMarloweVersion)
import Network.Protocol.Codec.Spec (Variations)

-- | A Marlowe contract header is a compact structure that contains all the
-- significant metadata related to a Marlowe Contract on chain.
data ContractHeader = ContractHeader
  { contractId :: ContractId
  -- ^ The ID of the Marlowe contract instance.
  , rolesCurrency :: PolicyId
  -- ^ The ID of the minting policy used to mint the role tokens.
  , metadata :: MarloweTransactionMetadata
  -- ^ Any custom metadata attached to the contract's creation transaction.
  , marloweScriptHash :: ScriptHash
  -- ^ The hash of the validator script.
  , marloweScriptAddress :: Address
  -- ^ The address of the validator script.
  , payoutScriptHash :: ScriptHash
  -- ^ The address of the payout validator script.
  , marloweVersion :: SomeMarloweVersion
  -- ^ The version of the contract.
  , blockHeader :: BlockHeader
  -- ^ The header of the block in which the contract instance was published.
  } deriving (Show, Eq, Ord, Generic, Binary)

instance ToJSON ContractHeader
instance Variations ContractHeader
