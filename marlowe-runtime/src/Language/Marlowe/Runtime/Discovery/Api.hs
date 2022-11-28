{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Discovery.Api
  where

import Data.Aeson (ToJSON)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Map (Map)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void, absurd)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (Address, BlockHeader, Metadata, PolicyId, ScriptHash)
import Language.Marlowe.Runtime.Core.Api (ContractId, SomeMarloweVersion)
import Network.Protocol.Query.Types (IsQuery(..), SomeTag(..))

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
  -- ^ The address of the payout validator script.
  , marloweVersion :: SomeMarloweVersion
  -- ^ The version of the contract.
  , blockHeader :: BlockHeader
  -- ^ The header of the block in which the contract instance was published.
  } deriving (Show, Eq, Ord, Generic, Binary)

instance ToJSON ContractHeader

-- | The queries supported by the discovery service.
data DiscoveryQuery delimiter err result where
  -- | Query the full list of contract headers. Results are delivered in pages
  -- and use a unit value to mark the next page.
  GetContractHeaders :: DiscoveryQuery () Void [ContractHeader]
  -- | Query all contract headers that use the given minting policy ID for role
  -- tokens.
  GetContractHeadersByRoleTokenCurrency :: PolicyId -> DiscoveryQuery Void Void [ContractHeader]

instance IsQuery DiscoveryQuery where
  data Tag DiscoveryQuery delimiter err result where
    TagGetContractHeaders :: Tag DiscoveryQuery () Void [ContractHeader]
    TagGetContractHeadersByRoleTokenCurrency :: Tag DiscoveryQuery Void Void [ContractHeader]
  tagFromQuery = \case
    GetContractHeaders -> TagGetContractHeaders
    GetContractHeadersByRoleTokenCurrency _ -> TagGetContractHeadersByRoleTokenCurrency
  tagEq TagGetContractHeaders TagGetContractHeaders = Just (Refl, Refl, Refl)
  tagEq TagGetContractHeaders _ = Nothing
  tagEq TagGetContractHeadersByRoleTokenCurrency TagGetContractHeadersByRoleTokenCurrency = Just (Refl, Refl, Refl)
  tagEq TagGetContractHeadersByRoleTokenCurrency _ = Nothing
  putTag = \case
    TagGetContractHeaders -> putWord8 0x01
    TagGetContractHeadersByRoleTokenCurrency -> putWord8 0x02
  getTag = getWord8 >>= \case
    0x01 -> pure $ SomeTag TagGetContractHeaders
    0x02 -> pure $ SomeTag TagGetContractHeadersByRoleTokenCurrency
    tag -> fail $ "Invalid query tag value " <> show tag
  putQuery = \case
    GetContractHeaders -> mempty
    GetContractHeadersByRoleTokenCurrency policyId -> put policyId
  getQuery = \case
    TagGetContractHeaders -> pure GetContractHeaders
    TagGetContractHeadersByRoleTokenCurrency -> GetContractHeadersByRoleTokenCurrency <$> get
  putDelimiter = \case
    TagGetContractHeaders -> put
    TagGetContractHeadersByRoleTokenCurrency -> absurd
  getDelimiter = \case
    TagGetContractHeaders -> get
    TagGetContractHeadersByRoleTokenCurrency -> fail "No delimiter defined"
  putErr = \case
    TagGetContractHeaders -> absurd
    TagGetContractHeadersByRoleTokenCurrency -> absurd
  getErr = \case
    TagGetContractHeaders -> fail "No error defined"
    TagGetContractHeadersByRoleTokenCurrency -> fail "No error defined"
  putResult = \case
    TagGetContractHeaders -> put
    TagGetContractHeadersByRoleTokenCurrency -> put
  getResult = \case
    TagGetContractHeaders -> get
    TagGetContractHeadersByRoleTokenCurrency -> get
