{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.Api where

import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Data (type (:~:) (..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics (TransactionInput)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Core.Api ()
import Network.Protocol.Codec.Spec (Variations (variations), varyAp)
import Network.Protocol.Handshake.Types (HasSignature, signature)
import Network.Protocol.Query.Client (QueryClient, request)
import Network.Protocol.Query.Types

data ContractRequest a where
  GetContract :: DatumHash -> ContractRequest (Maybe ContractWithAdjacency)
  MerkleizeInputs
    :: Contract
    -> State
    -> TransactionInput
    -> ContractRequest (Either MerkleizeInputsError TransactionInput)

deriving instance Show (ContractRequest a)
deriving instance Eq (ContractRequest a)

data MerkleizeInputsError
  = MerkleizeInputsContractNotFound DatumHash
  | MerkleizeInputsApplyNoMatch Input
  | MerkleizeInputsApplyAmbiguousInterval Input
  | MerkleizeInputsReduceAmbiguousInterval Input
  | MerkleizeInputsIntervalError IntervalError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, Variations)

getContract :: (Applicative m) => DatumHash -> QueryClient ContractRequest m (Maybe ContractWithAdjacency)
getContract = request . GetContract

merkleizeInputs
  :: (Applicative m)
  => Contract
  -> State
  -> TransactionInput
  -> QueryClient ContractRequest m (Either MerkleizeInputsError TransactionInput)
merkleizeInputs = (fmap . fmap) request . MerkleizeInputs

instance HasSignature ContractRequest where
  signature _ = "ContractRequest"

instance Request ContractRequest where
  data Tag ContractRequest a where
    TagGetContract :: Tag ContractRequest (Maybe ContractWithAdjacency)
    TagMerkleizeInputs :: Tag ContractRequest (Either MerkleizeInputsError TransactionInput)
  tagFromReq = \case
    GetContract{} -> TagGetContract
    MerkleizeInputs{} -> TagMerkleizeInputs
  tagEq = \case
    TagGetContract -> \case
      TagGetContract -> Just Refl
      _ -> Nothing
    TagMerkleizeInputs -> \case
      TagMerkleizeInputs -> Just Refl
      _ -> Nothing

deriving instance Show (Tag ContractRequest a)
deriving instance Eq (Tag ContractRequest a)
deriving instance Ord (Tag ContractRequest a)

instance BinaryRequest ContractRequest where
  putReq = \case
    GetContract hash -> do
      putWord8 0x00
      put hash
    MerkleizeInputs hash state input -> do
      putWord8 0x01
      put hash
      put state
      put input
  getReq = do
    tag <- getWord8
    case tag of
      0x00 -> SomeRequest . GetContract <$> get
      0x01 -> SomeRequest <$> (MerkleizeInputs <$> get <*> get <*> get)
      _ -> fail $ "Invalid ContractRequest tag " <> show tag
  putResult = \case
    TagGetContract -> put
    TagMerkleizeInputs -> put
  getResult = \case
    TagGetContract -> get
    TagMerkleizeInputs -> get

instance ShowRequest ContractRequest where
  showsPrecResult p = \case
    TagGetContract -> showsPrec p
    TagMerkleizeInputs -> showsPrec p

instance OTelRequest ContractRequest where
  reqTypeName _ = "contract_request"
  reqName = \case
    TagGetContract -> "get_contract"
    TagMerkleizeInputs -> "get_merkleized_inputs"

instance RequestVariations ContractRequest where
  tagVariations =
    NE.fromList
      [ SomeTag TagGetContract
      ]
  requestVariations = \case
    TagGetContract -> GetContract <$> variations
    TagMerkleizeInputs -> MerkleizeInputs <$> variations `varyAp` variations `varyAp` variations
  resultVariations = \case
    TagGetContract -> variations
    TagMerkleizeInputs -> variations

instance RequestEq ContractRequest where
  resultEq = \case
    TagGetContract -> (==)
    TagMerkleizeInputs -> (==)

-- | A contract with its adjacency and closure information.
data ContractWithAdjacency = ContractWithAdjacency
  { contractHash :: DatumHash
  -- ^ The hash of the contract (script datum hash)
  , contract :: Contract
  -- ^ The contract.
  , adjacency :: Set DatumHash
  -- ^ The set of continuation hashes explicitly contained in the contract.
  , closure :: Set DatumHash
  -- ^ The set of hashes contained in the contract and all recursive continuations of the contract.
  -- includes the hash of the contract its self.
  -- Does not contain the hash of the close contract.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Variations)
