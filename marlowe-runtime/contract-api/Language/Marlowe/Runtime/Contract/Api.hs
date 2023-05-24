{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.Api
  where

import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Data (type (:~:)(..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Core.Api ()
import Network.Protocol.Codec.Spec (Variations(variations), varyAp)
import Network.Protocol.Handshake.Types (HasSignature, signature)
import Network.Protocol.Query.Client (QueryClient, request)
import Network.Protocol.Query.Types

data ContractRequest a where
  GetContract :: DatumHash -> ContractRequest (Maybe ContractWithAdjacency)
  GetMerkleizedInputs
    :: DatumHash
    -> State
    -> TimeInterval
    -> [InputContent]
    -> ContractRequest (Either GetMerkleizedInputsError [Input])

deriving instance Show (ContractRequest a)
deriving instance Eq (ContractRequest a)

data GetMerkleizedInputsError
  = GetMerkleizedInputsContractNotFound DatumHash
  | GetMerkleizedInputsApplyNoMatch InputContent
  | GetMerkleizedInputsApplyAmbiguousInterval InputContent
  | GetMerkleizedInputsIntervalError IntervalError
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Binary, Variations)

getContract :: Applicative m => DatumHash -> QueryClient ContractRequest m (Maybe ContractWithAdjacency)
getContract = request . GetContract

getMerkleizedInputs
  :: Applicative m
  => DatumHash
  -> State
  -> TimeInterval
  -> [InputContent]
  -> QueryClient ContractRequest m (Either GetMerkleizedInputsError [Input])
getMerkleizedInputs = (fmap . fmap . fmap) request . GetMerkleizedInputs

instance HasSignature ContractRequest where
  signature _ = "ContractRequest"

instance Request ContractRequest where
  data Tag ContractRequest a where
    TagGetContract :: Tag ContractRequest (Maybe ContractWithAdjacency)
    TagGetMerkleizedInputs :: Tag ContractRequest (Either GetMerkleizedInputsError [Input])
  tagFromReq = \case
    GetContract{} -> TagGetContract
    GetMerkleizedInputs{} -> TagGetMerkleizedInputs
  tagEq = \case
    TagGetContract -> \case
      TagGetContract -> Just Refl
      _ -> Nothing
    TagGetMerkleizedInputs -> \case
      TagGetMerkleizedInputs -> Just Refl
      _ -> Nothing

deriving instance Show (Tag ContractRequest a)
deriving instance Eq (Tag ContractRequest a)
deriving instance Ord (Tag ContractRequest a)

instance BinaryRequest ContractRequest where
  putReq = \case
    GetContract hash -> do
      putWord8 0x00
      put hash
    GetMerkleizedInputs hash state timeInterval inputs -> do
      putWord8 0x01
      put hash
      put state
      put timeInterval
      put inputs
  getReq = do
    tag <- getWord8
    case tag of
      0x00 -> SomeRequest . GetContract <$> get
      0x01 -> SomeRequest <$> (GetMerkleizedInputs <$> get <*> get <*> get <*> get)
      _ -> fail $ "Invalid ContractRequest tag " <> show tag
  putResult = \case
    TagGetContract -> put
    TagGetMerkleizedInputs -> put
  getResult = \case
    TagGetContract -> get
    TagGetMerkleizedInputs -> get

instance ShowRequest ContractRequest where
  showsPrecResult p = \case
    TagGetContract -> showsPrec p
    TagGetMerkleizedInputs -> showsPrec p

instance OTelRequest ContractRequest where
  reqTypeName _ = "contract_request"
  reqName = \case
    TagGetContract -> "get_contract"
    TagGetMerkleizedInputs -> "get_merkleized_inputs"

instance RequestVariations ContractRequest where
  tagVariations = NE.fromList
    [ SomeTag TagGetContract
    ]
  requestVariations = \case
    TagGetContract -> GetContract <$> variations
    TagGetMerkleizedInputs -> GetMerkleizedInputs <$> variations `varyAp` variations `varyAp` variations `varyAp` variations
  resultVariations = \case
    TagGetContract -> variations
    TagGetMerkleizedInputs -> variations

instance RequestEq ContractRequest where
  resultEq = \case
    TagGetContract -> (==)
    TagGetMerkleizedInputs -> (==)

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
