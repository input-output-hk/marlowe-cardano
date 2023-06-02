{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.Api where

import Data.Binary (Binary, get, getWord8, put, putWord8)
import Data.Data (type (:~:)(..))
import qualified Data.List.NonEmpty as NE
import Data.Set (Set)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Core.Api ()
import Network.Protocol.Codec.Spec (Variations(variations))
import Network.Protocol.Handshake.Types (HasSignature, signature)
import Network.Protocol.Query.Client (QueryClient, request)
import Network.Protocol.Query.Types

data ContractRequest a where
  GetContract :: DatumHash -> ContractRequest (Maybe ContractWithAdjacency)

getContract :: Applicative m => DatumHash -> QueryClient ContractRequest m (Maybe ContractWithAdjacency)
getContract = request . GetContract

deriving instance Show (ContractRequest a)
deriving instance Eq (ContractRequest a)
deriving instance Ord (ContractRequest a)

instance HasSignature ContractRequest where
  signature _ = "ContractRequest"

instance Request ContractRequest where
  data Tag ContractRequest a where
    TagGetContract :: Tag ContractRequest (Maybe ContractWithAdjacency)
  tagFromReq = \case
    GetContract{} -> TagGetContract
  tagEq = \case
    TagGetContract -> \case
      TagGetContract -> Just Refl

deriving instance Show (Tag ContractRequest a)
deriving instance Eq (Tag ContractRequest a)
deriving instance Ord (Tag ContractRequest a)

instance BinaryRequest ContractRequest where
  putReq = \case
    GetContract hash -> do
      putWord8 0x00
      put hash
  getReq = do
    tag <- getWord8
    case tag of
      0x00 -> SomeRequest . GetContract <$> get
      _ -> fail $ "Invalid ContractRequest tag " <> show tag
  putResult = \case
    TagGetContract -> put
  getResult = \case
    TagGetContract -> get

instance ShowRequest ContractRequest where
  showsPrecResult p = \case
    TagGetContract -> showsPrec p

instance OTelRequest ContractRequest where
  reqTypeName _ = "contract_request"
  reqName = \case
    TagGetContract -> "get_contract"

instance RequestVariations ContractRequest where
  tagVariations = NE.fromList
    [ SomeTag TagGetContract
    ]
  requestVariations = \case
    TagGetContract -> GetContract <$> variations
  resultVariations = \case
    TagGetContract -> variations

instance RequestEq ContractRequest where
  resultEq = \case
    TagGetContract -> (==)

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
