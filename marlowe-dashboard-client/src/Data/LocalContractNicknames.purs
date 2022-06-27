module Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  , getContractNickname
  , insertContractNickname
  ) where

import Prologue

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.ContractNickname (ContractNickname)
import Data.Lens (Lens', iso, over, to, view)
import Data.Map (Map)
import Data.Map as Map
import Language.Marlowe.Core.V1.Semantics.Types (MarloweParams)

newtype LocalContractNicknames = LocalContractNicknames
  (Map MarloweParams ContractNickname)

derive instance Eq LocalContractNicknames
derive newtype instance EncodeJson LocalContractNicknames
derive newtype instance DecodeJson LocalContractNicknames

_LocalContractNicknames
  :: Lens' LocalContractNicknames (Map MarloweParams ContractNickname)
_LocalContractNicknames = iso
  (\(LocalContractNicknames m) -> m)
  (\m -> LocalContractNicknames m)

emptyLocalContractNicknames :: LocalContractNicknames
emptyLocalContractNicknames = LocalContractNicknames Map.empty

insertContractNickname
  :: MarloweParams
  -> ContractNickname
  -> LocalContractNicknames
  -> LocalContractNicknames
insertContractNickname marloweParams nickname = over _LocalContractNicknames $
  Map.insert marloweParams nickname

getContractNickname
  :: MarloweParams -> LocalContractNicknames -> Maybe ContractNickname
getContractNickname marloweParams = view
  (_LocalContractNicknames <<< to (Map.lookup marloweParams))
