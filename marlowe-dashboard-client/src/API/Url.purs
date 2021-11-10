module API.Url
  ( URLPiece
  , class ToUrlPiece
  , toUrlPiece
  ) where

import Data.UUID (toString) as UUID
import Data.UUID.Argonaut (UUID(..))
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractInstanceId(..))

type URLPiece = String

-- servant-purescript provides a ToUrlPiece class, but it doesn't work as we need it to
-- for our generated data types
class ToUrlPiece a where
  toUrlPiece :: a -> URLPiece

instance contractInstanceIdToUrlPiece :: ToUrlPiece ContractInstanceId where
  toUrlPiece (ContractInstanceId { unContractInstanceId: UUID uuid }) =
    UUID.toString uuid

instance walletToUrlPiece :: ToUrlPiece Wallet where
  toUrlPiece (Wallet { getWalletId }) = getWalletId
