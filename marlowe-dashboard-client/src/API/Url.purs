module API.Url
  ( URLPiece
  , class ToUrlPiece
  , toUrlPiece
  ) where

import Wallet.Emulator.Wallet (Wallet(..))

type URLPiece
  = String

-- servant-purescript provides a ToUrlPiece class, but it doesn't work as we need it to
-- for our generated data types
class ToUrlPiece a where
  toUrlPiece :: a -> URLPiece

instance stringToUrlPiece :: ToUrlPiece String where
  toUrlPiece str = str

instance walletToUrlPiece :: ToUrlPiece Wallet where
  toUrlPiece (Wallet { getWalletId }) = getWalletId
