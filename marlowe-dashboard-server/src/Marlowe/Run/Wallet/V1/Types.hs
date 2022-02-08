{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
module Marlowe.Run.Wallet.V1.Types where

import Cardano.Prelude

import qualified Cardano.Wallet.Primitive.Types as Cardano
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T
import Data.Text.Class (FromText (..), TextDecodingError (..), ToText (..))
import Ledger.Address (PaymentPubKeyHash)
import Servant (FromHttpApiData (..))

newtype WalletId = WalletId { unWalletId :: Cardano.WalletId }
    deriving (Eq, Ord, Show)

instance Aeson.ToJSON WalletId where
    toJSON = Aeson.toJSON . toText . unWalletId

instance Aeson.FromJSON WalletId where
    parseJSON value = do
        walletIdText <- Aeson.parseJSON value
        case fromText walletIdText of
          Left e   -> parseFail $ getTextDecodingError e
          Right id -> pure $ WalletId id

instance FromHttpApiData WalletId where
    parseUrlPiece = bimap (T.pack . getTextDecodingError) WalletId . fromText

newtype WalletName = WalletName { unWalletName :: Cardano.WalletName }
    deriving (Eq, Show)

instance Aeson.ToJSON WalletName where
    toJSON = Aeson.toJSON . toText . unWalletName

instance Aeson.FromJSON WalletName where
    parseJSON value = do
        walletNameText <- Aeson.parseJSON value
        case fromText walletNameText of
          Left e           -> parseFail $ getTextDecodingError e
          Right walletName -> pure $ WalletName walletName

data WalletInfo = WalletInfo
    { walletId   :: WalletId
    , pubKeyHash :: PaymentPubKeyHash
    }
    deriving stock (Generic, Eq, Show)
    deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)
