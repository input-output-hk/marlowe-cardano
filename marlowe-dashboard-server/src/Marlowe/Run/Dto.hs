{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Marlowe.Run.Dto where

import Cardano.Prelude
import Cardano.Wallet.Primitive.Types (WalletId)
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Map as M
import Data.Text.Class (FromText (fromText), ToText (toText))
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..), Value (..))
import qualified PlutusTx.AssocMap as AM
import PlutusTx.Prelude (BuiltinByteString, fromBuiltin, toBuiltin)
import qualified PlutusTx.Prelude as BI
import Servant (FromHttpApiData, ToHttpApiData)
import Servant.Server (ServerError, err400)

class ToDto a dto where
    toDto :: a -> dto

class FromDto a dto where
    fromDto :: dto -> Maybe a

instance ToDto Integer Integer where
    toDto = identity

instance FromDto Integer Integer where
    fromDto = Just

instance {-# OVERLAPPING #-} FromText a => FromDto a Text where
    fromDto = either (const Nothing) Just . fromText

instance {-# OVERLAPPING #-} ToText a => ToDto a Text where
    toDto = toText

instance (Ord k', ToJSONKey k', ToJSON v', ToDto k k', ToDto v v') => ToDto (AM.Map k v) (M.Map k' v') where
    toDto = M.fromList . fmap (bimap toDto toDto) . AM.toList

instance {-# OVERLAPPING #-} ToDto BuiltinByteString Text where
    toDto = fromBuiltin . BI.decodeUtf8

instance {-# OVERLAPPING #-} FromDto BuiltinByteString Text where
    fromDto = Just . BI.encodeUtf8 . toBuiltin

newtype CurrencySymbolDto = CurrencySymbolDto Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON , FromJSONKey, ToJSON, ToJSONKey)

instance ToDto CurrencySymbol CurrencySymbolDto where
    toDto = CurrencySymbolDto . toDto . unCurrencySymbol

instance FromDto CurrencySymbol CurrencySymbolDto where
    fromDto (CurrencySymbolDto t) = CurrencySymbol <$> fromDto t

newtype TokenNameDto = TokenNameDto Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON , FromJSONKey, ToJSON, ToJSONKey)

instance ToDto TokenName TokenNameDto where
    toDto = TokenNameDto . toDto . unTokenName

instance FromDto TokenName TokenNameDto where
    fromDto (TokenNameDto t) = TokenName <$> fromDto t

newtype WalletIdDto = WalletIdDto Text
    deriving (Show, Eq, Ord, Generic)
    deriving newtype (FromHttpApiData, ToHttpApiData, FromJSON , FromJSONKey, ToJSON, ToJSONKey)

instance ToDto WalletId WalletIdDto where
    toDto = WalletIdDto . toDto

instance FromDto WalletId WalletIdDto where
    fromDto (WalletIdDto t) = fromDto t

newtype AssetsDto = AssetsDto (M.Map CurrencySymbolDto (M.Map TokenNameDto Integer))
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON , FromJSONKey, ToJSON, ToJSONKey)

instance ToDto Value AssetsDto where
    toDto = AssetsDto . toDto . getValue

dtoHandler :: (MonadError ServerError m, FromDto a req, ToDto b res) => (a -> m b) -> req -> m res
dtoHandler f req = case fromDto req of
    Nothing -> throwError err400
    Just a  -> toDto <$> f a
