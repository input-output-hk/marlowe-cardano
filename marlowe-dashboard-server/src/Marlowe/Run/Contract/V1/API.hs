{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Marlowe.Run.Contract.V1.API
    ( API
    , ApiT(..)
    ) where

import Prelude

import Cardano.Api (AddressInEra, AlonzoEra, SerialiseAddress (serialiseAddress))
import Data.Aeson (ToJSON, Value (String), object)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Marlowe.Run.Contract.V1.Types (RoleToken (..))
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..))
import PlutusTx.Builtins.Class (FromBuiltin (..), ToBuiltin (..))
import Servant.API (Capture, FromHttpApiData (..), Get, JSON, (:>))

type API =
    (Capture "currency-symbol" (ApiT CurrencySymbol)
        :> "role-tokens"
        :> Capture "token-name" (ApiT TokenName)
        :> Get '[JSON] (ApiT RoleToken))

newtype ApiT a = ApiT { unApiT :: a }

instance FromHttpApiData (ApiT CurrencySymbol) where
  parseUrlPiece = fmap (ApiT . CurrencySymbol . toBuiltin) . decodeBase16 . encodeUtf8

instance ToJSON (ApiT CurrencySymbol) where
  toJSON = String . encodeBase16 . fromBuiltin . unCurrencySymbol . unApiT

instance ToJSON (ApiT TokenName) where
  toJSON = String . decodeUtf8 . fromBuiltin . unTokenName . unApiT

instance FromHttpApiData (ApiT TokenName) where
  parseUrlPiece = pure . ApiT . TokenName . toBuiltin . encodeUtf8

instance ToJSON (ApiT (AddressInEra AlonzoEra)) where
  toJSON = String . serialiseAddress . unApiT

instance ToJSON (ApiT RoleToken) where
  toJSON (ApiT RoleToken{..}) = object
    [ ("currencySymbol", toJSON $ ApiT _roleTokenCurrencySymbol)
    , ("tokenName", toJSON $ ApiT _roleTokenTokenName)
    , ("utxoAddress", toJSON $ ApiT _roleTokenUtxoAddress)
    ]
