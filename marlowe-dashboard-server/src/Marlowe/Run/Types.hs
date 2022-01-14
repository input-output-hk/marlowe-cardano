{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}

module Marlowe.Run.Types where

import Cardano.Prelude
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import qualified Data.Map as M
import Plutus.V1.Ledger.Api (CurrencySymbol (unCurrencySymbol), TokenName (unTokenName), Value (..))
import qualified PlutusTx.AssocMap as AM
import qualified PlutusTx.Builtins as BI
import PlutusTx.Prelude (fromBuiltin)
import Servant.Client (ClientEnv)

type Env = ClientEnv

newtype CurrencySymbolDto = CurrencySymbolDto Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON , FromJSONKey, ToJSON, ToJSONKey)

newtype TokenNameDto = TokenNameDto Text
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON , FromJSONKey, ToJSON, ToJSONKey)

type ValueDto = M.Map CurrencySymbolDto (M.Map TokenNameDto Integer)

assocMapToDto :: Ord k' => (k -> k') -> (v -> v') -> AM.Map k v -> M.Map k' v'
assocMapToDto f g = M.fromList . fmap (bimap f g) . AM.toList

valueToDto :: Value -> ValueDto
valueToDto = assocMapToDto currencySymbolToDto (assocMapToDto tokenNameToDto identity) . getValue

currencySymbolToDto :: CurrencySymbol -> CurrencySymbolDto
currencySymbolToDto = CurrencySymbolDto . fromBuiltin . BI.decodeUtf8 . unCurrencySymbol

tokenNameToDto :: TokenName -> TokenNameDto
tokenNameToDto = TokenNameDto . fromBuiltin . BI.decodeUtf8 . unTokenName
