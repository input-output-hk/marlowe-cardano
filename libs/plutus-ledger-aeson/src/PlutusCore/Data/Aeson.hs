{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusCore.Data.Aeson where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.ViaSerialise qualified as Aeson
import PlutusCore.Data (Data)

deriving via (Aeson.ViaSerialise Data) instance ToJSON Data
deriving via (Aeson.ViaSerialise Data) instance FromJSON Data
