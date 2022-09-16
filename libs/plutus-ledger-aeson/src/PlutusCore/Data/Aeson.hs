{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusCore.Data.Aeson
  where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson.ViaSerialise as Aeson
import PlutusCore.Data (Data)

deriving via (Aeson.ViaSerialise Data) instance ToJSON Data
deriving via (Aeson.ViaSerialise Data) instance FromJSON Data
