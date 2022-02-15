module Data.Address.Bech32 where

import Prologue

import Data.Address.Bech32.DataPart (Bech32DataPart)
import Data.Address.Bech32.DataPart as DP
import Data.Address.Bech32.HRP (Bech32HRP)
import Data.Address.Bech32.HRP as HRP
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', lens)
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split)

data Bech32Address = Bech32Address Bech32HRP Bech32DataPart

derive instance Generic Bech32Address _
derive instance Eq Bech32Address
derive instance Ord Bech32Address
instance Show Bech32Address where
  show = genericShow

fromString :: String -> Maybe Bech32Address
fromString = split (Pattern "1") >>> case _ of
  [ hrpString, dataString ] ->
    Bech32Address <$> HRP.fromString hrpString <*> DP.fromString dataString
  _ -> Nothing

toString :: Bech32Address -> String
toString (Bech32Address hrp dp) = HRP.toString hrp <> "1" <> DP.toString dp

_hrp :: Lens' Bech32Address Bech32HRP
_hrp = lens get set
  where
  get (Bech32Address hrp _) = hrp
  set (Bech32Address _ dp) hrp = Bech32Address hrp dp

_dataPart :: Lens' Bech32Address Bech32DataPart
_dataPart = lens get set
  where
  get (Bech32Address _ dp) = dp
  set (Bech32Address hrp _) dp = Bech32Address hrp dp
