module Data.PubKeyHash
  ( PubKeyHash
  , _PubKeyHash
  , fromString
  , toString
  ) where

import Prologue

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , decodeJson
  , encodeJson
  , (.:)
  )
import Data.Lens (Iso', iso)
import Data.ShowUtils (parens)

newtype PubKeyHash = PubKeyHash String

derive instance Eq PubKeyHash
derive instance Ord PubKeyHash
instance Show PubKeyHash where
  show = parens <<< append "fromString " <<< toString

instance EncodeJson PubKeyHash where
  encodeJson (PubKeyHash getPubKeyHash) =
    encodeJson { getPubKeyHash }

instance DecodeJson PubKeyHash where
  decodeJson json = do
    obj <- decodeJson json
    s <- obj .: "getPubKeyHash"
    pure $ PubKeyHash s

fromString :: String -> PubKeyHash
fromString = PubKeyHash

toString :: PubKeyHash -> String
toString (PubKeyHash s) = s

_PubKeyHash :: Iso' PubKeyHash String
_PubKeyHash = iso toString fromString
