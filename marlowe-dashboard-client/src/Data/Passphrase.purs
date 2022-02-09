module Data.Passpharse
  ( Passphrase
  , fromString
  , toString
  ) where

import Prologue

import Data.String (trim)

newtype Passphrase = Passphrase String

fromString :: String -> Maybe Passphrase
fromString s =
  if trim s /= s then Nothing
  else Just $ Passphrase s

toString :: Passphrase -> String
toString (Passphrase s) = s

