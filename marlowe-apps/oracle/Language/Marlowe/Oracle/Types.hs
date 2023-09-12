{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Oracle.Types (
  OracleRequest (..),
  choiceName',
) where

import Data.Aeson (ToJSON (..), object, (.=))
import Language.Marlowe.Core.V1.Semantics.Types (Bound, ChoiceName, Contract)
import PlutusLedgerApi.V2 (BuiltinByteString, fromBuiltin)

import qualified Data.ByteString.Base16.Aeson as Base16.Aeson (byteStringToJSON)
import Data.ByteString.Char8 as BS8 (unpack)

data OracleRequest = OracleRequest
  { choiceName :: ChoiceName
  , bounds :: [Bound]
  , continuation :: Either BuiltinByteString Contract
  }
  deriving (Eq, Ord, Show)

instance ToJSON OracleRequest where
  toJSON OracleRequest{..} =
    case continuation of
      Right continuation' ->
        object
          [ "symbol" .= BS8.unpack (fromBuiltin choiceName)
          , "bounds" .= bounds
          , "continuation" .= continuation'
          ]
      Left continuation' ->
        object
          [ "symbol" .= BS8.unpack (fromBuiltin choiceName)
          , "bounds" .= bounds
          , "merkleizedContinuation" .= Base16.Aeson.byteStringToJSON (fromBuiltin continuation')
          ]

choiceName'
  :: OracleRequest
  -> String
choiceName' = BS8.unpack . fromBuiltin . choiceName
