-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Orphan instances for the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Language.Marlowe.CLI.Orphans (
) where


import Cardano.Api (AddressAny (..), AsType (AsAddressAny), deserialiseAddress)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (String), object, withObject, withText, (.:), (.=))
import Data.ByteString.Short (ShortByteString, fromShort, toShort)
import Language.Marlowe.Semantics (Payment (..), TransactionOutput (..))

import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString.Char8 as BS8 (pack, unpack)
import qualified Data.Text as T (pack, unpack)


instance ToJSON ShortByteString where
  toJSON = toJSON . BS8.unpack . Base16.encode . fromShort

instance FromJSON ShortByteString where
  parseJSON =
    withText "ShortByteString"
      $ \t ->
        case Base16.decode . BS8.pack $ T.unpack t of
          Right bytes   -> pure $ toShort bytes
          Left  message -> fail message


instance ToJSON TransactionOutput where
  toJSON TransactionOutput{..} =
    object
      [
        "payments" .= toJSON txOutPayments
      , "state"    .= toJSON txOutState
      , "contract" .= toJSON txOutContract
      , "warnings" .= toJSON txOutWarnings
      ]
  toJSON (Error message) =
    object
      [
        "error" .= toJSON message
      ]


instance ToJSON Payment where
  toJSON (Payment accountId payee money) =
    object
      [
        "accountId" .= toJSON accountId
      , "payee"     .= toJSON payee
      , "money"     .= toJSON money
      ]


instance FromJSON Payment where
  parseJSON =
    withObject "Payment"
      $ \o ->
        Payment
          <$> (o .: "accountId")
          <*> (o .: "payee"    )
          <*> (o .: "money"    )


instance ToJSON AddressAny where
  toJSON = String . T.pack . show

instance FromJSON AddressAny where
  parseJSON =
    withText "AddressAny"
      $ \s ->
        case deserialiseAddress AsAddressAny s of
          Just address -> pure address
          Nothing      -> fail $ "Failed to parse address \"" <> T.unpack s <> "\"."
