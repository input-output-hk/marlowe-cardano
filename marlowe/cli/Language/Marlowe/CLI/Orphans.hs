-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Orphan instances for Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Language.Marlowe.CLI.Orphans (
) where


import           Data.Aeson             (FromJSON (..), ToJSON (..), withText)
import           Data.ByteString.Short  (ShortByteString, fromShort, toShort)

import qualified Data.ByteString.Base16 as Base16 (decode, encode)
import qualified Data.ByteString.Char8  as BS8 (pack, unpack)
import qualified Data.Text              as T (unpack)


instance ToJSON ShortByteString where
  toJSON = toJSON . BS8.unpack . Base16.encode . fromShort

instance FromJSON ShortByteString where
  parseJSON =
    withText "ShortByteString"
      $ \t ->
        case Base16.decode . BS8.pack $ T.unpack t of
          Right bytes   -> pure $ toShort bytes
          Left  message -> fail message
