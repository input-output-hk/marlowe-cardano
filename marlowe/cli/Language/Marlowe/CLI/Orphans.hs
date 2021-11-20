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


import           Data.Aeson             (ToJSON (..))
import           Data.ByteString.Short  (ShortByteString, fromShort)

import qualified Data.ByteString.Base16 as Base16 (encode)
import qualified Data.ByteString.Char8  as BS8 (unpack)


instance ToJSON ShortByteString where
  toJSON = toJSON . BS8.unpack . Base16.encode . fromShort
