-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

-- | Coding and decoding.
module Language.Marlowe.CLI.Codec (
  -- * Codecs
  decodeBech32,
  encodeBech32,
) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.Marlowe.CLI.IO (liftCli, liftCliMaybe)
import Language.Marlowe.CLI.Types (CliError)
import System.IO (hPutStrLn, stderr)

import Codec.Binary.Bech32 qualified as Bech32 (
  dataPartFromBytes,
  dataPartToBytes,
  decodeLenient,
  encodeLenient,
  humanReadablePartFromText,
  humanReadablePartToText,
 )
import Data.ByteString.Base16 qualified as Base16 (decode, encode)
import Data.ByteString.Char8 qualified as BS (pack, unpack)
import Data.Text qualified as T (pack, unpack)

-- | Decode Bech32 data.
decodeBech32
  :: (MonadError CliError m)
  => (MonadIO m)
  => String
  -- ^ The Bech32 data.
  -> m ()
  -- ^ Action to print the decoded data.
decodeBech32 text =
  do
    (humanReadablePart, dataPart) <-
      liftCli
        . Bech32.decodeLenient
        $ T.pack text
    let humanReadablePart' =
          T.unpack $
            Bech32.humanReadablePartToText humanReadablePart
    dataPart' <-
      liftCliMaybe "Failed decoding data part." $
        BS.unpack . Base16.encode
          <$> Bech32.dataPartToBytes dataPart
    liftIO . hPutStrLn stderr $ "Human-readable part: " <> humanReadablePart'
    liftIO $ putStrLn dataPart'

-- | Encode Bech32 data.
encodeBech32
  :: (MonadError CliError m)
  => (MonadIO m)
  => String
  -- ^ The human-readable prefix.
  -> String
  -- ^ The base 16 data to be encoded.
  -> m ()
  -- ^ Acction to print the encoded data.
encodeBech32 humanReadablePart dataPart =
  do
    humanReadablePart' <-
      liftCli
        . Bech32.humanReadablePartFromText
        $ T.pack humanReadablePart
    datapart' <-
      liftCli
        . fmap Bech32.dataPartFromBytes
        . Base16.decode
        $ BS.pack dataPart
    let encoded =
          T.unpack $
            Bech32.encodeLenient humanReadablePart' datapart'
    liftIO $ putStrLn encoded
