
-- | Functions for passing serialised data from Plutus scripts and then decoding them.


{-# LANGUAGE TupleSections #-}


module Plutus.Debug
  ( -- * Debugging
    debug
  , debugError
  , debugIfFalse
  , debugIfTrue
    -- * Decoding
  , recoverFromData
  , recoverPlutusData
  ) where


import Codec.Serialise (deserialise)
import Plutus.V2.Ledger.Api (ToData(..), unsafeFromBuiltinData)
import PlutusTx (Data, FromData(..), fromData)
import PlutusTx.Prelude (BuiltinString, decodeUtf8, encodeUtf8, error, fromBuiltin, trace)
import Prelude hiding (error)

import qualified Data.ByteString.Base16 as Base16 (decode)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Data.ByteString.Lazy as LBS (fromStrict)
import qualified Data.Text as T (unpack)


{-# INLINE debug #-}

-- | Write a message and serialised data to the trace log.
debug :: ToData a
      => BuiltinString  -- ^ A message.
      -> a              -- ^ The data.
      -> b              -- ^ The value to be returned.
      -> b              -- ^ The vaule returned.
debug message =
  trace
    . decodeUtf8
    . unsafeFromBuiltinData
    . toBuiltinData
    . (encodeUtf8 message, )


{-# INLINE debugIfTrue #-}

-- | Write a message and serialised data to the trace log, if a condition holds.
debugIfTrue :: ToData a
            => BuiltinString  -- ^ The message.
            -> a              -- ^ The data.
            -> Bool           -- ^ The condition to be returned.
            -> Bool           -- ^ The condition.
debugIfTrue message x condition =
  condition && debug message x True


{-# INLINE debugIfFalse #-}

-- | Write a message and serialised data to the trace log, if a condition does not hold.
debugIfFalse :: ToData a
             => BuiltinString  -- ^ The message.
             -> a              -- ^ The data.
             -> Bool           -- ^ The condition to be returned.
             -> Bool           -- ^ The condition.
debugIfFalse message x condition =
  condition || debug message x False


{-# INLINE debugError #-}

-- | Raise an error after writing a message and serialised data to the trace log.
debugError :: ToData a
           => BuiltinString  -- ^ The message.
           -> a              -- ^ The data.
           -> b              -- ^ The type to be returned.
debugError message x = error $ debug message x ()


-- | Recover serialised debugging data.
recoverPlutusData :: String              -- ^ The base-16 encoding of the data.
                  -> Either String Data  -- ^ The Plutus data.
recoverPlutusData raw =
  do
    bytes <- Base16.decode . BS8.pack $ raw
    pure
      . deserialise
      . LBS.fromStrict
      $ bytes


-- | Recover serialised debugging data.
recoverFromData :: FromData a
                => String                     -- ^ The base-16 encoding of the data.
                -> Either String (String, a)  -- ^ The original message and data.
recoverFromData raw =
  do
    raw' <- recoverPlutusData raw
    (message, x) <-
      maybe (Left "Failed decoding Plutus data.") Right
        $ fromData raw'
    pure
      (
        T.unpack . fromBuiltin . decodeUtf8 $ message
      , x
      )
