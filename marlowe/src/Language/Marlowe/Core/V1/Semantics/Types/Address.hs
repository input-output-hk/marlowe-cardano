

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}


module Language.Marlowe.Core.V1.Semantics.Types.Address
  ( -- * Types
    Network
  , mainnet
  , testnet
    -- * Serialisation
  , deserialiseAddress
  , deserialiseAddressBech32
  , serialiseAddress
  , serialiseAddressBech32
  ) where


import Codec.Binary.Bech32
  (HumanReadablePart, dataPartFromBytes, dataPartToBytes, decodeLenient, encodeLenient, humanReadablePartFromText)
import Control.Monad (guard)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.List (find)
import Data.Text (Text)
import Plutus.V2.Ledger.Api
  ( Address(Address)
  , BuiltinByteString
  , Credential(PubKeyCredential, ScriptCredential)
  , PubKeyHash(PubKeyHash)
  , StakingCredential(StakingHash, StakingPtr)
  , ValidatorHash(ValidatorHash)
  , fromBuiltin
  , toBuiltin
  )
import PlutusTx.Builtins (consByteString, lengthOfByteString, sliceByteString)

import qualified Data.ByteString as BS (uncons)


-- | Type of network.
type Network = Bool


-- | The main network.
mainnet :: Network
mainnet = toEnum 0x01


-- | A test network.
testnet :: Network
testnet = toEnum 0x00


-- | The human-readable Bech32 prefix for a network.
humanReadableFromNetwork :: Network -> HumanReadablePart
humanReadableFromNetwork network
  | network == mainnet = either (error . show) id $ humanReadablePartFromText "addr"       -- The error condition never occurs for this prefix.
  | otherwise          = either (error . show) id $ humanReadablePartFromText "addr_test"  -- The error condition never occurs for this prefix.


-- | The network corresponding to a human-readable Bech32 prefix.
humanReadableToNetwork :: HumanReadablePart -> Maybe Network
humanReadableToNetwork human = find ((== human) . humanReadableFromNetwork) [mainnet, testnet]


-- | Serialize a Plutus network address to CIP-19 bytes.
serialiseAddress :: Network -> Address -> ByteString
serialiseAddress network address =
  -- See CIP-19 binary format, <https://cips.cardano.org/cips/cip19/#binaryformat>.
  let
    header x = consByteString (x + toInteger (fromEnum network)) mempty
    encodeInteger :: Integer -> BuiltinByteString
    encodeInteger = error "Staking pointers not supported."  -- TODO: Add support for staking pointers, even though these may never have been used on the ledger.
  in
    fromBuiltin
      $ case address of
          Address (PubKeyCredential (PubKeyHash    pkh)) (Just (StakingHash (PubKeyCredential (PubKeyHash    spkh)))) -> header 0x00 <> pkh <> spkh
          Address (ScriptCredential (ValidatorHash vah)) (Just (StakingHash (PubKeyCredential (PubKeyHash    spkh)))) -> header 0x10 <> vah <> spkh
          Address (PubKeyCredential (PubKeyHash    pkh)) (Just (StakingHash (ScriptCredential (ValidatorHash svah)))) -> header 0x20 <> pkh <> svah
          Address (ScriptCredential (ValidatorHash vah)) (Just (StakingHash (ScriptCredential (ValidatorHash svah)))) -> header 0x30 <> vah <> svah
          Address (PubKeyCredential (PubKeyHash    pkh)) (Just (StakingPtr slot transaction certificate            )) -> header 0x40 <> pkh <> encodeInteger slot <> encodeInteger transaction <> encodeInteger certificate
          Address (ScriptCredential (ValidatorHash vah)) (Just (StakingPtr slot transaction certificate            )) -> header 0x50 <> vah <> encodeInteger slot <> encodeInteger transaction <> encodeInteger certificate
          Address (PubKeyCredential (PubKeyHash    pkh)) Nothing                                                      -> header 0x60 <> pkh
          Address (ScriptCredential (ValidatorHash vah)) Nothing                                                      -> header 0x70 <> vah



-- | Deserialize a Plutus network address from CIP-19 bytes.
deserialiseAddress :: ByteString -> Maybe (Network, Address)
deserialiseAddress bs =
  do
    (header, payload) <- BS.uncons bs
    let
      payload' = toBuiltin payload
      n = lengthOfByteString payload'
      k = 28
      slice i = sliceByteString (k * i) k payload'
      guard1 f   = guard (n == k    ) >> pure (f payload' , Nothing                         )
      guard2 f g = guard (n == 2 * k) >> pure (f $ slice 0, Just . StakingHash . g $ slice 1)
      pkc = PubKeyCredential . PubKeyHash
      scc = ScriptCredential . ValidatorHash
    network <- find ((fromEnum (header .&. 0x0F) ==) . fromEnum) [mainnet, testnet]
    (network, )
      . uncurry Address
      <$> case header .&. 0xF0 of
            0x00 -> guard2 pkc pkc
            0x10 -> guard2 scc pkc
            0x20 -> guard2 pkc scc
            0x30 -> guard2 scc scc
            0x40 -> fail "Staking pointers not supported."  -- TODO: Add support for staking pointers, even though these may never have been used on the ledger.
            0x50 -> fail "Staking pointers not supported."  -- TODO: Add support for staking pointers, even though these may never have been used on the ledger.
            0x60 -> guard1 pkc
            0x70 -> guard1 scc
            _      -> fail "Invalid address header."


-- | Serialize a Plutus address to CIP-19 Bech32.
serialiseAddressBech32 :: Network -> Address -> Text
serialiseAddressBech32 network address =
  let
    human = humanReadableFromNetwork network
    bs = serialiseAddress network address
  in
    encodeLenient human $ dataPartFromBytes bs


-- | Deserialize a Plutus address from CIP-19 Bech32.
deserialiseAddressBech32 :: Text -> Maybe (Network, Address)
deserialiseAddressBech32 encoded =
  do
    (human, dataPart) <- either (fail . show) Just $ decodeLenient encoded
    network <- humanReadableToNetwork human
    bs <- dataPartToBytes dataPart
    (network', address) <- deserialiseAddress bs
    guard $ network == network'
    pure (network, address)
