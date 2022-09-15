{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Core.AddressRegistry
  where

import Cardano.Api.Byron (NetworkId(..))
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (IsString(fromString))
import Language.Marlowe.Runtime.ChainSync.Api (Address, ScriptHash)
import Language.Marlowe.Runtime.Core.Api

-- | The hash and address for a script.
data ScriptAddressInfo = ScriptAddressInfo
  { scriptAddress :: Address
  , scriptHash :: ScriptHash
  } deriving (Show, Eq, Ord)

-- | The script addresses for a marlowe version.
data MarloweScriptAddresses = MarloweScriptAddresses
  { marloweScriptAddress :: ScriptAddressInfo
  , payoutScriptAddress :: ScriptAddressInfo
  } deriving (Show, Eq, Ord)

unsafeMarloweAddressesFromScriptHashStrings :: String -> String -> NetworkId -> MarloweScriptAddresses
unsafeMarloweAddressesFromScriptHashStrings marloweScriptHash payoutScriptHash networkId = MarloweScriptAddresses
  { marloweScriptAddress = unsafeScriptAddressInfoFromScriptHashString marloweScriptHash networkId
  , payoutScriptAddress = unsafeScriptAddressInfoFromScriptHashString payoutScriptHash networkId
  }

unsafeScriptAddressInfoFromScriptHashString :: String -> NetworkId -> ScriptAddressInfo
unsafeScriptAddressInfoFromScriptHashString scriptHash networkId = ScriptAddressInfo
  { scriptAddress = case networkId of
      Mainnet -> fromString $ "71" <> scriptHash
      Testnet _ -> fromString $ "70" <> scriptHash
  , scriptHash = fromString scriptHash
  }

-- | The current static script addresses for Marlowe V1 as of the current git
-- commit. Enforced in the test suite for the Marlowe Runtime.
--
-- STOP Are you here to change this value to fix a test failure? Before you do
-- so, please copy the current value into 'v1ScriptAddressSet' before updating
-- it to the new one (unless you are certain that the address here has never
-- been published).
currentMarloweV1Addresses :: NetworkId -> MarloweScriptAddresses
currentMarloweV1Addresses = unsafeMarloweAddressesFromScriptHashStrings
  "6c31af457894bb0789299c9c75e784f9e822d0b23c50daea694127fc"
  "6db99855e93e8fda9e917692bc746c9f6db73e9e9234a3abeeea971c"

-- | The set of script addresses for Marlowe V1
v1ScriptAddressSet :: NetworkId -> Set MarloweScriptAddresses
v1ScriptAddressSet = Set.fromList <$> sequence [currentMarloweV1Addresses]

-- | Key a set of Marlowe script address by its Marlowe script hash.
toScriptAddressMap :: Set MarloweScriptAddresses -> Map ScriptHash MarloweScriptAddresses
toScriptAddressMap  = Map.mapKeys (scriptHash . marloweScriptAddress) . Map.fromSet id

-- | The map of script addresses for Marlowe V1 keyed by their Marlowe script
-- hash.
v1ScriptAddressMap :: NetworkId -> Map ScriptHash MarloweScriptAddresses
v1ScriptAddressMap = toScriptAddressMap . v1ScriptAddressSet

-- | Lookup the Marlowe version and script addresses associated with the given
-- Marlowe script hash.
getMarloweVersion :: NetworkId -> ScriptHash -> Maybe (SomeMarloweVersion, MarloweScriptAddresses)
getMarloweVersion networkId hash = asum
  [ (SomeMarloweVersion MarloweV1,) <$> Map.lookup hash (v1ScriptAddressMap networkId)
  ]

-- | Get the script address set associated with the given Marlowe version.
-- Membership of the current script addresses is enforced in the test suite.
getScriptAddressSet :: NetworkId -> MarloweVersion v -> Set MarloweScriptAddresses
getScriptAddressSet networkId = \case
  MarloweV1 -> v1ScriptAddressSet networkId

-- | Get the current script addresses for the given Marlowe version as of the
-- current git commit. Enforced in the test suite.
getCurrentScriptAddresses :: NetworkId -> MarloweVersion v -> MarloweScriptAddresses
getCurrentScriptAddresses networkId = \case
  MarloweV1 -> currentMarloweV1Addresses networkId
