{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Core.ScriptRegistry
  where

import Cardano.Api (NetworkId(..), NetworkMagic(NetworkMagic))
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable (asum)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (ScriptHash, TxOutRef)
import Language.Marlowe.Runtime.Core.Api

newtype NetworkIdWithOrd = NetworkIdWithOrd NetworkId
  deriving (Eq, Show)

instance Ord NetworkIdWithOrd where
  compare (NetworkIdWithOrd Mainnet) (NetworkIdWithOrd Mainnet) = EQ
  compare (NetworkIdWithOrd Mainnet) (NetworkIdWithOrd _) = LT
  compare (NetworkIdWithOrd _) (NetworkIdWithOrd Mainnet) = GT
  compare (NetworkIdWithOrd (Testnet (NetworkMagic a))) (NetworkIdWithOrd (Testnet (NetworkMagic b))) =
    compare a b

-- | A set of script hashes for a marlowe version.
data MarloweScripts = MarloweScripts
  { marloweScript :: ScriptHash
  , payoutScript :: ScriptHash
  , marloweScriptUTxOs :: Map NetworkIdWithOrd TxOutRef
  , payoutScriptUTxOs :: Map NetworkIdWithOrd TxOutRef
  } deriving (Show, Eq, Ord)

previewNetworkId = NetworkIdWithOrd $ Testnet $ NetworkMagic 2

-- | The current pair of static script hashes for Marlowe V1 as of the current git
-- commit. Enforced in the test suite for the Marlowe Runtime.
--
-- STOP Are you here to change this value to fix a test failure? Before you do
-- so, please copy the current value into 'v1Scripts' before updating
-- it to the new one (unless you are certain that the address here has never
-- been published).
currentV1Scripts :: MarloweScripts
currentV1Scripts = MarloweScripts
  "6a9391d6aa51af28dd876ebb5565b69d1e83e5ac7861506bd29b56b0"
  "49076eab20243dc9462511fb98a9cfb719f86e9692288139b7c91df3"
  ( Map.fromList
      [ (previewNetworkId, "087f21f109a997193421a81886ea8c6397d336d19e696457b9c5c7aefdc31873#1")
      ]
  )
  ( Map.fromList
      [ (previewNetworkId, "087f21f109a997193421a81886ea8c6397d336d19e696457b9c5c7aefdc31873#2")
      ]
  )

-- | The set of known script hash sets for Marlowe V1.
v1Scripts :: Set MarloweScripts
v1Scripts = Set.fromList [currentV1Scripts]

-- | Key a set of script hash sets by their Marlowe script hashes.
toScriptMap :: Set MarloweScripts -> Map ScriptHash MarloweScripts
toScriptMap  = Map.mapKeys marloweScript . Map.fromSet id

-- | The map of script hash sets for Marlowe V1 keyed by their Marlowe script
-- hash.
v1ScriptMap :: Map ScriptHash MarloweScripts
v1ScriptMap = toScriptMap v1Scripts

-- | Lookup the Marlowe version and script hash set associated with the given
-- Marlowe script hash.
getMarloweVersion :: ScriptHash -> Maybe (SomeMarloweVersion, MarloweScripts)
getMarloweVersion hash = asum
  [ (SomeMarloweVersion MarloweV1,) <$> Map.lookup hash v1ScriptMap
  ]

-- | Get the set of known script hash sets associated with the given Marlowe
-- version.
--
-- NOTE Membership of the current script addresses is enforced in the test
-- suite.
getScripts :: MarloweVersion v -> Set MarloweScripts
getScripts = \case
  MarloweV1 -> v1Scripts

-- | Get the current script hash set for the given Marlowe version as of the
-- current git commit.
--
-- NOTE: Enforced in the test suite.
getCurrentScripts :: MarloweVersion v -> MarloweScripts
getCurrentScripts = \case
  MarloweV1 -> currentV1Scripts
