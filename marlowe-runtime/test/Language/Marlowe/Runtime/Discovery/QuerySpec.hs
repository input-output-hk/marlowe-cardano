{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Discovery.QuerySpec
  where

import Data.Void (absurd)
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Gen (resized)
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import qualified Language.Marlowe.Runtime.Discovery.Gen ()
import Network.Protocol.Codec.Spec
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "DiscoveryQuery" do
  prop "Has a lawful Query protocol codec" $ checkPropCodec genByteStringSplits (codecQuery @Discovery.DiscoveryQuery)

instance QueryEq Discovery.DiscoveryQuery where
  queryEq = \case
    Discovery.GetContractHeaders -> \case
      Discovery.GetContractHeaders -> True
    Discovery.GetContractHeadersByRoleTokenCurrency policyId -> \case
      Discovery.GetContractHeadersByRoleTokenCurrency policyId' -> policyId == policyId'
  delimiterEq = \case
    Discovery.TagGetContractHeaders -> (==)
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> (==)
  errEq = \case
    Discovery.TagGetContractHeaders -> (==)
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> (==)
  resultEq = \case
    Discovery.TagGetContractHeaders -> (==)
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> (==)

instance ShowQuery Discovery.DiscoveryQuery where
  showsPrecTag _ = showString . \case
    Discovery.TagGetContractHeaders -> "TagGetContractHeaders"
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> "TagGetContractHeadersByRoleTokenCurrency"
  showsPrecQuery p = \case
    Discovery.GetContractHeaders -> showString "GetContractHeaders"
    Discovery.GetContractHeadersByRoleTokenCurrency policyId -> showParen (p >= 1)
      ( showString "TagGetContractHeadersByRoleTokenCurrency"
      . showSpace
      . showsPrec 11 policyId
      )
  showsPrecDelimiter p = \case
    Discovery.TagGetContractHeaders -> showsPrec p
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> showsPrec p
  showsPrecErr p = \case
    Discovery.TagGetContractHeaders -> showsPrec p
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> showsPrec p
  showsPrecResult p = \case
    Discovery.TagGetContractHeaders -> showsPrec p
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> showsPrec p

instance ArbitraryQuery Discovery.DiscoveryQuery where
  arbitraryTag = elements [SomeTag Discovery.TagGetContractHeaders, SomeTag Discovery.TagGetContractHeadersByRoleTokenCurrency]
  arbitraryQuery = \case
    Discovery.TagGetContractHeaders -> pure Discovery.GetContractHeaders
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> Discovery.GetContractHeadersByRoleTokenCurrency <$> arbitrary
  arbitraryDelimiter = \case
    Discovery.TagGetContractHeaders -> Just arbitrary
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> Nothing
  arbitraryErr = \case
    Discovery.TagGetContractHeaders -> Nothing
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> Nothing
  arbitraryResults = \case
    Discovery.TagGetContractHeaders -> resized (min 30) arbitrary
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> resized (min 30) arbitrary
  shrinkQuery = \case
    Discovery.GetContractHeaders -> []
    Discovery.GetContractHeadersByRoleTokenCurrency _ -> []
  shrinkErr = \case
    Discovery.TagGetContractHeaders -> absurd
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> absurd
  shrinkResults = \case
    Discovery.TagGetContractHeaders -> shrink
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> shrink
  shrinkDelimiter = \case
    Discovery.TagGetContractHeaders -> const []
    Discovery.TagGetContractHeadersByRoleTokenCurrency -> absurd
