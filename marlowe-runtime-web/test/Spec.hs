{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  where

import Control.Monad (replicateM)
import Data.Aeson (Value(Null))
import qualified Data.ByteString as BS
import Data.OpenApi hiding (version)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics (Input(..))
import qualified Language.Marlowe.Runtime.Web as Web
import Network.Arbitrary ()
import Servant.OpenApi
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe, hspec)
import Test.QuickCheck (Arbitrary(..), Gen, elements, genericShrink, listOf, oneof, resize, suchThat)
import Test.QuickCheck.Instances ()
import Text.Regex.Posix ((=~))

main :: IO ()
main = hspec do
  describe "OpenAPI" openAPISpec

openAPISpec :: Spec
openAPISpec = validateEveryToJSONWithPatternChecker patternChecker (Proxy @Web.API)

patternChecker :: Pattern -> Text -> Bool
patternChecker pat text = T.unpack text =~ T.unpack pat

instance Arbitrary Web.WithdrawalHeader where
  arbitrary = Web.WithdrawalHeader
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Web.ContractHeader where
  arbitrary = Web.ContractHeader
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Web.TxHeader where
  arbitrary = Web.TxHeader
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Web.ContractState where
  arbitrary = Web.ContractState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    -- size of 6 will result in a 1-layer deep contract being generated (this is
    -- all we care about for the purposes of schema checking).
    <*> resize 6 arbitrary
    <*> resize 6 arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.PayoutRef where
  arbitrary = Web.PayoutRef
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Withdrawal where
  arbitrary = Web.Withdrawal
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Tx where
  arbitrary = Web.Tx
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitraryNormal  -- FIXME: This should handle merkleized input, too.
    <*> arbitrary
    -- size of 6 will result in a 1-layer deep contract being generated (this is
    -- all we care about for the purposes of schema checking).
    <*> resize 6 arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.PostWithdrawalsRequest where
  arbitrary = Web.PostWithdrawalsRequest <$> arbitrary <*> arbitrary

instance Arbitrary Web.PostContractsRequest where
  arbitrary = Web.PostContractsRequest
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    -- size of 6 will result in a 1-layer deep contract being generated (this is
    -- all we care about for the purposes of schema checking).
    <*> resize 6 arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.PostTransactionsRequest where
  arbitrary = Web.PostTransactionsRequest
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitraryNormal  -- FIXME: This should handle merkleized input, too.
  shrink = genericShrink

instance Arbitrary (Web.CreateTxEnvelope tx) where
  arbitrary = Web.CreateTxEnvelope <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Web.WithdrawTxEnvelope tx) where
  arbitrary = Web.WithdrawTxEnvelope <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary (Web.ApplyInputsTxEnvelope tx) where
  arbitrary = Web.ApplyInputsTxEnvelope <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.MarloweVersion where
  arbitrary = pure Web.V1

instance Arbitrary Web.RolesConfig where
  arbitrary = oneof
    [ Web.UsePolicy <$> arbitrary
    , Web.Mint <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary Web.RoleTokenConfig where
  arbitrary = oneof
    [ Web.RoleTokenSimple <$> arbitrary
    , Web.RoleTokenAdvanced <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary Web.TokenMetadata where
  arbitrary = Web.TokenMetadata
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.TokenMetadataFile where
  arbitrary = Web.TokenMetadataFile <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.Address where
  arbitrary = Web.Address <$> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (Web.ListObject a) where
  arbitrary = Web.ListObject <$> arbitrary
  shrink = genericShrink

instance Arbitrary Web.TextEnvelope where
  arbitrary = Web.TextEnvelope <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Web.TxOutRef where
  arbitrary = Web.TxOutRef <$> arbitrary <*> arbitrary

instance Arbitrary Web.TxId where
  arbitrary = Web.TxId . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Web.PolicyId where
  arbitrary = Web.PolicyId . BS.pack <$> listOf arbitrary

instance Arbitrary Web.Metadata where
  arbitrary = pure $ Web.Metadata Null

instance Arbitrary Web.TxStatus where
  arbitrary = elements
    [ Web.Unsigned
    , Web.Submitted
    , Web.Confirmed
    ]

instance Arbitrary Web.BlockHeader where
  arbitrary = Web.BlockHeader <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Web.Base16 where
  arbitrary = Web.Base16 . BS.pack <$> listOf arbitrary

instance Arbitrary a => Arbitrary (Web.WithLink name a) where
  arbitrary = oneof
    [ Web.OmitLink <$> arbitrary
    , Web.IncludeLink (Proxy @name) <$> arbitrary
    ]
  shrink (Web.OmitLink a) = Web.OmitLink <$> shrink a
  shrink (Web.IncludeLink n a) = [Web.OmitLink a] <> (Web.IncludeLink n <$> shrink a)

arbitraryNormal :: Gen [Semantics.Input]
arbitraryNormal =
  arbitrary `suchThat` all isNormal
    where isNormal (Semantics.NormalInput _) = True
          isNormal _ = False
