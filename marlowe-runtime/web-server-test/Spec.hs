{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  where

import Control.Monad (replicateM)
import Data.Aeson (Value(Null))
import qualified Data.ByteString as BS
import Data.OpenApi hiding (version)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Language.Marlowe.Runtime.Web (HasNamedLink)
import qualified Language.Marlowe.Runtime.Web as Web
import Servant.OpenApi
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe, hspec)
import Test.QuickCheck (Arbitrary(..), elements, listOf, oneof, resize)
import Text.Regex.Posix ((=~))

main :: IO ()
main = hspec do
  describe "OpenAPI" openAPISpec

openAPISpec :: Spec
openAPISpec = validateEveryToJSONWithPatternChecker patternChecker (Proxy @Web.API)

patternChecker :: Pattern -> Text -> Bool
patternChecker pat text = T.unpack text =~ T.unpack pat

instance Arbitrary Web.ContractHeader where
  arbitrary = Web.ContractHeader
    <$> arbitrary
    <*> arbitrary
    <*> pure Web.V1
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Web.ContractState where
  -- size of 6 will result in a 1-layer deep contract being generated (this is
  -- all we care about for the purposes of schema checking).
  arbitrary = resize 6 $ Web.ContractState
    <$> arbitrary
    <*> arbitrary
    <*> pure Web.V1
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink cs = [ cs { Web.initialContract = x } | x <- shrink $ Web.initialContract cs ]
           <> [ cs { Web.currentContract = x } | x <- shrink $ Web.currentContract cs ]
           <> [ cs { Web.state = x } | x <- shrink $ Web.state cs ]


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

instance (HasNamedLink a Web.API name, Arbitrary a) => Arbitrary (Web.WithLink name a) where
  arbitrary = oneof [Web.Omit <$> arbitrary, Web.Include Web.api (Proxy @name) <$> arbitrary]
