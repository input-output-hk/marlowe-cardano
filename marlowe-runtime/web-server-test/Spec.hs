{-# OPTIONS_GHC -Wno-orphans #-}

module Main
  where

import Control.Monad (replicateM)
import Data.Aeson (Value(Null))
import qualified Data.ByteString as BS
import Data.OpenApi
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Language.Marlowe.Runtime.Web as Web
import Servant.OpenApi
import Test.Hspec (Spec, describe, hspec)
import Test.QuickCheck (Arbitrary(..), listOf, oneof)
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

instance Arbitrary Web.TxOutRef where
  arbitrary = Web.TxOutRef <$> arbitrary <*> arbitrary

instance Arbitrary Web.TxId where
  arbitrary = Web.TxId . BS.pack <$> replicateM 32 arbitrary

instance Arbitrary Web.PolicyId where
  arbitrary = Web.PolicyId . BS.pack <$> listOf arbitrary

instance Arbitrary Web.Metadata where
  arbitrary = pure $ Web.Metadata Null

instance Arbitrary Web.TxStatusHeader where
  arbitrary = oneof
    [ pure Web.Unsigned
    , pure Web.Submitted
    , Web.Confirmed <$> arbitrary
    ]

instance Arbitrary Web.BlockHeader where
  arbitrary = Web.BlockHeader <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Web.Base16 where
  arbitrary = Web.Base16 . BS.pack <$> listOf arbitrary
