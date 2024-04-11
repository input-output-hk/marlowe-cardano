{-# LANGUAGE GADTs #-}

module Language.Marlowe.Object.TypesSpec (spec, checkLaws) where

import Data.Aeson (Result (..), ToJSON (..), fromJSON)
import Data.Binary (Binary, decode, encode)
import Data.Foldable (traverse_)
import Data.Proxy (Proxy (..))
import Language.Marlowe.Object.Gen ()
import Language.Marlowe.Object.Types
import Spec.Marlowe.Semantics.Arbitrary (ValidContractStructure (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (Success, label)
import Test.QuickCheck.Classes (Laws (..), eqLaws, jsonLaws, ordLaws, showLaws, showReadLaws)

spec :: Spec
spec = do
  describe "ObjectBundle" do
    -- Testing hand-written Binary instance only. Other instances are derived
    -- and expensive to run.
    checkLaws $ binaryLaws $ Proxy @ObjectBundle

  describe "LabelledObject" do
    checkLaws $ eqLaws $ Proxy @LabelledObject
    checkLaws $ ordLaws $ Proxy @LabelledObject
    checkLaws $ jsonLaws $ Proxy @LabelledObject
    checkLaws $ showLaws $ Proxy @LabelledObject
    checkLaws $ showReadLaws $ Proxy @LabelledObject

  describe "Contract" do
    prop "Can decode JSON generated from core contracts" \(ValidContractStructure contract) ->
      fromJSON (toJSON contract) === Success (fromCoreContract contract)

binaryLaws :: forall a. (Binary a, Show a, Arbitrary a, Eq a) => Proxy a -> Laws
binaryLaws _ =
  Laws
    "Binary"
    [
      ( "inverse"
      , property \(a :: a) -> decode (encode a) === a
      )
    ]

checkLaws :: Laws -> Spec
checkLaws Laws{..} = describe lawsTypeclass do
  traverse_ (uncurry prop) lawsProperties
