module Language.Marlowe.Runtime.Core.MetadataSpec
  where

import Language.Marlowe.Runtime.Core.Api (decodeMarloweTransactionMetadata, encodeMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Core.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (counterexample, (===))

spec :: Spec
spec = do
  prop "prop: roundtrip encoding" \metadata ->
    let
      encoded = encodeMarloweTransactionMetadata metadata
    in
      counterexample ("Encoded: " <> show encoded)
        $ decodeMarloweTransactionMetadata encoded === Right metadata
