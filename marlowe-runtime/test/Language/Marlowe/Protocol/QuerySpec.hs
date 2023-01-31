{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.QuerySpec
  where

import Data.Foldable (fold)
import Language.Marlowe.Protocol.Query.Codec (codecMarloweQuery)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Gen (StructureType(..), oneofStructured, resized)
import Language.Marlowe.Runtime.Discovery.Gen ()
import Network.Protocol.Codec.Spec
import Network.TypedProtocol.Codec
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "MarloweQuery protocol" do
  prop "Has a lawful codec" $ checkPropCodec genByteStringSplits codecMarloweQuery

instance ArbitraryMessage MarloweQuery where
  arbitraryMessage = resized (min 30) $ oneof
    [ do
        SomeRequest request <- arbitrary
        pure $ AnyMessageAndAgency (ClientAgency TokInit) $ MsgRequest request
    , do
        SomeStRequest req <- arbitrary
        a <- arbitraryResult req
        pure $ AnyMessageAndAgency (ServerAgency (TokRequest req)) $ MsgRespond a
    ]
    where
      arbitraryResult :: StRequest a -> Gen a
      arbitraryResult = \case
        TokContractHeaders -> arbitrary
        TokBoth a b -> resized (`div` 2) $ (,) <$> arbitraryResult a <*> arbitraryResult b

  shrinkMessage = \case
    ClientAgency TokInit -> \case
      MsgRequest req -> MsgRequest <$> shrinkRequest req
    ServerAgency (TokRequest req) -> \case
      MsgRespond a -> MsgRespond <$> shrinkResponse req a

data SomeStRequest where
  SomeStRequest :: StRequest a -> SomeStRequest

instance Arbitrary SomeStRequest where
  arbitrary = oneofStructured
    [ ( Node
      , resize 0 do
          SomeStRequest a <- arbitrary
          SomeStRequest b <- arbitrary
          pure $ SomeStRequest $ TokBoth a b
      )
    , (Leaf, pure $ SomeStRequest TokContractHeaders)
    ]

instance Arbitrary SomeRequest where
  arbitrary = oneofStructured
    [ ( Node
      , resize 0 do
          SomeRequest a <- arbitrary
          SomeRequest b <- arbitrary
          pure $ SomeRequest $ ReqBoth a b
      )
    , (Leaf, SomeRequest . ReqContractHeaders <$> arbitrary)
    ]
  shrink (SomeRequest req) = case req of
    ReqContractHeaders range -> SomeRequest . ReqContractHeaders <$> shrink range
    ReqBoth a b -> fold
      [ [ SomeRequest $ ReqBoth a' b | SomeRequest a' <- shrink (SomeRequest a) ]
      , [ SomeRequest $ ReqBoth a b' | SomeRequest b' <- shrink (SomeRequest b) ]
      ]

shrinkRequest :: Request a -> [Request a]
shrinkRequest = \case
  ReqContractHeaders range -> ReqContractHeaders <$> shrink range
  ReqBoth a b -> fold
    [ [ ReqBoth a' b | a' <- shrinkRequest a ]
    , [ ReqBoth a b' | b' <- shrinkRequest b ]
    ]

shrinkResponse :: StRequest a -> a -> [a]
shrinkResponse = \case
  TokContractHeaders -> shrink
  TokBoth ta tb -> \(a, b) -> fold
    [ [ (a', b) | a' <- shrinkResponse ta a ]
    , [ (a, b') | b' <- shrinkResponse tb b ]
    ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Page a b) where
  arbitrary = Page <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (Range a) where
  arbitrary = Range <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Order where
  arbitrary = elements [Ascending, Descending]
