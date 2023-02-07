{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.QuerySpec
  where

import Data.Foldable (fold)
import Language.Marlowe.Protocol.Query.Codec (codecMarloweQuery)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Gen (StructureType(..), oneofStructured, resized)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..), MarloweVersionTag(V1))
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
        pure $ AnyMessageAndAgency (ClientAgency TokReq) $ MsgRequest request
    , pure $ AnyMessageAndAgency (ClientAgency TokReq) MsgDone
    , do
        SomeStRes req <- arbitrary
        a <- arbitraryResult req
        pure $ AnyMessageAndAgency (ServerAgency (TokRes req)) $ MsgRespond a
    ]
    where
      arbitraryResult :: StRes a -> Gen a
      arbitraryResult = \case
        TokContractHeaders -> arbitrary
        TokContractState -> arbitrary
        TokBoth a b -> resized (`div` 2) $ (,) <$> arbitraryResult a <*> arbitraryResult b

  shrinkMessage = \case
    ClientAgency TokReq -> \case
      MsgRequest req -> MsgRequest <$> shrinkRequest req
      MsgDone -> []
    ServerAgency (TokRes req) -> \case
      MsgRespond a -> MsgRespond <$> shrinkResponse req a

data SomeStRes where
  SomeStRes :: StRes a -> SomeStRes

instance Arbitrary SomeContractState where
  arbitrary = SomeContractState MarloweV1 <$> arbitrary
  shrink (SomeContractState MarloweV1 state) = SomeContractState MarloweV1 <$> shrink state

instance Arbitrary (ContractState 'V1) where
  arbitrary = ContractState
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
  shrink = genericShrink

instance Arbitrary SomeStRes where
  arbitrary = oneofStructured
    [ ( Node
      , resize 0 do
          SomeStRes a <- arbitrary
          SomeStRes b <- arbitrary
          pure $ SomeStRes $ TokBoth a b
      )
    , (Leaf, pure $ SomeStRes TokContractHeaders)
    , (Leaf, pure $ SomeStRes TokContractState)
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
    , (Leaf, SomeRequest . ReqContractState <$> arbitrary)
    ]
  shrink (SomeRequest req) = case req of
    ReqContractHeaders range -> SomeRequest . ReqContractHeaders <$> shrink range
    ReqContractState contractId -> SomeRequest . ReqContractState <$> shrink contractId
    ReqBoth a b -> fold
      [ [ SomeRequest $ ReqBoth a' b | SomeRequest a' <- shrink (SomeRequest a) ]
      , [ SomeRequest $ ReqBoth a b' | SomeRequest b' <- shrink (SomeRequest b) ]
      ]

shrinkRequest :: Request a -> [Request a]
shrinkRequest = \case
  ReqContractHeaders range -> ReqContractHeaders <$> shrink range
  ReqContractState contractId -> ReqContractState <$> shrink contractId
  ReqBoth a b -> fold
    [ [ ReqBoth a' b | a' <- shrinkRequest a ]
    , [ ReqBoth a b' | b' <- shrinkRequest b ]
    ]

shrinkResponse :: StRes a -> a -> [a]
shrinkResponse = \case
  TokContractHeaders -> shrink
  TokContractState -> shrink
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
