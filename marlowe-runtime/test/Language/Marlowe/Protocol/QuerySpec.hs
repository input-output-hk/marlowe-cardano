{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.QuerySpec
  where

import Data.Foldable (fold)
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
  prop "Has a lawful codec" $ checkPropCodec @MarloweQuery
  codecGoldenTests @MarloweQuery "MarloweQuery"

instance ArbitraryMessage MarloweQuery where
  arbitraryMessage = resized (min 30) $ oneof
    [ do
        SomeStRes tok <- arbitrary
        AnyMessageAndAgency (ClientAgency TokReq) . MsgRequest <$> arbitraryRequest tok
    , pure $ AnyMessageAndAgency (ClientAgency TokReq) MsgDone
    , do
        SomeStRes req <- arbitrary
        AnyMessageAndAgency (ServerAgency (TokRes req)) . MsgRespond <$> arbitraryResponse req
    ]
  shrinkMessage = \case
    ClientAgency TokReq -> \case
      MsgRequest req -> MsgRequest <$> shrinkRequest req
      MsgDone -> []
    ServerAgency (TokRes req) -> \case
      MsgRespond a -> MsgRespond <$> shrinkResponse req a

instance Arbitrary SomeContractState where
  arbitrary = SomeContractState MarloweV1 <$> arbitrary
  shrink (SomeContractState MarloweV1 state) = SomeContractState MarloweV1 <$> shrink state

instance Arbitrary SomeTransaction where
  arbitrary = SomeTransaction MarloweV1 <$> arbitrary <*> arbitrary <*> arbitrary
  shrink (SomeTransaction MarloweV1 input consumedBy state) = fold
    [ SomeTransaction MarloweV1 input consumedBy <$> shrink state
    , SomeTransaction MarloweV1 input <$> shrink consumedBy <*> pure state
    ]

instance Arbitrary SomeTransactions where
  arbitrary = SomeTransactions MarloweV1 <$> arbitrary
  shrink (SomeTransactions MarloweV1 txs) = SomeTransactions MarloweV1 <$> shrink txs

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
    , (Leaf, pure $ SomeStRes TokTransaction)
    , (Leaf, pure $ SomeStRes TokTransactions)
    ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Page a b) where
  arbitrary = Page <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary a => Arbitrary (Range a) where
  arbitrary = Range <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary PayoutRef where
  arbitrary = PayoutRef <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Withdrawal where
  arbitrary = Withdrawal <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary Order where
  arbitrary = elements [Ascending, Descending]

instance Arbitrary ContractFilter where
  arbitrary = ContractFilter <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary WithdrawalFilter where
  arbitrary = WithdrawalFilter <$> arbitrary
  shrink = genericShrink

arbitraryRequest :: StRes a -> Gen (Request a)
arbitraryRequest = \case
  TokContractHeaders -> ReqContractHeaders <$> arbitrary <*> arbitrary
  TokContractState -> ReqContractState <$> arbitrary
  TokTransaction -> ReqTransaction <$> arbitrary
  TokTransactions -> ReqTransactions <$> arbitrary
  TokWithdrawal -> ReqWithdrawal <$> arbitrary
  TokWithdrawals -> ReqWithdrawals <$> arbitrary <*> arbitrary
  TokBoth a b -> resized (`div` 2) $ ReqBoth <$> arbitraryRequest a <*> arbitraryRequest b

shrinkRequest :: Request a -> [Request a]
shrinkRequest = \case
  ReqContractHeaders cFilter range -> fold
    [  ReqContractHeaders <$> shrink cFilter <*> pure range
    , ReqContractHeaders cFilter <$> shrink range
    ]
  ReqContractState contractId -> ReqContractState <$> shrink contractId
  ReqTransaction txId -> ReqTransaction <$> shrink txId
  ReqTransactions contractId -> ReqTransactions <$> shrink contractId
  ReqWithdrawal txId -> ReqWithdrawal <$> shrink txId
  ReqWithdrawals wFilter range -> fold
    [ ReqWithdrawals <$> shrink wFilter <*> pure range
    , ReqWithdrawals wFilter <$> shrink range
    ]
  ReqBoth a b -> fold
    [ [ ReqBoth a' b | a' <- shrinkRequest a ]
    , [ ReqBoth a b' | b' <- shrinkRequest b ]
    ]

arbitraryResponse :: StRes a -> Gen a
arbitraryResponse = \case
  TokContractHeaders -> arbitrary
  TokContractState -> arbitrary
  TokTransaction -> arbitrary
  TokTransactions -> arbitrary
  TokWithdrawal -> arbitrary
  TokWithdrawals -> arbitrary
  TokBoth a b -> resized (`div` 2) $ (,) <$> arbitraryResponse a <*> arbitraryResponse b

shrinkResponse :: StRes a -> a -> [a]
shrinkResponse = \case
  TokContractHeaders -> shrink
  TokContractState -> shrink
  TokTransaction -> shrink
  TokTransactions -> shrink
  TokWithdrawal -> shrink
  TokWithdrawals -> shrink
  TokBoth ta tb -> \(a, b) -> fold
    [ [ (a', b) | a' <- shrinkResponse ta a ]
    , [ (a, b') | b' <- shrinkResponse tb b ]
    ]
