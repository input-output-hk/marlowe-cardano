{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.HeaderSyncSpec where

import Language.Marlowe.Protocol.HeaderSync.Types
import Language.Marlowe.Runtime.ChainSync.Gen (resized)
import Language.Marlowe.Runtime.Discovery.Gen ()
import Network.Protocol.Codec.Spec
import Network.Protocol.Handshake.Types (Handshake)
import Network.TypedProtocol.Codec
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "MarloweHeaderSync protocol" do
  prop "Has a lawful codec" $ checkPropCodec @(Handshake MarloweHeaderSync)
  codecGoldenTests @MarloweHeaderSync "MarloweHeaderSync"

instance ArbitraryMessage MarloweHeaderSync where
  arbitraryMessage =
    oneof
      [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgIntersect <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
      , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgRequestNext
      , do
          msg <- MsgNewHeaders <$> arbitrary <*> resized (min 30) arbitrary
          pure $ AnyMessageAndAgency (ServerAgency TokNext) msg
      , do
          msg <- MsgRollBackward <$> arbitrary
          pure $ AnyMessageAndAgency (ServerAgency TokNext) msg
      , pure $ AnyMessageAndAgency (ServerAgency TokNext) MsgWait
      , pure $ AnyMessageAndAgency (ClientAgency TokWait) MsgPoll
      , pure $ AnyMessageAndAgency (ClientAgency TokWait) MsgCancel
      , AnyMessageAndAgency (ServerAgency TokIntersect) . MsgIntersectFound <$> arbitrary
      , pure $ AnyMessageAndAgency (ServerAgency TokIntersect) MsgIntersectNotFound
      ]
  shrinkMessage agency = \case
    MsgIntersect points -> MsgIntersect <$> shrink points
    MsgDone -> []
    MsgRequestNext -> []
    MsgNewHeaders blockHeader headers -> case agency of
      ServerAgency TokNext -> MsgNewHeaders blockHeader <$> shrink headers
    MsgRollBackward _ -> []
    MsgWait -> []
    MsgPoll -> []
    MsgCancel -> []
    MsgIntersectFound _ -> []
    MsgIntersectNotFound -> []
