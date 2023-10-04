{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.BulkSyncSpec where

import Language.Marlowe.Protocol.BulkSync.Types
import Language.Marlowe.Runtime.ChainSync.Gen (resized)
import Language.Marlowe.Runtime.History.Gen ()
import Network.Protocol.Codec.Spec
import Network.Protocol.Handshake.Types (Handshake)
import Network.TypedProtocol.Codec
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "MarloweBulkSync protocol" do
  prop "Has a lawful codec" $ checkPropCodec @(Handshake MarloweBulkSync)
  codecGoldenTests @MarloweBulkSync "MarloweBulkSync"

instance ArbitraryMessage MarloweBulkSync where
  arbitraryMessage =
    oneof
      [ AnyMessageAndAgency (ClientAgency TokIdle) . MsgIntersect <$> arbitrary
      , pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
      , AnyMessageAndAgency (ClientAgency TokIdle) . MsgRequestNext <$> arbitrary
      , do
          msg <- MsgRollForward <$> resized (min 30) arbitrary <*> arbitrary
          pure $ AnyMessageAndAgency (ServerAgency TokNext) msg
      , do
          msg <- MsgRollBackward <$> arbitrary <*> arbitrary
          pure $ AnyMessageAndAgency (ServerAgency TokNext) msg
      , pure $ AnyMessageAndAgency (ServerAgency TokNext) MsgWait
      , pure $ AnyMessageAndAgency (ClientAgency TokPoll) MsgPoll
      , pure $ AnyMessageAndAgency (ClientAgency TokPoll) MsgCancel
      , fmap (AnyMessageAndAgency (ServerAgency TokIntersect)) . MsgIntersectFound <$> arbitrary <*> arbitrary
      , AnyMessageAndAgency (ServerAgency TokIntersect) . MsgIntersectNotFound <$> arbitrary
      ]
  shrinkMessage agency = \case
    MsgIntersect points -> MsgIntersect <$> shrink points
    MsgDone -> []
    MsgRequestNext i -> MsgRequestNext <$> shrink i
    MsgRollForward blocks tip -> case agency of
      ServerAgency TokNext -> flip MsgRollForward tip <$> shrink blocks
    MsgRollBackward _ _ -> []
    MsgWait -> []
    MsgPoll -> []
    MsgCancel -> []
    MsgIntersectFound _ _ -> []
    MsgIntersectNotFound _ -> []
