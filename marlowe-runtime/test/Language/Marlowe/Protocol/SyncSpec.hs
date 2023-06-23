{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.SyncSpec where

import Language.Marlowe.Protocol.Sync.Types
import Language.Marlowe.Runtime.ChainSync.Gen (resized)
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.History.Gen ()
import Network.Protocol.Codec.Spec
import Network.Protocol.Handshake.Types (Handshake)
import Network.TypedProtocol.Codec
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "MarloweSync protocol" do
  prop "Has a lawful codec" $ checkPropCodec @(Handshake MarloweSync)
  codecGoldenTests @MarloweSync "MarloweSync"

instance ArbitraryMessage MarloweSync where
  arbitraryMessage =
    oneof
      [ AnyMessageAndAgency (ClientAgency TokInit) . MsgFollowContract <$> arbitrary
      , do
          msg <- MsgIntersect <$> arbitrary <*> pure Core.MarloweV1 <*> listOf arbitrary
          pure $ AnyMessageAndAgency (ClientAgency TokInit) msg
      , pure $ AnyMessageAndAgency (ServerAgency TokFollow) MsgContractNotFound
      , do
          msg <- MsgContractFound <$> arbitrary <*> pure Core.MarloweV1 <*> arbitrary
          pure $ AnyMessageAndAgency (ServerAgency TokFollow) msg
      , pure $ AnyMessageAndAgency (ClientAgency (TokIdle Core.MarloweV1)) MsgDone
      , pure $ AnyMessageAndAgency (ClientAgency (TokIdle Core.MarloweV1)) MsgRequestNext
      , do
          msg <- MsgRollForward <$> arbitrary <*> resized (min 30) arbitrary
          pure $ AnyMessageAndAgency (ServerAgency (TokNext Core.MarloweV1)) msg
      , do
          msg <- MsgRollBackward <$> arbitrary
          pure $ AnyMessageAndAgency (ServerAgency (TokNext Core.MarloweV1)) msg
      , pure $ AnyMessageAndAgency (ServerAgency (TokNext Core.MarloweV1)) MsgRollBackCreation
      , pure $ AnyMessageAndAgency (ServerAgency (TokNext Core.MarloweV1)) MsgWait
      , pure $ AnyMessageAndAgency (ClientAgency (TokWait Core.MarloweV1)) MsgPoll
      , pure $ AnyMessageAndAgency (ClientAgency (TokWait Core.MarloweV1)) MsgCancel
      , AnyMessageAndAgency (ServerAgency (TokIntersect Core.MarloweV1)) . MsgIntersectFound <$> arbitrary
      , pure $ AnyMessageAndAgency (ServerAgency (TokIntersect Core.MarloweV1)) MsgIntersectNotFound
      ]

  shrinkMessage agency = \case
    MsgFollowContract _ -> []
    MsgIntersect contractId Core.MarloweV1 points -> MsgIntersect contractId Core.MarloweV1 <$> shrink points
    MsgContractNotFound -> []
    MsgContractFound blockHeader Core.MarloweV1 createStep -> MsgContractFound blockHeader Core.MarloweV1 <$> shrink createStep
    MsgDone -> []
    MsgRequestNext -> []
    MsgRollForward blockHeader contractSteps -> case agency of
      ServerAgency (TokNext Core.MarloweV1) -> MsgRollForward blockHeader <$> shrink contractSteps
    MsgRollBackward _ -> []
    MsgRollBackCreation -> []
    MsgWait -> []
    MsgPoll -> []
    MsgCancel -> []
    MsgIntersectFound _ -> []
    MsgIntersectNotFound -> []
