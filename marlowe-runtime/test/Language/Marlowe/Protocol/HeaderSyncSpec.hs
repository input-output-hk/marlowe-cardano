{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.HeaderSyncSpec
  where

import GHC.Show (showSpace)
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

instance MessageEq MarloweHeaderSync where
  messageEq = \case
    AnyMessageAndAgency _ (MsgIntersect points) -> \case
      AnyMessageAndAgency _ (MsgIntersect points') -> points == points'
      _ -> False
    AnyMessageAndAgency _ MsgDone -> \case
      AnyMessageAndAgency _ MsgDone -> True
      _ -> False
    AnyMessageAndAgency _ MsgRequestNext -> \case
      AnyMessageAndAgency _ MsgRequestNext -> True
      _ -> False
    AnyMessageAndAgency agency (MsgNewHeaders blockHeader headers) -> \case
      AnyMessageAndAgency agency' (MsgNewHeaders blockHeader' contractSteps') -> blockHeader == blockHeader' && case (agency, agency') of
        (ServerAgency TokNext, ServerAgency TokNext) -> headers == contractSteps'
      _ -> False
    AnyMessageAndAgency _ (MsgRollBackward point) -> \case
      AnyMessageAndAgency _ (MsgRollBackward point') -> point == point'
      _ -> False
    AnyMessageAndAgency _ MsgWait -> \case
      AnyMessageAndAgency _ MsgWait -> True
      _ -> False
    AnyMessageAndAgency _ MsgPoll -> \case
      AnyMessageAndAgency _ MsgPoll -> True
      _ -> False
    AnyMessageAndAgency _ MsgCancel -> \case
      AnyMessageAndAgency _ MsgCancel -> True
      _ -> False
    AnyMessageAndAgency _ (MsgIntersectFound point) -> \case
      AnyMessageAndAgency _ (MsgIntersectFound point') -> point == point'
      _ -> False
    AnyMessageAndAgency _ MsgIntersectNotFound -> \case
      AnyMessageAndAgency _ MsgIntersectNotFound -> True
      _ -> False

instance ShowProtocol MarloweHeaderSync where
  showsPrecMessage p agency = \case
    MsgIntersect points -> showParen (p >= 11)
      ( showString "MsgIntersect"
      . showSpace
      . showsPrec 11 points
      )
    MsgDone -> showString "MsgDone"
    MsgRequestNext -> showString "MsgRequestNext"
    MsgNewHeaders blockHeader headers -> showParen (p >= 11)
      ( showString "MsgNewHeaders"
      . showSpace
      . showsPrec 11 blockHeader
      . showSpace
      . case agency of ServerAgency TokNext -> showsPrec 11 headers
      )
    MsgRollBackward point -> showParen (p >= 11)
      ( showString "MsgRollBackward"
      . showSpace
      . showsPrec 11 point
      )
    MsgWait -> showString "MsgWait"
    MsgPoll -> showString "MsgPoll"
    MsgCancel -> showString "MsgCancel"
    MsgIntersectFound point -> showParen (p >= 11)
      ( showString "MsgIntersectFound"
      . showSpace
      . showsPrec 11 point
      )
    MsgIntersectNotFound -> showString "MsgIntersectNotFound"

  showsPrecServerHasAgency _ = showString . \case
    TokNext -> "TokNext"
    TokIntersect -> "TokIntersect"

  showsPrecClientHasAgency _ = showString . \case
    TokIdle -> "TokIdle"
    TokWait -> "TokWait"

instance ArbitraryMessage MarloweHeaderSync where
  arbitraryMessage = oneof
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
