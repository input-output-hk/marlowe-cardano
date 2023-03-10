{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.SyncSpec
  where

import GHC.Show (showSpace)
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

instance MessageEq MarloweSync where
  messageEq = \case
    AnyMessageAndAgency _ (MsgFollowContract contractId) -> \case
      AnyMessageAndAgency _ (MsgFollowContract contractId') -> contractId == contractId'
      _ -> False
    AnyMessageAndAgency _ (MsgIntersect contractId Core.MarloweV1 points) -> \case
      AnyMessageAndAgency _ (MsgIntersect contractId' Core.MarloweV1 points') -> contractId == contractId' && points == points'
      _ -> False
    AnyMessageAndAgency _ MsgContractNotFound -> \case
      AnyMessageAndAgency _ MsgContractNotFound -> True
      _ -> False
    AnyMessageAndAgency _ (MsgContractFound blockHeader Core.MarloweV1 createStep) -> \case
      AnyMessageAndAgency _ (MsgContractFound blockHeader' Core.MarloweV1 createStep') -> blockHeader == blockHeader' && createStep == createStep'
      _ -> False
    AnyMessageAndAgency _ MsgDone -> \case
      AnyMessageAndAgency _ MsgDone -> True
      _ -> False
    AnyMessageAndAgency _ MsgRequestNext -> \case
      AnyMessageAndAgency _ MsgRequestNext -> True
      _ -> False
    AnyMessageAndAgency agency (MsgRollForward blockHeader contractSteps) -> \case
      AnyMessageAndAgency agency' (MsgRollForward blockHeader' contractSteps') -> blockHeader == blockHeader' && case (agency, agency') of
        (ServerAgency (TokNext Core.MarloweV1), ServerAgency (TokNext Core.MarloweV1)) -> contractSteps == contractSteps'
      _ -> False
    AnyMessageAndAgency _ (MsgRollBackward point) -> \case
      AnyMessageAndAgency _ (MsgRollBackward point') -> point == point'
      _ -> False
    AnyMessageAndAgency _ MsgRollBackCreation -> \case
      AnyMessageAndAgency _ MsgRollBackCreation -> True
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

instance ShowProtocol MarloweSync where
  showsPrecMessage p agency = \case
    MsgFollowContract contractId -> showParen (p >= 11)
      ( showString "MsgFollowContract"
      . showSpace
      . showsPrec 11 contractId
      )
    MsgIntersect contractId Core.MarloweV1 points -> showParen (p >= 11)
      ( showString "MsgIntersect"
      . showSpace
      . showsPrec 11 contractId
      . showSpace
      . showsPrec 11 Core.MarloweV1
      . showSpace
      . showsPrec 11 points
      )
    MsgContractNotFound -> showString "MsgContractNotFound"
    MsgContractFound blockHeader Core.MarloweV1 createStep -> showParen (p >= 11)
      ( showString "MsgContractFound"
      . showSpace
      . showsPrec 11 blockHeader
      . showSpace
      . showsPrec 11 Core.MarloweV1
      . showSpace
      . showsPrec 11 createStep
      )
    MsgDone -> showString "MsgDone"
    MsgRequestNext -> showString "MsgRequestNext"
    MsgRollForward blockHeader contractSteps -> showParen (p >= 11)
      ( showString "MsgRollForward"
      . showSpace
      . showsPrec 11 blockHeader
      . showSpace
      . case agency of ServerAgency (TokNext Core.MarloweV1) -> showsPrec 11 contractSteps
      )
    MsgRollBackward point -> showParen (p >= 11)
      ( showString "MsgRollBackward"
      . showSpace
      . showsPrec 11 point
      )
    MsgRollBackCreation -> showString "MsgRollBackCreation"
    MsgWait -> showString "MsgWait"
    MsgPoll -> showString "MsgPoll"
    MsgCancel -> showString "MsgCancel"
    MsgIntersectFound point -> showParen (p >= 11)
      ( showString "MsgIntersectFound"
      . showSpace
      . showsPrec 11 point
      )
    MsgIntersectNotFound -> showString "MsgIntersectNotFound"

  showsPrecServerHasAgency p = \case
    TokFollow -> showString "TokFollow"
    TokNext version -> showParen (p >= 11)
      ( showString "TokNext"
      . showSpace
      . showsPrec 11 version
      )
    TokIntersect version -> showParen (p >= 11)
      ( showString "TokIntersect"
      . showSpace
      . showsPrec 11 version
      )

  showsPrecClientHasAgency p = \case
    TokInit -> showString "TokInit"
    TokIdle version -> showParen (p >= 11)
      ( showString "TokIdle"
      . showSpace
      . showsPrec 11 version
      )
    TokWait version -> showParen (p >= 11)
      ( showString "TokWait"
      . showSpace
      . showsPrec 11 version
      )

instance ArbitraryMessage MarloweSync where
  arbitraryMessage = oneof
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
