{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.SyncSpec
  ( spec
  ) where

import qualified Data.Map as Map
import GHC.Show (showSpace)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Protocol.Common
import Language.Marlowe.Protocol.Sync.Codec (codecMarloweSync)
import Language.Marlowe.Protocol.Sync.Types
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Core.Api as Payout (Payout(..))
import qualified Language.Marlowe.Runtime.Core.Api as Transaction (Transaction(..))
import qualified Language.Marlowe.Runtime.Core.Api as TransactionScriptOutput (TransactionScriptOutput(..))
import qualified Language.Marlowe.Runtime.History.Api as History
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusCurrencySymbol)
import Network.Protocol.Codec.Spec
import Network.TypedProtocol.Codec
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "MarloweSync protocol" do
  prop "Has a lawful codec" $ checkPropCodec genByteStringSplits codecMarloweSync

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
        msg <- MsgContractFound <$> arbitrary <*> pure Core.MarloweV1 <*> genCreateStep Core.MarloweV1
        pure $ AnyMessageAndAgency (ServerAgency TokFollow) msg
    , pure $ AnyMessageAndAgency (ClientAgency (TokIdle Core.MarloweV1)) MsgDone
    , pure $ AnyMessageAndAgency (ClientAgency (TokIdle Core.MarloweV1)) MsgRequestNext
    , do
        msg <- MsgRollForward <$> arbitrary <*> sized \size -> resize (min size 30) $ listOf (genContractStep Core.MarloweV1)
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
    where
      genContractStep :: Core.MarloweVersion v -> Gen (History.ContractStep v)
      genContractStep version = oneof
        [ History.ApplyTransaction <$> genTransaction version
        , History.RedeemPayout <$> genRedeemStep version
        ]

      genRedeemStep :: Core.MarloweVersion v -> Gen (History.RedeemStep v)
      genRedeemStep version = History.RedeemStep
        <$> arbitrary
        <*> arbitrary
        <*> genPayoutDatum version

      genCreateStep :: Core.MarloweVersion v -> Gen (History.CreateStep v)
      genCreateStep version = History.CreateStep
        <$> genTransactionScriptOutput version
        <*> arbitrary
        <*> arbitrary

      genTransactionOutput :: Core.MarloweVersion v -> Gen (Core.TransactionOutput v)
      genTransactionOutput version = Core.TransactionOutput
        <$> (Map.fromList <$> listOf ((,) <$> arbitrary <*> genPayout version))
        <*> oneof [pure Nothing, Just <$> genTransactionScriptOutput version]

      genTransactionScriptOutput :: Core.MarloweVersion v -> Gen (Core.TransactionScriptOutput v)
      genTransactionScriptOutput version = Core.TransactionScriptOutput
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> genDatum version

      genTransaction :: Core.MarloweVersion v -> Gen (Core.Transaction v)
      genTransaction version = Core.Transaction
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> genUTCTime
        <*> genUTCTime
        <*> genInputs version
        <*> genTransactionOutput version

      genInputs :: Core.MarloweVersion v -> Gen (Core.Inputs v)
      genInputs Core.MarloweV1 = listOf arbitrary

      genPayout :: Core.MarloweVersion v -> Gen (Core.Payout v)
      genPayout version = Core.Payout
        <$> arbitrary
        <*> arbitrary
        <*> genPayoutDatum version

      genPayoutDatum :: Core.MarloweVersion v -> Gen (Core.PayoutDatum v)
      genPayoutDatum Core.MarloweV1 = arbitrary

      genDatum :: Core.MarloweVersion v -> Gen (Core.Datum v)
      genDatum Core.MarloweV1 = V1.MarloweData
        <$> (V1.MarloweParams . toPlutusCurrencySymbol <$> arbitrary)
        <*> arbitrary
        <*> arbitrary

  shrinkMessage agency = \case
    MsgFollowContract _ -> []
    MsgIntersect contractId Core.MarloweV1 points -> MsgIntersect contractId Core.MarloweV1 <$> shrinkList (const []) points
    MsgContractNotFound -> []
    MsgContractFound blockHeader version createStep -> MsgContractFound blockHeader version <$> shrinkCreateStep version createStep
    MsgDone -> []
    MsgRequestNext -> []
    MsgRollForward blockHeader contractSteps -> case agency of
      ServerAgency (TokNext version) -> MsgRollForward blockHeader <$> shrinkList (shrinkContractStep version) contractSteps
    MsgRollBackward _ -> []
    MsgRollBackCreation -> []
    MsgWait -> []
    MsgPoll -> []
    MsgCancel -> []
    MsgIntersectFound _ -> []
    MsgIntersectNotFound -> []
    where
      shrinkCreateStep :: Core.MarloweVersion v -> History.CreateStep v -> [History.CreateStep v]
      shrinkCreateStep version History.CreateStep{..} = []
        <> [ History.CreateStep {..} { History.createOutput = createOutput' } | createOutput' <- shrinkTransactionScriptOutput version createOutput ]
        <> [ History.CreateStep {..} { History.metadata = metadata' } | metadata' <- shrink metadata ]

      shrinkContractStep :: Core.MarloweVersion v -> History.ContractStep v -> [History.ContractStep v]
      shrinkContractStep version = \case
        History.ApplyTransaction tx -> History.ApplyTransaction <$> shrinkTransaction version tx
        History.RedeemPayout _ -> []

      shrinkTransaction :: Core.MarloweVersion v -> Core.Transaction v -> [Core.Transaction v]
      shrinkTransaction version Core.Transaction{..} = []
        <> [ Core.Transaction{..} { Transaction.metadata = metadata' } | metadata' <- shrink metadata ]
        <> [ Core.Transaction{inputs = inputs', transactionId, contractId, metadata, blockHeader, validityLowerBound, validityUpperBound, output} | inputs' <- shrinkInputs version inputs ]
        <> [ Core.Transaction{..} { Transaction.output = output' } | output' <- shrinkTransactionOutput version output ]

      shrinkInputs :: Core.MarloweVersion v -> Core.Inputs v -> [Core.Inputs v]
      shrinkInputs Core.MarloweV1 = shrinkList shrink

      shrinkTransactionOutput :: Core.MarloweVersion v -> Core.TransactionOutput v -> [Core.TransactionOutput v]
      shrinkTransactionOutput version Core.TransactionOutput{..} = []
        <> [ Core.TransactionOutput{..} { Core.payouts = payouts' } | payouts' <- shrinkMap (shrinkPayout version) payouts ]

      shrinkPayout :: Core.MarloweVersion v -> Core.Payout v -> [Core.Payout v]
      shrinkPayout _ Core.Payout{..} = [ Core.Payout{..} { Payout.assets = assets' } | assets' <- shrink assets ]

      shrinkTransactionScriptOutput :: Core.MarloweVersion v -> Core.TransactionScriptOutput v -> [Core.TransactionScriptOutput v]
      shrinkTransactionScriptOutput version Core.TransactionScriptOutput{..} = []
        <> [ Core.TransactionScriptOutput{..} { TransactionScriptOutput.assets = assets' } | assets' <- shrink assets ]
        <> [ Core.TransactionScriptOutput{datum = datum', address, assets, utxo} | datum' <- shrinkDatum version datum ]

      shrinkDatum :: Core.MarloweVersion v -> Core.Datum v -> [Core.Datum v]
      shrinkDatum Core.MarloweV1 V1.MarloweData{..} = []
        <> [ V1.MarloweData{..} { V1.marloweState = marloweState' } | marloweState' <- shrink marloweState ]
        <> [ V1.MarloweData{..} { V1.marloweContract = marloweContract' } | marloweContract' <- shrink marloweContract ]
