{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.History.QuerySpec
  where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (absurd)
import GHC.Show (showSpace)
import Language.Marlowe.Protocol.Common
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Language.Marlowe.Runtime.History.Api as History
import Network.Protocol.Codec.Spec
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "HistoryQuery" do
  prop "Has a lawful Query protocol codec" $ checkPropCodec genByteStringSplits (codecQuery @History.HistoryQuery)

instance QueryEq History.HistoryQuery where
  queryEq = \case
    History.GetFollowedContracts -> \case
      History.GetFollowedContracts -> True
    History.GetStatuses contractIds -> \case
      History.GetStatuses contractIds' -> contractIds == contractIds'
  delimiterEq = \case
    History.TagGetFollowedContracts -> (==)
    History.TagGetStatuses -> (==)
  errEq = \case
    History.TagGetFollowedContracts -> (==)
    History.TagGetStatuses -> (==)
  resultEq = \case
    History.TagGetFollowedContracts -> (==)
    History.TagGetStatuses -> (==)

instance ShowQuery History.HistoryQuery where
  showsPrecTag _ = showString . \case
    History.TagGetFollowedContracts -> "TagGetFollowedContracts"
    History.TagGetStatuses -> "TagGetStatuses"
  showsPrecQuery p = \case
    History.GetFollowedContracts -> showString "GetFollowedContracts"
    History.GetStatuses contractIds -> showParen (p >= 1)
      ( showString "TagGetStatuses"
      . showSpace
      . showsPrec 11 contractIds
      )
  showsPrecDelimiter p = \case
    History.TagGetFollowedContracts -> showsPrec p
    History.TagGetStatuses -> showsPrec p
  showsPrecErr p = \case
    History.TagGetFollowedContracts -> showsPrec p
    History.TagGetStatuses -> showsPrec p
  showsPrecResult p = \case
    History.TagGetFollowedContracts -> showsPrec p
    History.TagGetStatuses -> showsPrec p

instance ArbitraryQuery History.HistoryQuery where
  arbitraryTag = elements [SomeTag History.TagGetFollowedContracts, SomeTag History.TagGetStatuses]
  arbitraryQuery = \case
    History.TagGetFollowedContracts -> pure History.GetFollowedContracts
    History.TagGetStatuses -> History.GetStatuses . Set.fromList <$> listOf genContractId
  arbitraryDelimiter = \case
    History.TagGetFollowedContracts -> Just <$> genContractId
    History.TagGetStatuses -> pure Nothing
  arbitraryErr = \case
    History.TagGetFollowedContracts -> pure Nothing
    History.TagGetStatuses -> pure Nothing
  arbitraryResults = \case
    History.TagGetFollowedContracts -> Map.fromList <$> listOf ((,) <$> genContractId <*> genFollowerStatus)
    History.TagGetStatuses -> Map.fromList <$> listOf ((,) <$> genContractId <*> genFollowerStatus)
  shrinkQuery = \case
    History.GetFollowedContracts -> []
    History.GetStatuses contractIds -> History.GetStatuses . Set.fromDistinctAscList <$> shrinkList (const []) (Set.toAscList contractIds)
  shrinkErr = \case
    History.TagGetFollowedContracts -> absurd
    History.TagGetStatuses -> absurd
  shrinkResults = \case
    History.TagGetFollowedContracts -> shrinkMap (const [])
    History.TagGetStatuses -> shrinkMap (const [])
  shrinkDelimiter = \case
    History.TagGetFollowedContracts -> const []
    History.TagGetStatuses -> absurd


genFollowerStatus :: Gen History.FollowerStatus
genFollowerStatus = oneof
  [ pure History.Pending
  , History.Following <$> genSomeMarloweVersion
  , History.Waiting <$> genSomeMarloweVersion
  , History.Finished <$> genSomeMarloweVersion
  , History.Failed <$> genContractHistoryError
  ]

genContractHistoryError :: Gen History.ContractHistoryError
genContractHistoryError = oneof
  [ pure History.HansdshakeFailed
  , History.FindTxFailed <$> genTxError
  , History.ExtractContractFailed <$> genExtractCreationError
  , History.FollowScriptUTxOFailed <$> genUTxOError
  , History.FollowPayoutUTxOsFailed . Map.fromList <$> listOf ((,) <$> genTxOutRef <*> genUTxOError)
  , History.ExtractMarloweTransactionFailed <$> genExtractMarloweTransactionError
  , History.PayoutUTxONotFound <$> genTxOutRef
  , pure History.CreateTxRolledBack
  ]

genTxError :: Gen Chain.TxError
genTxError = oneof
  [ pure Chain.TxNotFound
  , Chain.TxInPast <$> genBlockHeader
  ]

genExtractCreationError :: Gen History.ExtractCreationError
genExtractCreationError = elements
  [ History.TxIxNotFound
  , History.ByronAddress
  , History.NonScriptAddress
  , History.InvalidScriptHash
  , History.NoCreateDatum
  , History.InvalidCreateDatum
  , History.NotCreationTransaction
  ]

genUTxOError :: Gen Chain.UTxOError
genUTxOError = oneof
  [ pure Chain.UTxONotFound
  , Chain.UTxOSpent <$> genTxId
  ]

genExtractMarloweTransactionError :: Gen History.ExtractMarloweTransactionError
genExtractMarloweTransactionError = oneof
  [ pure History.TxInNotFound
  , pure History.NoRedeemer
  , pure History.InvalidRedeemer
  , pure History.NoTransactionDatum
  , pure History.InvalidTransactionDatum
  , History.NoPayoutDatum <$> genTxOutRef
  , History.InvalidPayoutDatum <$> genTxOutRef
  , pure History.InvalidValidityRange
  , pure History.SlotConversionFailed
  ]
