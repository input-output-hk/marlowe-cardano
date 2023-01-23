{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.History.CommandSpec
  ( spec
  ) where

import Data.Void (absurd)
import GHC.Show (showSpace)
import qualified Language.Marlowe.Runtime.History.Api as History
import Language.Marlowe.Runtime.History.Gen ()
import Network.Protocol.Codec.Spec
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Types
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = describe "HistoryCommand" do
  prop "Has a lawful Job protocol codec" $ checkPropCodec genByteStringSplits (codecJob @History.HistoryCommand)

instance CommandEq History.HistoryCommand where
  jobIdEq = \case
  commandEq = \case
    History.FollowContract contractId -> \case
      History.FollowContract contractId' -> contractId == contractId'
    History.StopFollowingContract contractId -> \case
      History.StopFollowingContract contractId' -> contractId == contractId'
  statusEq = \case
    History.TagFollowContract -> (==)
    History.TagStopFollowingContract -> (==)
  errEq = \case
    History.TagFollowContract -> (==)
    History.TagStopFollowingContract -> (==)
  resultEq = \case
    History.TagFollowContract -> (==)
    History.TagStopFollowingContract -> (==)

instance ShowCommand History.HistoryCommand where
  showsPrecJobId _ = \case
  showsPrecTag _ = showString . \case
    History.TagFollowContract -> "TagFollowContract"
    History.TagStopFollowingContract -> "TagStopFollowingContract"
  showsPrecCommand p = \case
    History.FollowContract contractId -> showParen (p >= 1)
      ( showString "FollowContract"
      . showSpace
      . showsPrec 11 contractId
      )

    History.StopFollowingContract contractId -> showParen (p >= 1)
      ( showString "StopFollowingContract"
      . showSpace
      . showsPrec 11 contractId
      )
  showsPrecStatus p = \case
    History.TagFollowContract -> showsPrec p
    History.TagStopFollowingContract -> showsPrec p
  showsPrecErr p = \case
    History.TagFollowContract -> showsPrec p
    History.TagStopFollowingContract -> showsPrec p
  showsPrecResult p = \case
    History.TagFollowContract -> showsPrec p
    History.TagStopFollowingContract -> showsPrec p

instance ArbitraryCommand History.HistoryCommand where
  arbitraryJobId _ = Nothing
  arbitraryTag = elements [SomeTag History.TagFollowContract, SomeTag History.TagStopFollowingContract]
  arbitraryCmd = \case
    History.TagFollowContract -> History.FollowContract <$> arbitrary
    History.TagStopFollowingContract -> History.StopFollowingContract <$> arbitrary
  arbitraryStatus _ = Nothing
  arbitraryErr = \case
    History.TagFollowContract -> Just arbitrary
    History.TagStopFollowingContract -> Nothing
  arbitraryResult = \case
    History.TagFollowContract -> arbitrary
    History.TagStopFollowingContract -> arbitrary
  shrinkCommand = \case
    History.FollowContract _ -> []
    History.StopFollowingContract _ -> []
  shrinkErr = \case
    History.TagFollowContract -> const []
    History.TagStopFollowingContract -> absurd
  shrinkResult = \case
    History.TagFollowContract -> shrink
    History.TagStopFollowingContract -> shrink
  shrinkStatus = \case
    History.TagFollowContract -> absurd
    History.TagStopFollowingContract -> absurd
  shrinkJobId = \case
