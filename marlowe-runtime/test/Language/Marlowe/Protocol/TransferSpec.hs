{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.TransferSpec where

import Language.Marlowe.Object.Gen ()
import Language.Marlowe.Protocol.Transfer.Types
import Language.Marlowe.Runtime.ChainSync.Gen ()
import Network.Protocol.Codec.Spec
import Network.TypedProtocol
import Network.TypedProtocol.Codec
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = do
  describe "MarloweTransfer protocol" do
    prop "Has a lawful codec" $ checkPropCodec @MarloweTransfer
    codecGoldenTests @MarloweTransfer "MarloweTransfer"

instance ArbitraryMessage MarloweTransfer where
  arbitraryMessage =
    frequency
      [ (10, AnyMessageAndAgency (ClientAgency TokIdle) . MsgTransfer <$> arbitrary)
      , (10, AnyMessageAndAgency (ServerAgency TokTransfer) . MsgTransferred <$> arbitrary)
      , (10, AnyMessageAndAgency (ServerAgency TokTransfer) . MsgTransferFailed <$> arbitrary)
      , (1, pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone)
      ]

  shrinkMessage _ = \case
    MsgTransfer bundle -> MsgTransfer <$> shrink bundle
    MsgTransferred hashes -> MsgTransferred <$> shrink hashes
    MsgTransferFailed err -> MsgTransferFailed <$> shrink err
    MsgDone -> []
