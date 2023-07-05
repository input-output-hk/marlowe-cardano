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

instance Arbitrary ImportError where
  arbitrary =
    frequency
      [ (10, LinkError <$> arbitrary)
      , (1, ContinuationNotInStore <$> arbitrary)
      ]
  shrink = genericShrink

instance ArbitraryMessage MarloweTransfer where
  arbitraryMessage =
    frequency
      [ (5, AnyMessageAndAgency (ClientAgency TokCanUpload) . MsgUpload <$> arbitrary)
      , (1, pure $ AnyMessageAndAgency (ClientAgency TokCanUpload) MsgImported)
      , (5, AnyMessageAndAgency (ServerAgency TokUpload) . MsgUploaded <$> arbitrary)
      , (5, AnyMessageAndAgency (ServerAgency TokUpload) . MsgUploadFailed <$> arbitrary)
      , (5, AnyMessageAndAgency (ClientAgency TokCanDownload) . MsgDownload <$> arbitrary)
      , (1, pure $ AnyMessageAndAgency (ClientAgency TokCanDownload) MsgCancel)
      , (5, AnyMessageAndAgency (ServerAgency TokDownload) . MsgDownloaded <$> arbitrary)
      , (1, pure $ AnyMessageAndAgency (ServerAgency TokDownload) MsgExported)
      , (5, AnyMessageAndAgency (ClientAgency TokIdle) . MsgStartExport <$> arbitrary)
      , (1, pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgStartImport)
      , (1, pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone)
      ]
  shrinkMessage _ = \case
    MsgStartImport -> []
    MsgStartExport hash -> MsgStartExport <$> shrink hash
    MsgDone -> []
    MsgUpload bundle -> MsgUpload <$> shrink bundle
    MsgUploaded hashes -> MsgUploaded <$> shrink hashes
    MsgUploadFailed err -> MsgUploadFailed <$> shrink err
    MsgImported -> []
    MsgDownload i -> MsgDownload <$> shrink i
    MsgDownloaded bundle -> MsgDownloaded <$> shrink bundle
    MsgCancel -> []
    MsgExported -> []
