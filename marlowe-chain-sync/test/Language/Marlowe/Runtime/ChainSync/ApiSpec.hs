module Language.Marlowe.Runtime.ChainSync.ApiSpec
  where

import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.ChainSync.Gen ()
import Network.Protocol.Codec (binaryCodec)
import Network.Protocol.Codec.Spec (checkPropCodec, genByteStringSplits)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "Language.Marlowe.Runtime.ChainSync.Api" do
  describe "ChainSeek protocol" do
    prop "It has a lawful codec" $ checkPropCodec genByteStringSplits $ binaryCodec @_ @RuntimeChainSeek
  describe "ChainSyncQuery" do
    prop "It has a lawful Query protocol codec" $ checkPropCodec genByteStringSplits $ binaryCodec @_ @(Query ChainSyncQuery)
  describe "ChainSyncCommand" do
    prop "It has a lawful Job protocol codec" $ checkPropCodec genByteStringSplits $ binaryCodec @_ @(Job ChainSyncCommand)
