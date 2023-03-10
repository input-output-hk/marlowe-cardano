module Language.Marlowe.Runtime.ChainSync.ApiSpec
  where

import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.ChainSync.Gen ()
import Network.Protocol.Codec.Spec (checkPropCodec, codecGoldenTests)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = describe "Language.Marlowe.Runtime.ChainSync.Api" do
  describe "ChainSeek protocol" do
    prop "It has a lawful codec" $ checkPropCodec @RuntimeChainSeek
    codecGoldenTests @RuntimeChainSeek "ChainSeek"
  describe "ChainSyncQuery" do
    prop "It has a lawful Query protocol codec" $ checkPropCodec @(Query ChainSyncQuery)
    codecGoldenTests @(Query ChainSyncQuery) "ChainSeekQuery"
  describe "ChainSyncCommand" do
    prop "It has a lawful Job protocol codec" $ checkPropCodec @(Job ChainSyncCommand)
