{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Protocol.ChainSeek.CodecSpec
  ( spec
  ) where

import Data.Binary (Binary(get), getWord8, put, putWord8)
import Data.Data (type (:~:)(Refl))
import Data.Functor.Identity (runIdentity)
import Data.Text as T
import Network.Protocol.ChainSeek.Codec (codecChainSeek)
import Network.Protocol.ChainSeek.Types
  (ChainSeek(..), ClientHasAgency(..), Message(..), Query(..), ServerHasAgency(..), StNextKind(..), TokNextKind(..))
import qualified Network.Protocol.ChainSeek.Types as ChainSeek
import Network.TypedProtocol.Codec
  ( AnyMessage(AnyMessage)
  , AnyMessageAndAgency(AnyMessageAndAgency)
  , PeerHasAgency(ClientAgency, ServerAgency)
  , prop_codec
  , prop_codecM
  )
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary), Gen)

data TestQuery err result where
  TestQuery :: TestQuery () ()

instance Query TestQuery where
  data Tag TestQuery err result where
    TestTag :: ChainSeek.Tag TestQuery () ()

  tagFromQuery = \case
    TestQuery -> TestTag
  tagEq TestTag TestTag = Just (Refl, Refl)

  putTag = \case
    TestTag -> putWord8 0x01
  getTag = getWord8 >>= \case
    0x01 -> pure $ ChainSeek.SomeTag TestTag
    tag -> fail $ "Invalid query tag: " <> show tag
  putQuery = \case
    TestQuery -> mempty
  getQuery = \case
    TestTag -> pure TestQuery
  putErr = \case
    TestTag -> put
  getErr = \case
    TestTag -> get
  putResult = \case
    TestTag -> put
  getResult = \case
    TestTag -> get

instance Eq (AnyMessage (ChainSeek TestQuery () ())) where
  (AnyMessage (MsgRequestHandshake sv1)) == (AnyMessage (MsgRequestHandshake sv2))  = sv1 == sv2
  (AnyMessage MsgConfirmHandshake) == (AnyMessage MsgConfirmHandshake)  = True
  (AnyMessage (MsgRejectHandshake sv1)) == (AnyMessage (MsgRejectHandshake sv2))  = sv1 == sv2
  (AnyMessage (MsgQueryNext TestQuery)) == (AnyMessage (MsgQueryNext TestQuery))  = True
  (AnyMessage MsgRejectQuery {}) == (AnyMessage MsgRejectQuery {})  = True
  (AnyMessage MsgRollForward {}) == (AnyMessage MsgRollForward {})  = True
  (AnyMessage MsgRollBackward {}) == (AnyMessage MsgRollBackward {})  = True
  (AnyMessage MsgWait {}) == (AnyMessage MsgWait {})  = True
  (AnyMessage MsgPing {}) == (AnyMessage MsgPing {})  = True
  (AnyMessage MsgPong {}) == (AnyMessage MsgPong {})  = True
  (AnyMessage MsgDone) == (AnyMessage MsgDone) = True
  _ == _ = False

genSchemaVersion :: Gen ChainSeek.SchemaVersion
genSchemaVersion = ChainSeek.SchemaVersion . T.pack <$> arbitrary

genMsgRequestHandshake :: Gen (Message (ChainSeek TestQuery () ()) 'StInit 'StHandshake)
genMsgRequestHandshake =
  MsgRequestHandshake <$> genSchemaVersion

genMsgRejectHandshake :: Gen (Message (ChainSeek TestQuery () ()) 'StHandshake 'StFault)
genMsgRejectHandshake =
  MsgRejectHandshake . pure <$> genSchemaVersion

spec :: Spec
spec = do
  prop "MsgRequestHandshake serialization roundtrip" do
    msg <- genMsgRequestHandshake
    prop_codecM codecChainSeek (AnyMessageAndAgency (ClientAgency TokInit) msg)
  it "MsgConfirmHandshake serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) 'StHandshake 'StIdle
      msg = MsgConfirmHandshake
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ServerAgency TokHandshake) msg) `shouldBe` True
  prop "MsgRejectHandshake serialization roundtrip" do
    msg <- genMsgRejectHandshake
    prop_codecM codecChainSeek (AnyMessageAndAgency (ServerAgency TokHandshake) msg)
  it "MsgQueryNext serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) 'StIdle ('StNext () () 'StCanAwait)
      msg = MsgQueryNext TestQuery
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ClientAgency TokIdle) msg) `shouldBe` True
  it "MsgRejectQuery serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) ('StNext () () 'StCanAwait) 'StIdle
      msg = MsgRejectQuery () ()
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ServerAgency (TokNext tag TokCanAwait)) msg) `shouldBe` True
  it "MsgRollForward serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) ('StNext () () 'StCanAwait) 'StIdle
      msg = MsgRollForward () () ()
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ServerAgency (TokNext tag TokCanAwait)) msg) `shouldBe` True
  it "MsgRollBackward serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) ('StNext () () 'StCanAwait) 'StIdle
      msg = MsgRollBackward () ()
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ServerAgency (TokNext tag TokCanAwait)) msg) `shouldBe` True
  it "MsgWait serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) ('StNext () () 'StCanAwait) ('StNext () () 'StMustReply)
      msg = MsgWait
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ServerAgency (TokNext tag TokCanAwait)) msg) `shouldBe` True
  it "MsgDone serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) 'StIdle 'StDone
      msg = MsgDone
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ClientAgency TokIdle) msg) `shouldBe` True
  it "MsgPing serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) ('StNext () () 'StMustReply) ('StPing () ())
      msg = MsgPing
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ServerAgency (TokNext tag TokMustReply)) msg) `shouldBe` True
  it "MsgPong serialization roundtrip" $ do
    let
      msg :: Message (ChainSeek TestQuery () ()) ('StPing () ()) ('StNext () () 'StMustReply)
      msg = MsgPong
    prop_codec runIdentity codecChainSeek (AnyMessageAndAgency (ClientAgency TokPing) msg) `shouldBe` True


