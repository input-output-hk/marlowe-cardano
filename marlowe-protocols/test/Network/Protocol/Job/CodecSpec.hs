{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Protocol.Job.CodecSpec
  ( spec
  ) where

import Data.Binary (Binary(get), getWord8, put, putWord8)
import Data.Data (type (:~:)(Refl))
import Data.Functor.Identity (runIdentity)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Types as Job
import Network.TypedProtocol.Codec
  ( AnyMessage(AnyMessage)
  , AnyMessageAndAgency(AnyMessageAndAgency)
  , PeerHasAgency(ClientAgency, ServerAgency)
  , prop_codec
  , prop_codecM
  )
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary(arbitrary))

data TestCommand status err result where
  TestCommand :: TestCommand () () ()

instance Command TestCommand where

  data Tag TestCommand status err result where
    TestTag :: Job.Tag TestCommand () () ()

  data JobId TestCommand status err result where
    TestJobId :: Int -> Job.JobId TestCommand () () ()

  tagFromCommand TestCommand = TestTag

  tagFromJobId (TestJobId _) = TestTag

  tagEq TestTag TestTag = Just (Refl, Refl, Refl)

  putTag TestTag = putWord8 0x01
  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> pure $ Job.SomeTag TestTag
      _    -> fail $ "Invalid command tag: " <> show tag
  putJobId (TestJobId jid) = put jid
  getJobId TestTag = TestJobId <$> get

  putCommand TestCommand = putWord8 0x01
  getCommand TestTag = do
    tag <- getWord8
    case tag of
      0x01 -> pure TestCommand
      _    -> fail $ "Invalid command value: " <> show tag

  putStatus = \case
    TestTag -> put

  getStatus = \case
    TestTag -> get

  putErr = \case
    TestTag -> put

  getErr = \case
    TestTag -> get

  putResult = \case
    TestTag -> put

  getResult = \case
    TestTag -> get

instance Eq (AnyMessage (Job.Job TestCommand)) where
  (AnyMessage (MsgExec TestCommand)) == (AnyMessage (MsgExec TestCommand))  = True
  (AnyMessage (MsgAttach jId1)) == (AnyMessage (MsgAttach jId2)) = case (jId1, jId2) of
    (TestJobId i1, TestJobId i2) -> i1 == i2
  (AnyMessage MsgAttached) == (AnyMessage MsgAttached) = True
  (AnyMessage MsgAttachFailed) == (AnyMessage MsgAttachFailed) = True
  (AnyMessage (MsgFail _)) == (AnyMessage (MsgFail _)) = True
  (AnyMessage (MsgSucceed _)) == (AnyMessage (MsgSucceed _)) = True
  (AnyMessage (MsgAwait _ _)) == (AnyMessage (MsgAwait _ _)) = True
  (AnyMessage MsgPoll) == (AnyMessage MsgPoll) = True
  (AnyMessage MsgDetach) == (AnyMessage MsgDetach) = True
  _ == _ = False

spec :: Spec
spec = do
  it "MsgExec serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) 'StInit ('StCmd () () ())
      msg = MsgExec TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ClientAgency TokInit) msg) `shouldBe` True
  prop "MsgAttach serialization roundtrip" $ do
    jid <- arbitrary
    let
      msg :: Message (Job TestCommand) 'StInit ('StAttach () () ())
      msg = MsgAttach (TestJobId jid)
    prop_codecM codecJob (AnyMessageAndAgency (ClientAgency TokInit) msg)
  it "MsgAttached serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StAttach () () ()) ('StCmd () () ())
      msg = MsgAttached
      tag = tagFromJobId (TestJobId 8)
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ServerAgency (TokAttach tag)) msg) `shouldBe` True
  it "MsgAttachFail serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StAttach () () ()) 'StDone
      msg = MsgAttachFailed
      tag = tagFromJobId (TestJobId 8)
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ServerAgency (TokAttach tag)) msg) `shouldBe` True
  it "MsgFail serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StCmd () () ()) 'StDone
      msg = MsgFail ()
      cmdTag = tagFromCommand TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ServerAgency (TokCmd cmdTag)) msg) `shouldBe` True
  it "MsgSucceed serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StCmd () () ()) 'StDone
      msg = MsgSucceed ()
      cmdTag = tagFromCommand TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ServerAgency (TokCmd cmdTag)) msg) `shouldBe` True
  it "MsgSucceed serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StCmd () () ()) 'StDone
      msg = MsgSucceed ()
      cmdTag = tagFromCommand TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ServerAgency (TokCmd cmdTag)) msg) `shouldBe` True
  it "MsgAwait serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StCmd () () ()) ('StAwait () () ())
      msg = MsgAwait () (TestJobId 8)
      cmdTag = tagFromCommand TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ServerAgency (TokCmd cmdTag)) msg) `shouldBe` True
  it "MsgPoll serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StAwait () () ()) ('StCmd () () ())
      msg = MsgPoll
      cmdTag = tagFromCommand TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ClientAgency (TokAwait cmdTag)) msg) `shouldBe` True
  it "MsgDetach serialization roundtrip" $ do
    let
      msg :: Message (Job TestCommand) ('StAwait () () ()) 'StDone
      msg = MsgDetach
      cmdTag = tagFromCommand TestCommand
    prop_codec runIdentity codecJob (AnyMessageAndAgency (ClientAgency (TokAwait cmdTag)) msg) `shouldBe` True

