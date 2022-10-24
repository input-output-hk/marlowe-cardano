{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Protocol.Query.CodecSpec
  ( spec
  ) where

import Data.Binary (Binary(get), getWord8, put, putWord8)
import Data.Data (type (:~:)(Refl))
import Data.Functor.Identity (runIdentity)
import Network.Protocol.Arbitrary (genSchemaVersion)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Types
  ( ClientHasAgency(..)
  , IsQuery(tagFromQuery)
  , Message(..)
  , Query(..)
  , ServerHasAgency(..)
  , StNextKind(..)
  , TokNextKind(..)
  )
import qualified Network.Protocol.Query.Types as Query
import Network.Protocol.SchemaVersion (SchemaVersion)
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

data TestQuery delimiter err result where
  TestQuery :: TestQuery () () ()

instance IsQuery TestQuery where
  data Tag TestQuery delimiter err result where
    TestTag :: Query.Tag TestQuery () () ()

  tagFromQuery = \case
    TestQuery -> TestTag
  tagEq TestTag TestTag = Just (Refl, Refl, Refl)

  putTag = \case
    TestTag -> putWord8 0x01
  getTag = getWord8 >>= \case
    0x01 -> pure $ Query.SomeTag TestTag
    tag -> fail $ "Invalid query tag: " <> show tag
  putQuery = \case
    TestQuery -> mempty
  getQuery = \case
    TestTag -> pure TestQuery
  putDelimiter = \case
    TestTag -> put
  getDelimiter = \case
    TestTag -> get
  putErr = \case
    TestTag -> put
  getErr = \case
    TestTag -> get
  putResult = \case
    TestTag -> put
  getResult = \case
    TestTag -> get

instance Eq (AnyMessage (Query.Query TestQuery)) where
  (AnyMessage (MsgRequestHandshake sv1)) == (AnyMessage (MsgRequestHandshake sv2))  = sv1 == sv2
  (AnyMessage MsgConfirmHandshake) == (AnyMessage MsgConfirmHandshake)  = True
  (AnyMessage (MsgRejectHandshake sv1)) == (AnyMessage (MsgRejectHandshake sv2))  = sv1 == sv2
  (AnyMessage MsgDone) == (AnyMessage MsgDone) = True
  (AnyMessage (MsgRequest TestQuery)) == (AnyMessage (MsgRequest TestQuery))  = True
  (AnyMessage (MsgReject _)) == (AnyMessage (MsgReject _))  = True
  (AnyMessage (MsgNextPage _ _)) == (AnyMessage (MsgNextPage _ _))  = True
  (AnyMessage (MsgRequestNext _)) == (AnyMessage (MsgRequestNext _))  = True
  (AnyMessage MsgRequestDone) == (AnyMessage MsgRequestDone)  = True
  _ == _ = False

genMsgRequestHandshake :: Gen (Message (Query TestQuery) 'StInit 'StHandshake)
genMsgRequestHandshake =
  MsgRequestHandshake <$> (genSchemaVersion :: Gen (SchemaVersion cmd))

genMsgRejectHandshake :: Gen (Message (Query TestQuery) 'StHandshake 'StFault)
genMsgRejectHandshake =
  MsgRejectHandshake <$> (genSchemaVersion :: Gen (SchemaVersion cmd))

spec :: Spec
spec = do
  prop "MsgRequestHandshake serialization roundtrip" do
    msg <- genMsgRequestHandshake
    prop_codecM codecQuery (AnyMessageAndAgency (ClientAgency TokInit) msg)
  it "MsgConfirmHandshake serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) 'StHandshake 'StIdle
      msg = MsgConfirmHandshake
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ServerAgency TokHandshake) msg) `shouldBe` True
  prop "MsgRejectHandshake serialization roundtrip" do
    msg <- genMsgRejectHandshake
    prop_codecM codecQuery (AnyMessageAndAgency (ServerAgency TokHandshake) msg)
  it "MsgDone serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) 'StIdle 'StDone
      msg = MsgDone
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ClientAgency TokIdle) msg) `shouldBe` True
  it "MsgRequest serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) 'StIdle ('StNext 'CanReject () () ())
      msg = MsgRequest TestQuery
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ClientAgency TokIdle) msg) `shouldBe` True
  it "MsgReject serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) ('StNext 'CanReject () () ()) 'StDone
      msg = MsgReject ()
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ServerAgency (TokNext TokCanReject tag)) msg) `shouldBe` True
  prop "MsgNextPage serialization roundtrip" $ do
    delimiter <- arbitrary
    let
      msg :: Message (Query TestQuery) ('StNext 'CanReject () () ()) ('StPage () () ())
      msg = MsgNextPage () delimiter
      tag = tagFromQuery TestQuery
    prop_codecM codecQuery (AnyMessageAndAgency (ServerAgency (TokNext TokCanReject tag)) msg)
  it "MsgRequestNext serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) ('StPage () () ()) ('StNext 'MustReply () () ())
      msg = MsgRequestNext ()
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ClientAgency (TokPage tag)) msg) `shouldBe` True
  it "MsgRequestDone serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) ('StPage () () ()) 'StDone
      msg = MsgRequestDone
      tag = tagFromQuery TestQuery
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ClientAgency (TokPage tag)) msg) `shouldBe` True

