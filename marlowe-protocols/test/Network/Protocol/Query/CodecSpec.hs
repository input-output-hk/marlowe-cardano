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
  (AnyMessage (MsgRequest TestQuery)) == (AnyMessage (MsgRequest TestQuery))  = True
  (AnyMessage (MsgReject _)) == (AnyMessage (MsgReject _))  = True
  (AnyMessage (MsgNextPage _ _)) == (AnyMessage (MsgNextPage _ _))  = True
  (AnyMessage (MsgRequestNext _)) == (AnyMessage (MsgRequestNext _))  = True
  _ == _ = False

spec :: Spec
spec = do
  it "MsgRequest serialization roundtrip" $ do
    let
      msg :: Message (Query TestQuery) 'StInit ('StNext 'CanReject () () ())
      msg = MsgRequest TestQuery
    prop_codec runIdentity codecQuery (AnyMessageAndAgency (ClientAgency TokInit) msg) `shouldBe` True
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

