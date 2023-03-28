{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.ApiSpec
  ( spec
  ) where

import Language.Marlowe.Runtime.Transaction.Api (NFTMetadata(..), NFTMetadataDetails(..), NFTMetadataFileDetails(..))

import Control.Arrow ((&&&), (***))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Encoding.Base16 as Base16
import qualified Data.Vector as Vector
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId(PolicyId), TokenName(TokenName))
import qualified Network.URI as Network (URI(..), URIAuth(..))
import qualified Network.URI hiding (URI(..), URIAuth(..))
import Test.Hspec (Spec, shouldBe, shouldSatisfy)
import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

spec :: Spec
spec = do
  Hspec.describe "CIP-25 Metadata" do
    prop "uriGen is valid" uriGenValidityTests
    prop "NFTMetadataFileDetails is valid" cip25MetadataFileDetailsValidityTests
    prop "NFTMetadataFileDetails is sound" cip25MetadataFileDetailsSoundnessTests
    prop "NFTMetadataDetails is valid" cip25MetadataDetailsValidityTests
    prop "NFTMetadata is valid" cip25MetadataValidityTests

cip25MetadataFileDetailsSoundnessTests :: Gen.Property
cip25MetadataFileDetailsSoundnessTests =
  -- FIXME: Improve notNFTMetadataFileDetailsGen quality:
  Gen.forAll notNFTMetadataFileDetailsGen \json -> do
    let document = Aeson.encode json
    Aeson.decode @NFTMetadataFileDetails document `shouldBe` Nothing
  where
  notNFTMetadataFileDetailsGen :: Gen Aeson.Value
  notNFTMetadataFileDetailsGen =
    Gen.arbitrary `Gen.suchThat` (not . isNFTMetadataFileDetailsJSON)

  isNFTMetadataFileDetailsJSON :: Aeson.Value -> Bool
  isNFTMetadataFileDetailsJSON = \case
    Aeson.Object x ->
      and @[]
        [ maybe False isJSONString $ Aeson.KeyMap.lookup "name" x
        , maybe False isJSONString $ Aeson.KeyMap.lookup "mediaType" x
        , maybe False isJSONURI $ Aeson.KeyMap.lookup "src" x
        ]
    _ -> False

isJSONString :: Aeson.Value -> Bool
isJSONString = \case Aeson.String _ -> True; _ -> False

isJSONURI :: Aeson.Value -> Bool
isJSONURI = \case Aeson.String uri -> Network.URI.isURI $ Text.unpack uri; _ -> False

mkPolicyId :: Text -> PolicyId
mkPolicyId = coerce Text.Encoding.encodeUtf8

mkTokenName :: Text -> TokenName
mkTokenName = coerce Text.Encoding.encodeUtf8

base16EncodedTextGen :: Int -> Gen Text
base16EncodedTextGen n = fromString <$> Gen.vectorOf n (Gen.elements $ ['0' .. '9'] <> ['a' .. 'f'])

cip25MetadataValidityTests :: Gen.Property
cip25MetadataValidityTests = Gen.checkCoverage $
  Gen.forAll cip25MetadataJSONRelationGen \(nftMetadata@(NFTMetadata policies), json) ->
    Gen.cover 1.0 (Maybe.isJust $ find null policies) "some policy with no token" $
    Gen.cover 40.0 (Maybe.isJust $ find (not . null) policies) "some policy with some token" $
    Gen.cover 40.0 (not $ null policies) "some policies" $
    Gen.cover 1.0 (null policies) "no policies" do
    let document = Aeson.encode json
    Aeson.encode nftMetadata `shouldBe` document
    fmap show (Aeson.decode @NFTMetadata document) `shouldBe` Just (show nftMetadata)
    nftMetadata' `shouldBe` Just nftMetadata
  where
  cip25MetadataJSONRelationGen :: Gen (NFTMetadata, Aeson.Value)
  cip25MetadataJSONRelationGen = do
    let emptyGen = pure []
        nonemptyGen = do
          n <- Gen.chooseInt (1, 8)
          Gen.vectorOf n do
            (policyId, policyIdJSON) <- policyIdJSONKeyRelationGen
            (tokenMetadata, tokenMetadataJSON) <- tokenMetadataJSONRelationGen
            pure ((policyId, tokenMetadata), (policyIdJSON, tokenMetadataJSON))
    (Map.fromList -> metadata, Aeson.Object . Aeson.KeyMap.fromList -> json) <-
      unzip <$> Gen.frequency [(1, emptyGen), (4, nonemptyGen)]
    pure (NFTMetadata metadata, Aeson.Object [("721", json)])

  tokenMetadataJSONRelationGen :: Gen (Map TokenName NFTMetadataDetails, Aeson.Value)
  tokenMetadataJSONRelationGen = do
    let emptyGen = pure []
        nonEmptyGen = do
          n <- Gen.chooseInt (1, 8)
          Gen.vectorOf n do
            (tokenName, tokenNameJSON) <- tokenNameJSONKeyRelationGen
            (metadataDetails, metadataDetailsJSON) <- cip25MetadataDetailsJSONRelationGen
            pure ((tokenName, metadataDetails), (tokenNameJSON, metadataDetailsJSON))
    (Map.fromList *** Aeson.Object . Aeson.KeyMap.fromList) .
      unzip <$> Gen.frequency [(1, emptyGen),(4, nonEmptyGen)]

  policyIdJSONKeyRelationGen :: Gen (PolicyId, Aeson.Key)
  policyIdJSONKeyRelationGen =
    (mkPolicyId &&& Aeson.Key.fromText . Base16.encodeBase16)
      . fromString <$> Gen.arbitrary

  tokenNameJSONKeyRelationGen :: Gen (TokenName, Aeson.Key)
  tokenNameJSONKeyRelationGen = do
    ((*2) -> n) <- Gen.chooseInt (0, 32)
    (mkTokenName &&& Aeson.Key.fromText)
      <$> base16EncodedTextGen n

cip25MetadataDetailsValidityTests :: Gen.Property
cip25MetadataDetailsValidityTests = Gen.checkCoverage $
  Gen.forAll cip25MetadataDetailsJSONRelationGen \(details@NFTMetadataDetails{..}, json) ->
    Gen.cover 20.0 (Maybe.isNothing description) "has no description" $
    Gen.cover 20.0 (maybe False Text.null description) "has empty description" $
    Gen.cover 20.0 (maybe False (not . Text.null) description) "has not empty description" $
    Gen.cover 20.0 (Maybe.isNothing mediaType) "has no mediaType" $
    Gen.cover 20.0 (maybe False Text.null mediaType) "has empty mediaType" $
    Gen.cover 20.0 (maybe False (not . Text.null) mediaType) "has not empty mediaType" $
    Gen.cover 30.0 (Text.null name) "has empty name" $
    Gen.cover 30.0 (not $ Text.null name) "has name" do
    let document = Aeson.encode json
    Aeson.encode details `shouldBe` document
    fmap show (Aeson.decode @NFTMetadataDetails document) `shouldBe` Just (show details)

cip25MetadataDetailsJSONRelationGen :: Gen (NFTMetadataDetails, Aeson.Value)
cip25MetadataDetailsJSONRelationGen = do
  name <- Gen.oneof [pure "", fromString <$> Gen.listOf1 Gen.arbitrary]
  (image, imageJSON) <- uriJSONRelationGen
  mediaType <- Gen.oneof [pure Nothing, pure (Just ""), Just . fromString <$> Gen.listOf1 Gen.arbitrary]
  description <- Gen.oneof [pure Nothing, pure (Just ""), Just . fromString <$> Gen.listOf1 Gen.arbitrary]
  (files, filesJSON) <- cip25MetadataFileDetailsJSONRelationGen
  let json = Aeson.Object $ Aeson.KeyMap.fromList $
        [ ("name", Aeson.String name)
        , ("image", imageJSON)
        ]
        <> maybeToList (("mediaType",) . Aeson.String <$> mediaType)
        <> maybeToList (("description",) . Aeson.String <$> description)
        <> [("files", filesJSON) | not (null files)]
  pure (NFTMetadataDetails {..}, json)

cip25MetadataFileDetailsValidityTests :: Gen.Property
cip25MetadataFileDetailsValidityTests = Gen.checkCoverage $
  Gen.forAll cip25MetadataFileDetailsJSONRelationGen \(fds, json) ->
    Gen.cover 15.0 (Maybe.isJust $ find (\NFTMetadataFileDetails{..} -> Text.null name) fds) "some file details has empty name" $
    Gen.cover 15.0 (Maybe.isJust $ find (\NFTMetadataFileDetails{..} -> not $ Text.null name) fds) "some file details has name" $
    Gen.cover 15.0 (Maybe.isJust $ find (\NFTMetadataFileDetails{..} -> Text.null mediaType) fds) "some file details has empty mediaType" $
    Gen.cover 15.0 (Maybe.isJust $ find (\NFTMetadataFileDetails{..} -> not $ Text.null mediaType) fds) "some file details has mediaType" $
    Gen.cover 1.0 (null fds) "no file details" $
    Gen.cover 80.0 (not $ null fds) "some file details" do
    let document = Aeson.encode json
    Aeson.encode fds `shouldBe` document
    fmap show (Aeson.decode @[NFTMetadataFileDetails] document) `shouldBe` Just (show fds)

cip25MetadataFileDetailsJSONRelationGen :: Gen ([NFTMetadataFileDetails], Aeson.Value)
cip25MetadataFileDetailsJSONRelationGen = do
  let emptyGen = pure []
      nonEmptyGen = do
        n <- Gen.chooseInt (1, 10)
        Gen.vectorOf n do
          name <- Gen.oneof [pure "" ,fromString <$> Gen.listOf1 Gen.arbitrary]
          mediaType <- Gen.oneof [pure "" ,fromString <$> Gen.listOf1 Gen.arbitrary]
          (src, srcJSON) <- uriJSONRelationGen
          let json = Aeson.Object
                [ ("name", Aeson.String name)
                , ("mediaType", Aeson.String mediaType)
                , ("src", srcJSON)
                ]
          pure (NFTMetadataFileDetails {..}, json)
  (cip25MetadataFileDetails, json) <- unzip <$> Gen.frequency [(1, emptyGen), (9, nonEmptyGen)]
  pure (cip25MetadataFileDetails, Aeson.Array $ Vector.fromList json)

uriJSONRelationGen :: Gen (Text, Aeson.Value)
uriJSONRelationGen = (id &&& Aeson.String) . fromString . show <$> uriGen

uriGenValidityTests :: Gen.Property
uriGenValidityTests = Gen.checkCoverage $
  Gen.forAll uriGen \uri@Network.URI {..} ->
    Gen.cover 30.0 (Maybe.isJust uriAuthority) "has uriAuthority" $
    Gen.cover 30.0 (Maybe.isNothing uriAuthority) "hasn't uriAuthority" $
    Gen.cover 15.0 (maybe False (null . Network.uriUserInfo) uriAuthority) "has uriAuthority and it hasn't uriUserInfo" $
    Gen.cover 15.0 (maybe False (not . null . Network.uriUserInfo) uriAuthority) "has uriAuthority and it has uriUserInfo" $
    Gen.cover 15.0 (maybe False (null . Network.uriRegName) uriAuthority) "has uriAuthority and it hasn't uriRegName" $
    Gen.cover 15.0 (maybe False (not . null . Network.uriRegName) uriAuthority) "has uriAuthority and it has uriRegName" $
    Gen.cover 15.0 (maybe False (null . Network.uriRegName) uriAuthority) "has uriAuthority and it hasn't uriRegName" $
    Gen.cover 15.0 (maybe False (null . Network.uriPort) uriAuthority) "has uriAuthority and it hasn't uriPort" $
    Gen.cover 15.0 (maybe False (not . null . Network.uriPort) uriAuthority) "has uriAuthority and it has uriPort" $
    Gen.cover 30.0 (not $ null uriQuery) "has uriQuery" $
    Gen.cover 30.0 (null uriQuery) "hasn't uriQuery" $
    Gen.cover 30.0 (not $ null uriFragment) "has uriFragment" $
    Gen.cover 30.0 (null uriFragment) "hasn't uriFragment" $
    Gen.cover 30.0 (not $ null uriPath) "has uriPath" $
    Gen.cover 30.0 (null uriPath) "hasn't uriPath" $
    Gen.cover 30.0 (length uriScheme > 2) "has long uriScheme" $
    Gen.cover 30.0 (length uriScheme <= 2) "has short uriScheme" do
    uri `shouldSatisfy` Network.URI.isURI . show

uriGen :: Gen Network.URI
uriGen = do
  uriScheme <- do
    c <- charLetterGen
    fmap (c:) $ Gen.oneof [pure "", Gen.listOf $ Gen.oneof [charLetterGen, charNumberGen]]

  uriAuthority <-
    Gen.oneof
      [ pure Nothing
      , do
          uriUserInfo <- Gen.oneof [pure "", Gen.listOf specialCharGen]
          uriRegName <- Gen.oneof [pure "", Gen.listOf specialCharGen]
          uriPort <- Gen.oneof [pure "", Gen.listOf charNumberGen]
          pure $ Just $ Network.URIAuth {..}
      ]

  uriPath <- Gen.oneof [pure "", ('/':) <$> Gen.listOf specialCharGen]
  uriQuery <- Gen.oneof [pure "", Gen.listOf1 specialCharGen]
  uriFragment <- Gen.oneof [pure "", Gen.listOf1 specialCharGen]

  pure $ Network.URI.rectify $ Network.URI {..}

  where
  specialCharGen :: Gen Char
  specialCharGen = Gen.oneof [charLetterGen, charNumberGen, Gen.elements ['.', '/', '-', '_', '?', '=', ';']]

  charLetterGen :: Gen Char
  charLetterGen = Gen.elements $ ['a' .. 'z'] <> ['A' .. 'Z']

  charNumberGen :: Gen Char
  charNumberGen = Gen.elements ['0' .. '9']
