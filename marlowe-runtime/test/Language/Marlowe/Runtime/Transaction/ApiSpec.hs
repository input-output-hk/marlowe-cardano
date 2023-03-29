{-# LANGUAGE OverloadedLists #-}

module Language.Marlowe.Runtime.Transaction.ApiSpec
  ( spec
  ) where

import Control.Applicative (liftA2)
import Control.Arrow ((&&&), (***))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.ByteString.Char8 as BS
import Data.Coerce (coerce)
import Data.Foldable (find)
import Data.Maybe (maybeToList)
import qualified Data.Maybe as Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Vector as Vector
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId(PolicyId), TokenName(TokenName))
import Language.Marlowe.Runtime.Transaction.Api
  (NFTMetadataFile(..), RoleTokenMetadata(..), decodeRoleTokenMetadata, encodeRoleTokenMetadata)
import Language.Marlowe.Runtime.Transaction.Gen ()
import Network.HTTP.Media (MediaType, (//))
import qualified Network.URI as Network (URI(..), URIAuth(..))
import qualified Network.URI hiding (URI(..), URIAuth(..))
import Test.Hspec (Spec, shouldBe, shouldSatisfy)
import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen)
import qualified Test.QuickCheck as Gen

spec :: Spec
spec =
  Hspec.describe "CIP-25 Metadata" do
    prop "uriGen is valid" uriGenValidityTests
    prop "NFTMetadataFile is valid" cip25MetadataFileDetailsValidityTests
    prop "NFTMetadataFile is sound" cip25MetadataFileDetailsSoundnessTests
    prop "RoleTokenMetadata JSON roundtrip" roleTokenMetadataJSONRoundtrip
    prop "RoleTokenMetadata Metadata roundtrip" roleTokenMetadataMetadataRoundtrip
    prop "MediaType JSON roundtrip" mediaTypeJSONInstancesTests

cip25MetadataFileDetailsSoundnessTests :: Gen.Property
cip25MetadataFileDetailsSoundnessTests =
  -- FIXME: Improve notNFTMetadataFileGen quality:
  Gen.forAll notNFTMetadataFileGen \json -> do
    let document = Aeson.encode json
    Aeson.decode @NFTMetadataFile document `shouldBe` Nothing
  where
  notNFTMetadataFileGen :: Gen Aeson.Value
  notNFTMetadataFileGen =
    Gen.arbitrary `Gen.suchThat` (not . isNFTMetadataFileJSON)

  isNFTMetadataFileJSON :: Aeson.Value -> Bool
  isNFTMetadataFileJSON = \case
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

roleTokenMetadataJSONRoundtrip :: Gen.Property
roleTokenMetadataJSONRoundtrip = Gen.checkCoverage $
  Gen.forAll cip25MetadataDetailsJSONRelationGen \(metadata@RoleTokenMetadata{..}, json) ->
    Gen.cover 20.0 (Maybe.isNothing description) "has no description" $
    Gen.cover 20.0 (maybe False Text.null description) "has empty description" $
    Gen.cover 20.0 (maybe False (not . Text.null) description) "has not empty description" $
    Gen.cover 20.0 (Maybe.isNothing mediaType) "has no mediaType" $
    Gen.cover 30.0 (Text.null name) "has empty name" $
    Gen.cover 30.0 (not $ Text.null name) "has name" do
    let document = Aeson.encode json
    Aeson.encode metadata `shouldBe` document
    Aeson.decode @RoleTokenMetadata document `shouldBe` Just metadata

roleTokenMetadataMetadataRoundtrip :: Gen.Property
roleTokenMetadataMetadataRoundtrip = Gen.checkCoverage \metadata@RoleTokenMetadata{..} ->
    Gen.cover 20.0 (Maybe.isNothing description) "has no description" $
    Gen.cover 20.0 (maybe False Text.null description) "has empty description" $
    Gen.cover 20.0 (maybe False (not . Text.null) description) "has not empty description" $
    Gen.cover 20.0 (Maybe.isNothing mediaType) "has no mediaType" $
    Gen.cover 30.0 (Text.null name) "has empty name" $
    Gen.cover 30.0 (not $ Text.null name) "has name" do
    decodeRoleTokenMetadata (encodeRoleTokenMetadata metadata) `shouldBe` Just metadata

mediaTypeJSONInstancesTests :: Gen.Property
mediaTypeJSONInstancesTests =
  Gen.forAll mediaTypeJSONRelationGen \(mediaType, json) -> do
    let document = Aeson.encode json
    Aeson.encode mediaType `shouldBe` document
    fmap show (Aeson.decode @MediaType document) `shouldBe` Just (show mediaType)

mediaTypeJSONRelationGen :: Gen (MediaType, Aeson.Value)
mediaTypeJSONRelationGen = do
  let stringGen = do
        n <- Gen.chooseInt (1, 127)
        BS.pack <$> Gen.vectorOf n (Gen.elements ['a' .. 'z'])
  mediaType <- liftA2 (//) stringGen stringGen
  pure (mediaType, Aeson.String $ Text.pack $ show mediaType)

cip25MetadataDetailsJSONRelationGen :: Gen (RoleTokenMetadata, Aeson.Value)
cip25MetadataDetailsJSONRelationGen = do
  name <- Gen.oneof [pure "", fromString <$> Gen.listOf1 Gen.arbitrary]
  (image, imageJSON) <- uriJSONRelationGen
  (mediaType, mediaTypeJSON) <- Gen.oneof [pure (Nothing, Nothing), (Just *** Just) <$> mediaTypeJSONRelationGen]
  description <- Gen.oneof [pure Nothing, pure (Just ""), Just . fromString <$> Gen.listOf1 Gen.arbitrary]
  (files, filesJSON) <- cip25MetadataFileDetailsJSONRelationGen
  let json = Aeson.Object $ Aeson.KeyMap.fromList $
        [ ("name", Aeson.String name)
        , ("image", imageJSON)
        ]
        <> maybeToList (("mediaType",) <$> mediaTypeJSON)
        <> maybeToList (("description",) . Aeson.String <$> description)
        <> [("files", filesJSON) | not (null files)]
  pure (RoleTokenMetadata {..}, json)

cip25MetadataFileDetailsValidityTests :: Gen.Property
cip25MetadataFileDetailsValidityTests = Gen.checkCoverage $
  Gen.forAll cip25MetadataFileDetailsJSONRelationGen \(fds, json) ->
    Gen.cover 15.0 (Maybe.isJust $ find (\NFTMetadataFile{..} -> Text.null name) fds) "some file details has empty name" $
    Gen.cover 15.0 (Maybe.isJust $ find (\NFTMetadataFile{..} -> not $ Text.null name) fds) "some file details has name" $
    Gen.cover 1.0 (null fds) "no file details" $
    Gen.cover 80.0 (not $ null fds) "some file details" do
    let document = Aeson.encode json
    Aeson.encode fds `shouldBe` document
    fmap show (Aeson.decode @[NFTMetadataFile] document) `shouldBe` Just (show fds)

cip25MetadataFileDetailsJSONRelationGen :: Gen ([NFTMetadataFile], Aeson.Value)
cip25MetadataFileDetailsJSONRelationGen = do
  let emptyGen = pure []
      nonEmptyGen = do
        n <- Gen.chooseInt (1, 10)
        Gen.vectorOf n do
          name <- Gen.oneof [pure "" ,fromString <$> Gen.listOf1 Gen.arbitrary]
          (mediaType, mediaTypeJSON) <- mediaTypeJSONRelationGen
          (src, srcJSON) <- uriJSONRelationGen
          let json = Aeson.Object
                [ ("name", Aeson.String name)
                , ("mediaType", mediaTypeJSON)
                , ("src", srcJSON)
                ]
          pure (NFTMetadataFile {..}, json)
  (cip25MetadataFileDetails, json) <- unzip <$> Gen.frequency [(1, emptyGen), (9, nonEmptyGen)]
  pure (cip25MetadataFileDetails, Aeson.Array $ Vector.fromList json)

uriJSONRelationGen :: Gen (Network.URI, Aeson.Value)
uriJSONRelationGen = (id &&& Aeson.String . fromString . show) <$> uriGen

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
  specialCharGen = Gen.oneof [charLetterGen, charNumberGen, Gen.elements ['.', '-', '_', '=', ';']]

  charLetterGen :: Gen Char
  charLetterGen = Gen.elements $ ['a' .. 'z'] <> ['A' .. 'Z']

  charNumberGen :: Gen Char
  charNumberGen = Gen.elements ['0' .. '9']
