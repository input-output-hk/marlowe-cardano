{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.Api (
  Account (..),
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
  BurnError (..),
  BurnTx (..),
  BurnTxInEra (..),
  CoinSelectionError (..),
  ConstraintError (..),
  ContractCreated (..),
  ContractCreatedInEra (..),
  CreateBuildupError (..),
  CreateError (..),
  Destination (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  JobId (..),
  LoadHelpersContextError (..),
  LoadMarloweContextError (..),
  MarloweTxCommand (..),
  Mint (..),
  MintRole (..),
  NFTMetadataFile (..),
  RoleToken (..),
  RoleTokenFilter (
    ..,
    RoleTokensOr,
    RoleTokensAnd,
    RoleTokenFilterByContracts,
    RoleTokenFilterByPolicyIds,
    RoleTokenFilterByTokens
  ),
  RoleTokenMetadata (..),
  RoleTokensConfig (RoleTokensNone, RoleTokensUsePolicy, RoleTokensMint),
  SubmitError (..),
  SubmitStatus (..),
  Tag (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
  decodeRoleTokenMetadata,
  encodeRoleTokenMetadata,
  evalRoleTokenFilter,
  filterRoleTokens,
  getTokenQuantities,
  hasRecipient,
  mkMint,
  optimizeRoleTokenFilter,
) where

import Cardano.Api (
  AnyCardanoEra (..),
  AsType (..),
  BabbageEra,
  BabbageEraOnwards (..),
  ConwayEra,
  IsShelleyBasedEra,
  Tx,
  TxBody,
  TxBodyContent (..),
  cardanoEra,
  createAndValidateTransactionBody,
  deserialiseFromCBOR,
  serialiseToCBOR,
  serialiseToTextEnvelope,
 )
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import Control.Applicative ((<|>))
import Data.Aeson (
  ToJSON (..),
  Value (..),
  object,
  (.!=),
  (.:?),
  (.=),
  (<?>),
 )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import Data.Binary (Binary, Get, get, getWord8, put)
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.SatInt (SatInt)
import Data.Semigroup (Semigroup (..))
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Type.Equality (type (:~:) (Refl))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import qualified Language.Marlowe.Analysis.Safety.Types as Safety
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import Language.Marlowe.Runtime.Cardano.Feature (hush)
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  AssetId (..),
  Assets,
  BlockHeader,
  DatumHash,
  Lovelace,
  Metadata (..),
  PlutusScript,
  PolicyId (..),
  ScriptHash,
  SlotNo,
  StakeCredential,
  TokenName (..),
  Tokens,
  TxId,
  TxOutRef,
  parseMetadataList,
  parseMetadataMap,
  parseMetadataText,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript)
import Language.Marlowe.Runtime.History.Api (ExtractCreationError, ExtractMarloweTransactionError)
import Network.HTTP.Media (MediaType)

import Control.Monad (join)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as Aeson
import Data.Bifunctor (Bifunctor (..))
import Data.Binary.Get (label)
import Data.Function (on)
import Data.Key (forWithKey)
import Data.List (nub)
import qualified Data.List.NonEmpty as NE
import Data.Map.NonEmpty (NEMap)
import qualified Data.Map.NonEmpty as NEMap
import Data.Semigroup.Foldable (Foldable1 (foldMap1))
import qualified Data.Set as Set
import Network.Protocol.Codec.Spec (Variations (..), varyAp)
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Job.Types
import qualified Network.URI as Network
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU, ExMemory)
import qualified PlutusLedgerApi.V2 as PV2

instance Binary Network.URIAuth
instance Binary Network.URI

instance Variations Network.URIAuth
instance Variations Network.URI

instance Binary MediaType where
  put = put . show
  get = fromString <$> get

instance Variations MediaType where
  variations = pure "text/plain"

instance Aeson.ToJSON MediaType where
  toJSON = Aeson.String . Text.pack . show

instance Aeson.FromJSON MediaType where
  parseJSON = fmap fromString . Aeson.parseJSON

data NFTMetadataFile = NFTMetadataFile
  { name :: Text
  , mediaType :: MediaType
  , src :: Network.URI
  , additionalProperties :: Map Text Metadata
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Binary, Variations)

instance Aeson.ToJSON NFTMetadataFile where
  toJSON NFTMetadataFile{..} =
    Aeson.object
      [ ("name", toJSON name)
      , ("mediaType", toJSON mediaType)
      , ("src", toJSON $ show src)
      ]

parseJsonUri :: Text -> Aeson.Types.Parser Network.URI
parseJsonUri (Text.unpack -> s) =
  maybe (fail $ s <> " is not a valid URI!") pure $ Network.parseURI s

instance Aeson.FromJSON NFTMetadataFile where
  parseJSON = Aeson.withObject "NFTMetadataFile" \x -> do
    let additionalProperties =
          Map.mapKeys Key.toText . Map.withoutKeys (Aeson.toMap x) $
            Set.fromList
              [ "name"
              , "mediaType"
              , "src"
              ]
    NFTMetadataFile
      <$> x .: "name"
      <*> x .: "mediaType"
      <*> (parseJsonUri =<< x .: "src")
      <*> forWithKey additionalProperties \key value -> Aeson.parseJSON value <?> Aeson.Types.Key (Key.fromText key)

data RoleTokenMetadata = RoleTokenMetadata
  { name :: Text
  , image :: Network.URI
  , mediaType :: Maybe MediaType
  , description :: Maybe Text
  , files :: [NFTMetadataFile]
  , additionalProperties :: Map Text Metadata
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Binary, Variations)

instance Aeson.ToJSON RoleTokenMetadata where
  toJSON RoleTokenMetadata{..} =
    Aeson.Object $
      Aeson.KeyMap.fromList $
        [ ("name", toJSON name)
        , ("image", toJSON $ show image)
        ]
          <> maybeToList (fmap (("mediaType",) . toJSON) mediaType)
          <> maybeToList (fmap (("description",) . Aeson.String) description)
          <> case files of
            [] -> []
            _ -> [("files", toJSON files)]
          <> Map.toList (Map.mapKeys Key.fromText $ toJSON <$> additionalProperties)

instance Aeson.FromJSON RoleTokenMetadata where
  parseJSON = Aeson.withObject "RoleTokenMetadata" \x -> do
    let additionalProperties =
          Map.mapKeys Key.toText . Map.withoutKeys (Aeson.toMap x) $
            Set.fromList
              [ "name"
              , "image"
              , "mediaType"
              , "description"
              , "files"
              ]
    RoleTokenMetadata
      <$> x .: "name"
      <*> (parseJsonUri =<< x .: "image")
      <*> x .:? "mediaType"
      <*> x .:? "description"
      <*> x .:? "files" .!= []
      <*> forWithKey additionalProperties \key value -> Aeson.parseJSON value <?> Aeson.Types.Key (Key.fromText key)

decodeRoleTokenMetadata :: Metadata -> Maybe RoleTokenMetadata
decodeRoleTokenMetadata = parseNFTMetadataDetails
  where
    parseNFTMetadataDetails :: Metadata -> Maybe RoleTokenMetadata
    parseNFTMetadataDetails metadata = do
      textKeyMap <- parseMetadataRecord metadata
      name <- parseMetadataText =<< Map.lookup "name" textKeyMap
      image <- Network.parseURI . Text.unpack =<< parseSplittableText =<< Map.lookup "image" textKeyMap
      let mediaType = parseMediaType =<< Map.lookup "mediaType" textKeyMap
          description = parseSplittableText =<< Map.lookup "description" textKeyMap
          parseSingleFileDetails = fmap (: []) . parseNFTMetadataFile
          parseManyFileDetails = parseMetadataList parseNFTMetadataFile
          parseFileDetails md = parseSingleFileDetails md <|> parseManyFileDetails md
          files = fromMaybe [] $ parseFileDetails =<< Map.lookup "files" textKeyMap
          additionalProperties =
            Map.withoutKeys textKeyMap $
              Set.fromList
                [ "name"
                , "image"
                , "mediaType"
                , "description"
                , "files"
                ]
      Just $ RoleTokenMetadata{..}

    parseNFTMetadataFile :: Metadata -> Maybe NFTMetadataFile
    parseNFTMetadataFile metadata = do
      textKeyMap <- parseMetadataRecord metadata
      name <- parseMetadataText =<< Map.lookup "name" textKeyMap
      mediaType <- parseMediaType =<< Map.lookup "mediaType" textKeyMap
      src <- Network.parseURI . Text.unpack =<< parseSplittableText =<< Map.lookup "src" textKeyMap
      let additionalProperties = Map.withoutKeys textKeyMap $ Set.fromList ["name", "mediaType", "src"]
      Just $ NFTMetadataFile{..}

    parseMediaType :: Metadata -> Maybe MediaType
    parseMediaType = fmap (fromString . Text.unpack) . parseMetadataText

    parseMetadataRecord :: Metadata -> Maybe (Map Text Metadata)
    parseMetadataRecord = parseMetadataMap parseMetadataText Just

    parseSplittableText :: Metadata -> Maybe Text
    parseSplittableText md = parseMetadataText md <|> mconcat <$> parseMetadataList parseMetadataText md

encodeRoleTokenMetadata :: RoleTokenMetadata -> Metadata
encodeRoleTokenMetadata = encodeNFTMetadataDetails
  where
    encodeNFTMetadataDetails :: RoleTokenMetadata -> Metadata
    encodeNFTMetadataDetails RoleTokenMetadata{..} =
      MetadataMap $
        [ (MetadataText "name", MetadataText name)
        , (MetadataText "image", encodeText $ fromString $ Network.uriToString id image "")
        ]
          <> maybeToList ((MetadataText "mediaType",) . encodeMediaType <$> mediaType)
          <> maybeToList ((MetadataText "description",) . encodeText <$> description)
          <> case files of
            [] -> []
            [fileDetails] ->
              [(MetadataText "files", encodeNFTMetadataFile fileDetails)]
            fileDetails ->
              [(MetadataText "files", MetadataList $ fmap encodeNFTMetadataFile fileDetails)]
          <> fmap (first MetadataText) (Map.toList additionalProperties)

    encodeNFTMetadataFile :: NFTMetadataFile -> Metadata
    encodeNFTMetadataFile NFTMetadataFile{..} =
      MetadataMap $
        [ (MetadataText "name", MetadataText name)
        , (MetadataText "mediaType", encodeMediaType mediaType)
        , (MetadataText "src", encodeText $ fromString $ Network.uriToString id src "")
        ]
          <> fmap (first MetadataText) (Map.toList additionalProperties)

    encodeMediaType :: MediaType -> Metadata
    encodeMediaType = MetadataText . Text.pack . show

    encodeText :: Text -> Metadata
    encodeText = MetadataList . fmap MetadataText . Text.chunksOf 64

data Destination
  = ToAddress Address
  | ToScript HelperScript
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data MintRole = MintRole
  { roleMetadata :: Maybe RoleTokenMetadata
  , roleTokenRecipients :: NEMap Destination Chain.Quantity
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Variations)

instance (Binary k, Binary v) => Binary (NEMap k v) where
  put = put . NEMap.toMap
  get = maybe (fail "Unexpected empty map") pure . NEMap.nonEmptyMap =<< get

instance (Variations k, Variations v) => Variations (NEMap k v) where
  variations = NEMap.singleton <$> variations `varyAp` variations

instance Semigroup MintRole where
  a <> b =
    MintRole
      { roleMetadata = on (<|>) roleMetadata a b
      , roleTokenRecipients = on (NEMap.unionWith (+)) roleTokenRecipients a b
      }

getTokenQuantities :: Mint -> NEMap TokenName Chain.Quantity
getTokenQuantities = fmap (sum . roleTokenRecipients) . unMint

hasRecipient :: Destination -> Mint -> Bool
hasRecipient destination = any (NEMap.member destination . roleTokenRecipients) . unMint

-- | Non empty mint request.
newtype Mint = Mint {unMint :: NEMap TokenName MintRole}
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Variations)

instance Semigroup Mint where
  a <> b =
    Mint
      { unMint = on (NEMap.unionWith (<>)) unMint a b
      }

mkMint :: NonEmpty (TokenName, Maybe RoleTokenMetadata, Destination, Chain.Quantity) -> Mint
mkMint = foldMap1 \(token, metadata, dest, quantity) ->
  Mint $ NEMap.singleton token $ MintRole metadata $ NEMap.singleton dest quantity

data RoleTokensConfig
  = RoleTokensNone
  | UnsafeRoleTokensUsePolicy PolicyId (Map TokenName (Map Destination Chain.Quantity))
  | UnsafeRoleTokensMint Mint
  deriving stock (Show, Eq, Ord)

instance Binary RoleTokensConfig where
  put RoleTokensNone = putWord8 0
  put (RoleTokensUsePolicy policy dist) = do
    putWord8 1
    put policy
    put dist
  put (RoleTokensMint mint) = do
    putWord8 2
    put mint
  get = label "RoleTokensConfig" do
    ctorIx <- getWord8
    case ctorIx of
      0 -> pure RoleTokensNone
      1 -> label "RoleTokensUsePolicy" $ RoleTokensUsePolicy <$> get <*> get
      2 -> label "RoleTokensMint" $ RoleTokensMint <$> get
      _ -> fail $ "Unknown constructor index " <> show ctorIx

instance Variations RoleTokensConfig where
  variations =
    join $
      NE.fromList $
        nub
          [ pure RoleTokensNone
          , RoleTokensUsePolicy <$> variations `varyAp` variations
          , RoleTokensMint <$> variations
          ]

{-# COMPLETE RoleTokensNone, RoleTokensUsePolicy, RoleTokensMint #-}

pattern RoleTokensUsePolicy
  :: PolicyId
  -> Map TokenName (Map Destination Chain.Quantity)
  -> RoleTokensConfig
pattern RoleTokensUsePolicy policy dist <- UnsafeRoleTokensUsePolicy policy dist
  where
    RoleTokensUsePolicy policy dist =
      UnsafeRoleTokensUsePolicy policy $
        Map.filter (not . Map.null) $
          Map.filter (> 0) <$> dist

pattern RoleTokensMint :: Mint -> RoleTokensConfig
pattern RoleTokensMint mint <- UnsafeRoleTokensMint mint
  where
    RoleTokensMint (Mint mint) =
      maybe RoleTokensNone (UnsafeRoleTokensMint . Mint)
        . NEMap.nonEmptyMap
        . NEMap.mapMaybe
          ( \MintRole{..} -> do
              recipients <- NEMap.nonEmptyMap $ NEMap.filter (> 0) roleTokenRecipients
              pure MintRole{roleTokenRecipients = recipients, ..}
          )
        $ mint

data ContractCreated v where
  ContractCreated
    :: BabbageEraOnwards era -> ContractCreatedInEra era v -> ContractCreated v

instance Variations (ContractCreated 'V1) where
  variations =
    sconcat
      [ ContractCreated BabbageEraOnwardsBabbage <$> variations
      , ContractCreated BabbageEraOnwardsConway <$> variations
      ]

instance Show (ContractCreated 'V1) where
  showsPrec p (ContractCreated BabbageEraOnwardsBabbage created) =
    showParen (p > 10) $
      showString "ContractCreated"
        . showSpace
        . showString "BabbageEraOnwardsBabbage"
        . showsPrec 11 created
  showsPrec p (ContractCreated BabbageEraOnwardsConway created) =
    showParen (p > 10) $
      showString "ContractCreated"
        . showSpace
        . showString "BabbageEraOnwardsConway"
        . showsPrec 11 created

instance Eq (ContractCreated 'V1) where
  ContractCreated BabbageEraOnwardsBabbage a == ContractCreated BabbageEraOnwardsBabbage b =
    a == b
  ContractCreated BabbageEraOnwardsBabbage _ == _ = False
  ContractCreated BabbageEraOnwardsConway a == ContractCreated BabbageEraOnwardsConway b =
    a == b
  ContractCreated BabbageEraOnwardsConway _ == _ = False

instance ToJSON (ContractCreated 'V1) where
  toJSON (ContractCreated BabbageEraOnwardsBabbage created) =
    object
      [ "era" .= String "babbage"
      , "contractCreated" .= created
      ]
  toJSON (ContractCreated BabbageEraOnwardsConway created) =
    object
      [ "era" .= String "conway"
      , "contractCreated" .= created
      ]

instance Binary (ContractCreated 'V1) where
  put (ContractCreated BabbageEraOnwardsBabbage created) = do
    putWord8 0
    put created
  put (ContractCreated BabbageEraOnwardsConway created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> ContractCreated BabbageEraOnwardsBabbage <$> get
      1 -> ContractCreated BabbageEraOnwardsConway <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data ContractCreatedInEra era v = ContractCreatedInEra
  { contractId :: ContractId
  , rolesCurrency :: PolicyId
  , metadata :: MarloweTransactionMetadata
  , marloweScriptHash :: ScriptHash
  , marloweScriptAddress :: Address
  , payoutScriptHash :: ScriptHash
  , payoutScriptAddress :: Address
  , version :: MarloweVersion v
  , datum :: Datum v
  , assets :: Assets
  , txBody :: TxBody era
  , safetyErrors :: [SafetyError]
  }

deriving instance Show (ContractCreatedInEra BabbageEra 'V1)
deriving instance Show (ContractCreatedInEra ConwayEra 'V1)
deriving instance Eq (ContractCreatedInEra BabbageEra 'V1)
deriving instance Eq (ContractCreatedInEra ConwayEra 'V1)

instance (IsShelleyBasedEra era) => Variations (ContractCreatedInEra era 'V1) where
  variations =
    ContractCreatedInEra
      <$> variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` pure MarloweV1
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations

instance (IsShelleyBasedEra era) => ToJSON (ContractCreatedInEra era 'V1) where
  toJSON ContractCreatedInEra{..} =
    object
      [ "contract-id" .= contractId
      , "roles-currency" .= rolesCurrency
      , "metadata" .= metadata
      , "marlowe-script-hash" .= marloweScriptHash
      , "marlowe-script-address" .= marloweScriptAddress
      , "payout-script-hash" .= payoutScriptHash
      , "payout-script-address" .= payoutScriptAddress
      , "datum" .= datum
      , "assets" .= assets
      , "tx-body" .= serialiseToTextEnvelope Nothing txBody
      , "safety-errors" .= safetyErrors
      ]

instance (IsShelleyBasedEra era) => Binary (ContractCreatedInEra era 'V1) where
  put ContractCreatedInEra{..} = do
    put contractId
    put rolesCurrency
    put metadata
    put marloweScriptHash
    put marloweScriptAddress
    put payoutScriptHash
    put payoutScriptAddress
    putDatum MarloweV1 datum
    put assets
    putTxBody txBody
    put safetyErrors
  get = do
    contractId <- get
    rolesCurrency <- get
    metadata <- get
    marloweScriptHash <- get
    marloweScriptAddress <- get
    payoutScriptHash <- get
    payoutScriptAddress <- get
    datum <- getDatum MarloweV1
    assets <- get
    txBody <- getTxBody
    safetyErrors <- get
    let version = MarloweV1
    pure ContractCreatedInEra{..}

data InputsApplied v where
  InputsApplied
    :: BabbageEraOnwards era -> InputsAppliedInEra era v -> InputsApplied v

instance Variations (InputsApplied 'V1) where
  variations = InputsApplied BabbageEraOnwardsBabbage <$> variations

instance Show (InputsApplied 'V1) where
  showsPrec p (InputsApplied BabbageEraOnwardsBabbage created) =
    showParen (p > 10) $
      showString "InputsApplied"
        . showSpace
        . showString "BabbageEraOnwardsBabbage"
        . showsPrec 11 created
  showsPrec p (InputsApplied BabbageEraOnwardsConway created) =
    showParen (p > 10) $
      showString "InputsApplied"
        . showSpace
        . showString "BabbageEraOnwardsConway"
        . showsPrec 11 created

instance Eq (InputsApplied 'V1) where
  InputsApplied BabbageEraOnwardsBabbage a == InputsApplied BabbageEraOnwardsBabbage b =
    a == b
  InputsApplied BabbageEraOnwardsBabbage _ == _ = False
  InputsApplied BabbageEraOnwardsConway a == InputsApplied BabbageEraOnwardsConway b =
    a == b
  InputsApplied BabbageEraOnwardsConway _ == _ = False

instance ToJSON (InputsApplied 'V1) where
  toJSON (InputsApplied BabbageEraOnwardsBabbage created) =
    object
      [ "era" .= String "babbage"
      , "contractCreated" .= created
      ]
  toJSON (InputsApplied BabbageEraOnwardsConway created) =
    object
      [ "era" .= String "conway"
      , "contractCreated" .= created
      ]

instance Binary (InputsApplied 'V1) where
  put (InputsApplied BabbageEraOnwardsBabbage created) = do
    putWord8 0
    put created
  put (InputsApplied BabbageEraOnwardsConway created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> InputsApplied BabbageEraOnwardsBabbage <$> get
      1 -> InputsApplied BabbageEraOnwardsConway <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data InputsAppliedInEra era v = InputsAppliedInEra
  { version :: MarloweVersion v
  , contractId :: ContractId
  , metadata :: MarloweTransactionMetadata
  , input :: TransactionScriptOutput v
  , output :: TransactionOutput v
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , inputs :: Inputs v
  , txBody :: TxBody era
  }

deriving instance Show (InputsAppliedInEra BabbageEra 'V1)
deriving instance Eq (InputsAppliedInEra BabbageEra 'V1)
deriving instance Show (InputsAppliedInEra ConwayEra 'V1)
deriving instance Eq (InputsAppliedInEra ConwayEra 'V1)

instance (IsShelleyBasedEra era) => Variations (InputsAppliedInEra era 'V1) where
  variations =
    InputsAppliedInEra MarloweV1
      <$> variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations
        `varyAp` variations

instance (IsShelleyBasedEra era) => Binary (InputsAppliedInEra era 'V1) where
  put InputsAppliedInEra{..} = do
    put contractId
    put metadata
    put input
    put output
    put invalidBefore
    put invalidHereafter
    putInputs MarloweV1 inputs
    putTxBody txBody
  get = do
    let version = MarloweV1
    contractId <- get
    metadata <- get
    input <- get
    output <- get
    invalidBefore <- get
    invalidHereafter <- get
    inputs <- getInputs MarloweV1
    txBody <- getTxBody
    pure InputsAppliedInEra{..}

data WithdrawTx v where
  WithdrawTx
    :: BabbageEraOnwards era -> WithdrawTxInEra era v -> WithdrawTx v

instance Variations (WithdrawTx 'V1) where
  variations = WithdrawTx BabbageEraOnwardsBabbage <$> variations

instance Show (WithdrawTx 'V1) where
  showsPrec p (WithdrawTx BabbageEraOnwardsBabbage created) =
    showParen (p > 10) $
      showString "WithdrawTx"
        . showSpace
        . showString "BabbageEraOnwardsBabbage"
        . showsPrec 11 created
  showsPrec p (WithdrawTx BabbageEraOnwardsConway created) =
    showParen (p > 10) $
      showString "WithdrawTx"
        . showSpace
        . showString "BabbageEraOnwardsConway"
        . showsPrec 11 created

instance Eq (WithdrawTx 'V1) where
  WithdrawTx BabbageEraOnwardsBabbage a == WithdrawTx BabbageEraOnwardsBabbage b =
    a == b
  WithdrawTx BabbageEraOnwardsBabbage _ == _ = False
  WithdrawTx BabbageEraOnwardsConway a == WithdrawTx BabbageEraOnwardsConway b =
    a == b
  WithdrawTx BabbageEraOnwardsConway _ == _ = False

instance Binary (WithdrawTx 'V1) where
  put (WithdrawTx BabbageEraOnwardsBabbage created) = do
    putWord8 0
    put created
  put (WithdrawTx BabbageEraOnwardsConway created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> WithdrawTx BabbageEraOnwardsBabbage <$> get
      1 -> WithdrawTx BabbageEraOnwardsConway <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data WithdrawTxInEra era v = WithdrawTxInEra
  { version :: MarloweVersion v
  , inputs :: Map TxOutRef (Payout v)
  , txBody :: TxBody era
  }

deriving instance Show (WithdrawTxInEra BabbageEra 'V1)
deriving instance Eq (WithdrawTxInEra BabbageEra 'V1)
deriving instance Show (WithdrawTxInEra ConwayEra 'V1)
deriving instance Eq (WithdrawTxInEra ConwayEra 'V1)

instance (IsShelleyBasedEra era) => Variations (WithdrawTxInEra era 'V1) where
  variations = WithdrawTxInEra MarloweV1 <$> variations `varyAp` variations

instance (IsShelleyBasedEra era) => Binary (WithdrawTxInEra era 'V1) where
  put WithdrawTxInEra{..} = do
    put inputs
    putTxBody txBody
  get = do
    let version = MarloweV1
    inputs <- get
    txBody <- getTxBody
    pure WithdrawTxInEra{..}

instance (IsShelleyBasedEra era) => ToJSON (InputsAppliedInEra era 'V1) where
  toJSON InputsAppliedInEra{..} =
    object
      [ "contract-id" .= contractId
      , "input" .= input
      , "output" .= output
      , "invalid-before" .= invalidBefore
      , "invalid-hereafter" .= invalidHereafter
      , "inputs" .= inputs
      , "tx-body" .= serialiseToTextEnvelope Nothing txBody
      ]

data BurnTx where
  BurnTx
    :: BabbageEraOnwards era -> BurnTxInEra era -> BurnTx

instance Variations BurnTx where
  variations = BurnTx BabbageEraOnwardsBabbage <$> variations

instance Show BurnTx where
  showsPrec p (BurnTx BabbageEraOnwardsBabbage created) =
    showParen (p > 10) $
      showString "BurnTx"
        . showSpace
        . showString "BabbageEraOnwardsBabbage"
        . showsPrec 11 created
  showsPrec p (BurnTx BabbageEraOnwardsConway created) =
    showParen (p > 10) $
      showString "BurnTx"
        . showSpace
        . showString "BabbageEraOnwardsConway"
        . showsPrec 11 created

instance Eq BurnTx where
  BurnTx BabbageEraOnwardsBabbage a == BurnTx BabbageEraOnwardsBabbage b =
    a == b
  BurnTx BabbageEraOnwardsBabbage _ == _ = False
  BurnTx BabbageEraOnwardsConway a == BurnTx BabbageEraOnwardsConway b =
    a == b
  BurnTx BabbageEraOnwardsConway _ == _ = False

instance Binary BurnTx where
  put (BurnTx BabbageEraOnwardsBabbage created) = do
    putWord8 0
    put created
  put (BurnTx BabbageEraOnwardsConway created) = do
    putWord8 1
    put created
  get = do
    eraTag <- getWord8
    case eraTag of
      0 -> BurnTx BabbageEraOnwardsBabbage <$> get
      1 -> BurnTx BabbageEraOnwardsConway <$> get
      _ -> fail $ "Invalid era tag value: " <> show eraTag

data BurnTxInEra era = BurnTxInEra
  { burnedTokens :: Chain.Assets
  , txBody :: TxBody era
  }

deriving instance Show (BurnTxInEra BabbageEra)
deriving instance Eq (BurnTxInEra BabbageEra)
deriving instance Show (BurnTxInEra ConwayEra)
deriving instance Eq (BurnTxInEra ConwayEra)

instance (IsShelleyBasedEra era) => Variations (BurnTxInEra era) where
  variations = BurnTxInEra <$> variations `varyAp` variations

instance (IsShelleyBasedEra era) => Binary (BurnTxInEra era) where
  put BurnTxInEra{..} = do
    put burnedTokens
    putTxBody txBody
  get = do
    burnedTokens <- get
    txBody <- getTxBody
    pure BurnTxInEra{..}

data Account
  = RoleAccount TokenName
  | AddressAccount Address
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data BurnError
  = BurnRolesActive (Set AssetId)
  | BurnNoTokens
  | BurnBalancingError String
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data RoleTokenFilter
  = MkRoleTokensOr RoleTokenFilter RoleTokenFilter
  | MkRoleTokensAnd RoleTokenFilter RoleTokenFilter
  | RoleTokenFilterAny
  | RoleTokenFilterNone
  | MkRoleTokenFilterByContracts (Set ContractId)
  | MkRoleTokenFilterByPolicyIds (Set PolicyId)
  | MkRoleTokenFilterByTokens (Set AssetId)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Variations)

optimizeRoleTokenFilter :: RoleTokenFilter -> RoleTokenFilter
optimizeRoleTokenFilter = \case
  RoleTokensOr a b -> RoleTokensOr (optimizeRoleTokenFilter a) (optimizeRoleTokenFilter b)
  RoleTokensAnd a b -> RoleTokensAnd (optimizeRoleTokenFilter a) (optimizeRoleTokenFilter b)
  RoleTokenFilterAny -> RoleTokenFilterAny
  RoleTokenFilterNone -> RoleTokenFilterNone
  RoleTokenFilterByContracts a -> RoleTokenFilterByContracts a
  RoleTokenFilterByPolicyIds a -> RoleTokenFilterByPolicyIds a
  RoleTokenFilterByTokens a -> RoleTokenFilterByTokens a

instance Binary RoleTokenFilter where
  put (RoleTokensOr a b) = do
    putWord8 0
    put a
    put b
  put (RoleTokensAnd a b) = do
    putWord8 1
    put a
    put b
  put RoleTokenFilterAny = putWord8 2
  put RoleTokenFilterNone = putWord8 3
  put (RoleTokenFilterByContracts contracts) = do
    putWord8 4
    put contracts
  put (RoleTokenFilterByPolicyIds policies) = do
    putWord8 5
    put policies
  put (RoleTokenFilterByTokens tokens) = do
    putWord8 6
    put tokens

  get = label "RoleTokenFilter" do
    tag <- getWord8
    join $ label "tag" case tag of
      0 -> pure $ label "RoleTokensOr" $ RoleTokensOr <$> label "lhs" get <*> label "rhs" get
      1 -> pure $ label "RoleTokensAnd" $ RoleTokensAnd <$> label "lhs" get <*> label "rhs" get
      2 -> pure $ pure RoleTokenFilterAny
      3 -> pure $ pure RoleTokenFilterNone
      4 -> pure $ label "RoleTokenFilterByContracts" $ RoleTokenFilterByContracts <$> get
      5 -> pure $ label "RoleTokenFilterByPolicyIds" $ RoleTokenFilterByPolicyIds <$> get
      6 -> pure $ label "RoleTokenFilterByTokens" $ RoleTokenFilterByTokens <$> get
      _ -> fail $ "invalid value: " <> show tag

{-# COMPLETE
  RoleTokensOr
  , RoleTokensAnd
  , RoleTokenFilterAny
  , RoleTokenFilterNone
  , RoleTokenFilterByContracts
  , RoleTokenFilterByPolicyIds
  , RoleTokenFilterByTokens
  #-}

pattern RoleTokensOr :: RoleTokenFilter -> RoleTokenFilter -> RoleTokenFilter
pattern RoleTokensOr a b <- MkRoleTokensOr a b
  where
    RoleTokensOr RoleTokenFilterAny _ = RoleTokenFilterAny
    RoleTokensOr _ RoleTokenFilterAny = RoleTokenFilterAny
    RoleTokensOr RoleTokenFilterNone b = b
    RoleTokensOr a RoleTokenFilterNone = a
    RoleTokensOr (RoleTokenFilterByContracts a) (RoleTokenFilterByContracts b) =
      RoleTokenFilterByContracts (Set.union a b)
    RoleTokensOr (RoleTokenFilterByPolicyIds a) (RoleTokenFilterByPolicyIds b) =
      RoleTokenFilterByPolicyIds (Set.union a b)
    RoleTokensOr (RoleTokenFilterByTokens a) (RoleTokenFilterByTokens b) =
      RoleTokenFilterByTokens (Set.union a b)
    RoleTokensOr (RoleTokenFilterByPolicyIds a) (RoleTokenFilterByTokens b) =
      RoleTokensOr (RoleTokenFilterByTokens b) (RoleTokenFilterByPolicyIds a)
    RoleTokensOr (RoleTokenFilterByTokens a) (RoleTokenFilterByPolicyIds b) =
      let tokens = Set.filter (not . flip Set.member b . policyId) a
       in if tokens == a
            then RoleTokensOr (RoleTokenFilterByTokens tokens) (RoleTokenFilterByPolicyIds b)
            else MkRoleTokensOr (RoleTokenFilterByTokens tokens) (RoleTokenFilterByPolicyIds b)
    RoleTokensOr a (RoleTokensOr b c) = RoleTokensOr (RoleTokensOr a b) c
    RoleTokensOr a b
      | a == b = a
      | otherwise = MkRoleTokensOr a b

pattern RoleTokensAnd :: RoleTokenFilter -> RoleTokenFilter -> RoleTokenFilter
pattern RoleTokensAnd a b <- MkRoleTokensAnd a b
  where
    RoleTokensAnd RoleTokenFilterAny b = b
    RoleTokensAnd a RoleTokenFilterAny = a
    RoleTokensAnd RoleTokenFilterNone _ = RoleTokenFilterNone
    RoleTokensAnd _ RoleTokenFilterNone = RoleTokenFilterNone
    RoleTokensAnd (RoleTokenFilterByContracts a) (RoleTokenFilterByContracts b) =
      RoleTokenFilterByContracts (Set.intersection a b)
    RoleTokensAnd (RoleTokenFilterByPolicyIds a) (RoleTokenFilterByPolicyIds b) =
      RoleTokenFilterByPolicyIds (Set.intersection a b)
    RoleTokensAnd (RoleTokenFilterByTokens a) (RoleTokenFilterByTokens b) =
      RoleTokenFilterByTokens (Set.intersection a b)
    RoleTokensAnd (RoleTokenFilterByPolicyIds a) (RoleTokenFilterByTokens b) =
      RoleTokensAnd (RoleTokenFilterByTokens b) (RoleTokenFilterByPolicyIds a)
    RoleTokensAnd (RoleTokenFilterByTokens a) (RoleTokenFilterByPolicyIds b) =
      RoleTokenFilterByTokens $ Set.filter (flip Set.member b . policyId) a
    RoleTokensAnd a (RoleTokensAnd b c) = RoleTokensAnd (RoleTokensAnd a b) c
    RoleTokensAnd a b
      | a == b = a
      | otherwise = MkRoleTokensAnd a b

pattern RoleTokenFilterByContracts :: Set ContractId -> RoleTokenFilter
pattern RoleTokenFilterByContracts contracts <- MkRoleTokenFilterByContracts contracts
  where
    RoleTokenFilterByContracts contracts
      | Set.null contracts = RoleTokenFilterNone
      | otherwise = MkRoleTokenFilterByContracts contracts

pattern RoleTokenFilterByPolicyIds :: Set PolicyId -> RoleTokenFilter
pattern RoleTokenFilterByPolicyIds policies <- MkRoleTokenFilterByPolicyIds policies
  where
    RoleTokenFilterByPolicyIds policies
      | Set.null policies = RoleTokenFilterNone
      | otherwise = MkRoleTokenFilterByPolicyIds policies

pattern RoleTokenFilterByTokens :: Set AssetId -> RoleTokenFilter
pattern RoleTokenFilterByTokens tokens <- MkRoleTokenFilterByTokens tokens
  where
    RoleTokenFilterByTokens tokens
      | Set.null tokens = RoleTokenFilterNone
      | otherwise = MkRoleTokenFilterByTokens tokens

data RoleToken = RoleToken
  { roleTokenAssetId :: AssetId
  , roleTokenContract :: ContractId
  }

evalRoleTokenFilter :: RoleTokenFilter -> RoleToken -> Bool
evalRoleTokenFilter f RoleToken{..} = go f
  where
    go = \case
      RoleTokensOr f1 f2 -> go f1 || go f2
      RoleTokensAnd f1 f2 -> go f1 && go f2
      RoleTokenFilterAny -> True
      RoleTokenFilterNone -> False
      RoleTokenFilterByContracts contracts -> Set.member roleTokenContract contracts
      RoleTokenFilterByPolicyIds policies -> Set.member (policyId roleTokenAssetId) policies
      RoleTokenFilterByTokens tokens -> Set.member roleTokenAssetId tokens

filterRoleTokens :: RoleTokenFilter -> Set RoleToken -> Set RoleToken
filterRoleTokens = Set.filter . evalRoleTokenFilter

-- | The low-level runtime API for building and submitting transactions.
data MarloweTxCommand status err result where
  -- | Construct a transaction that starts a new Marlowe contract. The
  -- resulting, unsigned transaction can be signed via the cardano API or a
  -- wallet provider. When signed, the 'Submit' command can be used to submit
  -- the transaction to the attached Cardano node.
  Create
    :: Maybe StakeCredential
    -- ^ A reference to the stake address to use for script addresses.
    -> MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> Maybe TokenName
    -- ^ An optional thread token name that gets sent to the contract using the roles policy ID. Defaults to @""@.
    -> RoleTokensConfig
    -- ^ How to initialize role tokens
    -> MarloweTransactionMetadata
    -- ^ Optional metadata to attach to the transaction
    -> Maybe Lovelace
    -- ^ Optional min Lovelace deposit which should be used for the contract output.
    -> Map Account Assets
    -- ^ Initial account balances. The min ADA deposit will be added to this.
    -> Either (Contract v) DatumHash
    -- ^ The contract to run, or the hash of the contract to load from the store.
    -> MarloweTxCommand Void CreateError (ContractCreated v)
  -- | Construct a transaction that advances an active Marlowe contract by
  -- applying a sequence of inputs. The resulting, unsigned transaction can be
  -- signed via the cardano API or a wallet provider. When signed, the 'Submit'
  -- command can be used to submit the transaction to the attached Cardano node.
  ApplyInputs
    :: MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> ContractId
    -- ^ The ID of the contract to apply the inputs to.
    -> MarloweTransactionMetadata
    -- ^ Optional metadata to attach to the transaction
    -> Maybe UTCTime
    -- ^ The "invalid before" bound of the validity interval. If omitted, this
    -- is computed from the contract.
    -> Maybe UTCTime
    -- ^ The "invalid hereafter" bound of the validity interval. If omitted, this
    -- is computed from the contract.
    -> Inputs v
    -- ^ The inputs to apply.
    -> MarloweTxCommand Void ApplyInputsError (InputsApplied v)
  -- | Construct a transaction that withdraws available assets from a set of
  -- Marlowe contract payouts. The resulting, unsigned transaction can be signed
  -- via the cardano API or a wallet provider. When signed, the 'Submit' command
  -- can be used to submit the transaction to the attached Cardano node.
  Withdraw
    :: MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> Set TxOutRef
    -- ^ The payouts to withdraw.
    -> MarloweTxCommand
        Void
        WithdrawError
        ( WithdrawTx v -- The unsigned tx body, to be signed by a wallet.
        )
  -- | Construct a transaction that burns all role tokens in a wallet which match
  -- the given filter.
  Burn
    :: WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> RoleTokenFilter
    -- ^ Which role tokens to burn
    -> MarloweTxCommand Void BurnError BurnTx
  -- | Submits a signed transaction to the attached Cardano node.
  Submit
    :: BabbageEraOnwards era
    -> Tx era
    -- ^ A signed transaction to submit
    -> MarloweTxCommand
        SubmitStatus -- This job reports the status of the tx submission, which can take some time.
        SubmitError
        BlockHeader -- The block header of the block this transaction was added to.

instance HasSignature MarloweTxCommand where
  signature _ = "MarloweTxCommand"

instance OTelCommand MarloweTxCommand where
  commandTypeName _ = "marlowe_tx_command"
  commandName = \case
    TagCreate _ -> "create"
    TagApplyInputs _ -> "apply_inputs"
    TagWithdraw _ -> "withdraw"
    TagBurn -> "burn"
    TagSubmit -> "submit"

instance Command MarloweTxCommand where
  data Tag MarloweTxCommand status err result where
    TagCreate :: MarloweVersion v -> Tag MarloweTxCommand Void CreateError (ContractCreated v)
    TagApplyInputs :: MarloweVersion v -> Tag MarloweTxCommand Void ApplyInputsError (InputsApplied v)
    TagWithdraw :: MarloweVersion v -> Tag MarloweTxCommand Void WithdrawError (WithdrawTx v)
    TagBurn :: Tag MarloweTxCommand Void BurnError BurnTx
    TagSubmit :: Tag MarloweTxCommand SubmitStatus SubmitError BlockHeader

  data JobId MarloweTxCommand stats err result where
    JobIdSubmit :: TxId -> JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader

  tagFromCommand = \case
    Create _ version _ _ _ _ _ _ _ -> TagCreate version
    ApplyInputs version _ _ _ _ _ _ -> TagApplyInputs version
    Withdraw version _ _ -> TagWithdraw version
    Burn _ _ -> TagBurn
    Submit _ _ -> TagSubmit

  tagFromJobId = \case
    JobIdSubmit _ -> TagSubmit

  tagEq t1 t2 = case (t1, t2) of
    (TagCreate MarloweV1, TagCreate MarloweV1) -> pure (Refl, Refl, Refl)
    (TagCreate _, _) -> Nothing
    (TagApplyInputs MarloweV1, TagApplyInputs MarloweV1) -> pure (Refl, Refl, Refl)
    (TagApplyInputs _, _) -> Nothing
    (TagWithdraw MarloweV1, TagWithdraw MarloweV1) -> pure (Refl, Refl, Refl)
    (TagWithdraw _, _) -> Nothing
    (TagBurn, TagBurn) -> pure (Refl, Refl, Refl)
    (TagBurn, _) -> Nothing
    (TagSubmit, TagSubmit) -> pure (Refl, Refl, Refl)
    (TagSubmit, _) -> Nothing

  putTag = \case
    TagCreate version -> putWord8 0x01 *> put (SomeMarloweVersion version)
    TagApplyInputs version -> putWord8 0x02 *> put (SomeMarloweVersion version)
    TagWithdraw version -> putWord8 0x03 *> put (SomeMarloweVersion version)
    TagSubmit -> putWord8 0x04
    TagBurn -> putWord8 0x05

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> do
        SomeMarloweVersion version <- get
        pure $ SomeTag $ TagCreate version
      0x02 -> do
        SomeMarloweVersion version <- get
        pure $ SomeTag $ TagApplyInputs version
      0x03 -> do
        SomeMarloweVersion version <- get
        pure $ SomeTag $ TagWithdraw version
      0x04 -> pure $ SomeTag TagSubmit
      0x05 -> pure $ SomeTag TagBurn
      _ -> fail $ "Invalid command tag: " <> show tag

  putJobId = \case
    JobIdSubmit txId -> put txId

  getJobId = \case
    TagCreate _ -> fail "create has no job ID"
    TagApplyInputs _ -> fail "apply inputs has no job ID"
    TagWithdraw _ -> fail "withdraw has no job ID"
    TagBurn -> fail "burn has no job ID"
    TagSubmit -> JobIdSubmit <$> get

  putCommand = \case
    Create mStakeCredential MarloweV1 walletAddresses threadName roles metadata minAda accounts contract -> do
      put mStakeCredential
      put walletAddresses
      put threadName
      put roles
      put metadata
      put minAda
      put accounts
      put contract
    ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer -> do
      put walletAddresses
      put contractId
      put metadata
      maybe (putWord8 0) (\t -> putWord8 1 *> put t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> put t) invalidHereafter
      putInputs version redeemer
    Withdraw _ walletAddresses payoutIds -> do
      put walletAddresses
      put payoutIds
    Burn walletAddresses tokenFilter -> do
      put walletAddresses
      put tokenFilter
    Submit era tx -> case era of
      BabbageEraOnwardsBabbage -> do
        putWord8 0
        put $ serialiseToCBOR tx
      BabbageEraOnwardsConway -> do
        putWord8 1
        put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate MarloweV1 ->
      Create <$> get <*> pure MarloweV1 <*> get <*> get <*> get <*> get <*> get <*> get <*> get
    TagApplyInputs version -> do
      walletAddresses <- get
      contractId <- get
      metadata <- get
      invalidBefore <-
        getWord8 >>= \case
          0 -> pure Nothing
          1 -> Just <$> get
          t -> fail $ "Invalid Maybe tag: " <> show t
      invalidHereafter <-
        getWord8 >>= \case
          0 -> pure Nothing
          1 -> Just <$> get
          t -> fail $ "Invalid Maybe tag: " <> show t
      redeemer <- getInputs version
      pure $ ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer
    TagWithdraw version -> do
      walletAddresses <- get
      Withdraw version walletAddresses <$> get
    TagBurn -> Burn <$> get <*> get
    TagSubmit -> do
      eraTag <- getWord8
      case eraTag of
        0 -> do
          bytes <- get @ByteString
          Submit BabbageEraOnwardsBabbage <$> case deserialiseFromCBOR (AsTx AsBabbageEra) bytes of
            Left err -> fail $ show err
            Right tx -> pure tx
        1 -> do
          bytes <- get @ByteString
          Submit BabbageEraOnwardsConway <$> case deserialiseFromCBOR (AsTx AsConwayEra) bytes of
            Left err -> fail $ show err
            Right tx -> pure tx
        _ -> fail $ "Invalid era tag: " <> show eraTag

  putStatus = \case
    TagCreate _ -> absurd
    TagApplyInputs _ -> absurd
    TagWithdraw _ -> absurd
    TagBurn -> absurd
    TagSubmit -> put

  getStatus = \case
    TagCreate _ -> fail "create has no status"
    TagApplyInputs _ -> fail "apply inputs has no status"
    TagWithdraw _ -> fail "withdraw has no status"
    TagBurn -> fail "burn has no status"
    TagSubmit -> get

  putErr = \case
    TagCreate MarloweV1 -> put
    TagApplyInputs MarloweV1 -> put
    TagWithdraw MarloweV1 -> put
    TagBurn -> put
    TagSubmit -> put

  getErr = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw MarloweV1 -> get
    TagBurn -> get
    TagSubmit -> get

  putResult = \case
    TagCreate MarloweV1 -> put
    TagApplyInputs MarloweV1 -> put
    TagWithdraw MarloweV1 -> put
    TagBurn -> put
    TagSubmit -> put

  getResult = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw MarloweV1 -> get
    TagBurn -> get
    TagSubmit -> get

putTxBody :: (IsShelleyBasedEra era) => TxBody era -> Put
putTxBody = put . serialiseToCBOR

getTxBody :: forall era. (IsShelleyBasedEra era) => Get (TxBody era)
getTxBody = do
  bytes <- get @ByteString
  case deserialiseFromCBOR (AsTxBody $ cardanoEraToAsType $ cardanoEra @era) bytes of
    Left err -> fail $ show err
    Right txBody -> pure txBody

data WalletAddresses = WalletAddresses
  { changeAddress :: Address
  , extraAddresses :: Set Address
  , collateralUtxos :: Set TxOutRef
  }
  deriving (Eq, Show, Generic, Binary, ToJSON, Variations)

data CoinSelectionError
  = NoCollateralFound (Set TxOutRef)
  | InsufficientLovelace
      { required :: Integer
      , available :: Integer
      }
  | InsufficientTokens Tokens
  deriving (Eq, Ord, Show, Generic, Binary, ToJSON, Variations)

-- | Errors that can occur when trying to solve the constraints.
data ConstraintError
  = MintingUtxoNotFound TxOutRef
  | RoleTokenNotFound AssetId
  | ToCardanoError
  | MissingMarloweInput
  | PayoutNotFound TxOutRef
  | InvalidPayoutDatum TxOutRef (Maybe Chain.Datum)
  | InvalidHelperDatum TxOutRef (Maybe Chain.Datum)
  | InvalidPayoutScriptAddress TxOutRef Address
  | CalculateMinUtxoFailed String
  | CoinSelectionFailed CoinSelectionError
  | BalancingError String
  | MarloweInputInWithdraw
  | MarloweOutputInWithdraw
  | PayoutOutputInWithdraw
  | PayoutInputInCreateOrApply
  | UnknownPayoutScript ScriptHash
  | HelperScriptNotFound Chain.TokenName
  deriving (Generic)

deriving instance Eq ConstraintError
deriving instance Ord ConstraintError
deriving instance Show ConstraintError
deriving instance Binary ConstraintError
deriving instance Variations ConstraintError
deriving instance ToJSON ConstraintError

data CreateError
  = CreateEraUnsupported AnyCardanoEra
  | CreateConstraintError ConstraintError
  | CreateLoadMarloweContextFailed LoadMarloweContextError
  | CreateLoadHelpersContextFailed LoadHelpersContextError
  | CreateBuildupFailed CreateBuildupError
  | CreateToCardanoError
  | CreateSafetyAnalysisFailed [SafetyError]
  | -- | This error is thrown when the safety analysis process itself fails itself
    -- due to a timeout or other reasons, such as missing merkleization data.
    CreateSafetyAnalysisError String
  | CreateContractNotFound
  | ProtocolParamNoUTxOCostPerByte
  | InsufficientMinAdaDeposit Lovelace
  deriving (Generic)

deriving instance Eq CreateError
deriving instance Show CreateError
instance Binary CreateError
instance Variations CreateError
instance ToJSON CreateError

data CreateBuildupError
  = MintingUtxoSelectionFailed
  | AddressDecodingFailed Address
  | MintingScriptDecodingFailed PlutusScript
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data ApplyInputsError
  = ApplyInputsEraUnsupported AnyCardanoEra
  | ApplyInputsConstraintError ConstraintError
  | ScriptOutputNotFound
  | ApplyInputsLoadMarloweContextFailed LoadMarloweContextError
  | ApplyInputsLoadHelpersContextFailed LoadHelpersContextError
  | ApplyInputsConstraintsBuildupFailed ApplyInputsConstraintsBuildupError
  | SlotConversionFailed String
  | TipAtGenesis
  | ValidityLowerBoundTooHigh SlotNo SlotNo
  deriving (Generic)

deriving instance Eq ApplyInputsError
deriving instance Show ApplyInputsError
instance Binary ApplyInputsError
instance Variations ApplyInputsError
instance ToJSON ApplyInputsError

data ApplyInputsConstraintsBuildupError
  = MarloweComputeTransactionFailed V1.TransactionError
  | UnableToDetermineTransactionTimeout
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, Variations, ToJSON)

data WithdrawError
  = WithdrawEraUnsupported AnyCardanoEra
  | WithdrawConstraintError ConstraintError
  | EmptyPayouts
  | WithdrawLoadHelpersContextFailed LoadHelpersContextError
  deriving (Generic)

deriving instance Eq WithdrawError
deriving instance Show WithdrawError
instance Binary WithdrawError
instance Variations WithdrawError
instance ToJSON WithdrawError

data LoadMarloweContextError
  = LoadMarloweContextErrorNotFound
  | LoadMarloweContextErrorVersionMismatch SomeMarloweVersion
  | LoadMarloweContextToCardanoError
  | MarloweScriptNotPublished ScriptHash
  | PayoutScriptNotPublished ScriptHash
  | ExtractCreationError ExtractCreationError
  | ExtractMarloweTransactionError ExtractMarloweTransactionError
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data LoadHelpersContextError
  = HelperScriptNotFoundInRegistry HelperScript
  | LoadHelpersContextErrorNotFound TxId
  | LoadHelpersContextErrorVersionMismatch SomeMarloweVersion
  | ContractNotExtractedError ExtractCreationError
  | RollForwardToGenesisError
  | LoadHelpersContextTxOutRefNotFoundError TxOutRef
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data SubmitError
  = SubmitException String
  | SubmitFailed String -- should be from show TxValidationErrorInMode
  | TxDiscarded
  deriving (Eq, Show, Generic, Binary, ToJSON, Variations)

data SubmitStatus
  = Submitting
  | Accepted
  deriving (Eq, Show, Generic, Binary, ToJSON, Variations)

instance CommandEq MarloweTxCommand where
  commandEq = \case
    Create stake MarloweV1 wallet threadName roleTokenConfig metadata minAda accounts contract -> \case
      Create stake' MarloweV1 wallet' threadName' roleTokenConfig' metadata' minAda' accounts' contract' ->
        stake == stake'
          && wallet == wallet'
          && threadName == threadName'
          && roleTokenConfig == roleTokenConfig'
          && metadata == metadata'
          && minAda == minAda'
          && accounts == accounts'
          && contract == contract'
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> \case
      ApplyInputs MarloweV1 wallet' contractId' metadata' invalidBefore' invalidHereafter' inputs' ->
        wallet == wallet'
          && contractId == contractId'
          && metadata == metadata'
          && invalidBefore == invalidBefore'
          && invalidHereafter == invalidHereafter'
          && inputs == inputs'
    Withdraw MarloweV1 wallet payoutIds -> \case
      Withdraw MarloweV1 wallet' payoutIds' ->
        wallet == wallet'
          && payoutIds == payoutIds'
    Burn wallet tokenFilter -> \case
      Burn wallet' tokenFilter' ->
        wallet == wallet'
          && tokenFilter == tokenFilter'
    Submit BabbageEraOnwardsBabbage tx -> \case
      Submit BabbageEraOnwardsBabbage tx' -> tx == tx'
      _ -> False
    Submit BabbageEraOnwardsConway tx -> \case
      Submit BabbageEraOnwardsConway tx' -> tx == tx'
      _ -> False

  jobIdEq = \case
    JobIdSubmit txId -> \case
      JobIdSubmit txId' -> txId == txId'

  statusEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagBurn -> (==)
    TagSubmit -> (==)

  errEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagBurn -> (==)
    TagSubmit -> (==)

  resultEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagBurn -> (==)
    TagSubmit -> (==)

instance ShowCommand MarloweTxCommand where
  showsPrecTag p = \case
    TagCreate MarloweV1 ->
      showParen
        (p >= 11)
        ( showString "TagCreate"
            . showSpace
            . showString "MarloweV1"
        )
    TagApplyInputs MarloweV1 ->
      showParen
        (p >= 11)
        ( showString "TagApplyInputs"
            . showSpace
            . showString "MarloweV1"
        )
    TagWithdraw MarloweV1 ->
      showParen
        (p >= 11)
        ( showString "TagWithdraw"
            . showSpace
            . showString "MarloweV1"
        )
    TagBurn -> showString "TagBurn"
    TagSubmit -> showString "TagSubmit"

  showsPrecCommand p =
    showParen (p >= 11) . \case
      Create stake MarloweV1 wallet threadName roleTokenConfig metadata minAda accounts contract ->
        ( showString "Create"
            . showSpace
            . showsPrec 11 stake
            . showSpace
            . showsPrec 11 MarloweV1
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 threadName
            . showSpace
            . showsPrec 11 roleTokenConfig
            . showSpace
            . showsPrec 11 metadata
            . showSpace
            . showsPrec 11 minAda
            . showSpace
            . showsPrec 11 accounts
            . showSpace
            . showsPrec 11 contract
        )
      ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs ->
        ( showString "ApplyInputs"
            . showSpace
            . showsPrec 11 MarloweV1
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 contractId
            . showSpace
            . showsPrec 11 metadata
            . showSpace
            . showsPrec 11 invalidBefore
            . showSpace
            . showsPrec 11 invalidHereafter
            . showSpace
            . showsPrec 11 inputs
        )
      Withdraw MarloweV1 wallet payoutIds ->
        ( showString "Withdraw"
            . showSpace
            . showsPrec 11 MarloweV1
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 payoutIds
        )
      Burn wallet tokenFilter ->
        ( showString "Burn"
            . showSpace
            . showsPrec 11 wallet
            . showSpace
            . showsPrec 11 tokenFilter
        )
      Submit BabbageEraOnwardsBabbage tx ->
        ( showString "Submit"
            . showSpace
            . showString "BabbageEraOnwardsBabbage"
            . showSpace
            . showsPrec 11 tx
        )
      Submit BabbageEraOnwardsConway tx ->
        ( showString "Submit"
            . showSpace
            . showString "BabbageEraOnwardsConway"
            . showSpace
            . showsPrec 11 tx
        )

  showsPrecJobId p = \case
    JobIdSubmit txId ->
      showParen
        (p >= 11)
        ( showString "JobIdSubmit"
            . showSpace
            . showsPrec 11 txId
        )

  showsPrecStatus p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagBurn -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecErr p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagBurn -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecResult p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagBurn -> showsPrec p
    TagSubmit -> showsPrec p

instance Variations V1.TransactionError
instance Variations V1.TransactionWarning
instance Variations V1.Payment
instance Variations V1.TransactionOutput
instance (Variations a) => Variations (Safety.Transaction a)
instance Variations SatInt
instance Variations ExCPU
instance Variations ExMemory
instance Variations ExBudget
instance Variations PV2.DatumHash where
  variations = PV2.DatumHash . PV2.toBuiltin <$> variations @ByteString
instance Variations SafetyError

instance (IsShelleyBasedEra era) => Variations (TxBody era) where
  variations =
    either (error . show) pure $
      createAndValidateTransactionBody
        (C.shelleyBasedEra @era)
        (C.defaultTxBodyContent era)
          { txIns =
              [
                ( C.TxIn
                    (fromJust $ hush $ C.deserialiseFromRawBytes C.AsTxId $ BS.pack $ replicate 32 0)
                    $ C.TxIx 0
                , C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending
                )
              ]
          , txOuts = [C.TxOut addr value C.TxOutDatumNone CS.ReferenceScriptNone]
          }
    where
      era = C.shelleyBasedEra @era

      addr :: C.AddressInEra era
      addr =
        either (error . show) (C.AddressInEra $ C.ShelleyAddressInEra era) $
          C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) "addr1vy9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupceql82h"

      value :: C.TxOutValue era
      value = case era of
        C.ShelleyBasedEraShelley -> C.TxOutValueByron 1
        C.ShelleyBasedEraAllegra -> C.TxOutValueByron 1
        C.ShelleyBasedEraMary -> C.TxOutValueShelleyBased era $ C.toLedgerValue C.MaryEraOnwardsMary $ C.lovelaceToValue 1
        C.ShelleyBasedEraAlonzo -> C.TxOutValueShelleyBased era $ C.toLedgerValue C.MaryEraOnwardsMary $ C.lovelaceToValue 1
        C.ShelleyBasedEraBabbage -> C.TxOutValueShelleyBased era $ C.toLedgerValue C.MaryEraOnwardsMary $ C.lovelaceToValue 1
        C.ShelleyBasedEraConway -> C.TxOutValueShelleyBased era $ C.toLedgerValue C.MaryEraOnwardsMary $ C.lovelaceToValue 1

instance (IsShelleyBasedEra era) => Variations (Tx era) where
  variations = C.signShelleyTransaction (C.shelleyBasedEra @era) <$> variations <*> pure []
