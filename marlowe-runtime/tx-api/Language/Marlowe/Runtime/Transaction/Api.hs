{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
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
  IsToken (..),
  JobId (..),
  LoadHelpersContextError (..),
  LoadMarloweContextError (..),
  MarloweTxCommand (..),
  Mint (..),
  MintRole (..),
  NFTMetadataFile (..),
  RoleTokenFilter' (..),
  RoleTokenFilter,
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
  getTokenQuantities,
  hasRecipient,
  mkMint,
  optimizeRoleTokenFilter,
  rewriteRoleTokenFilter,
  roleTokenFilterToRoleCurrencyFilter,
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

import Control.Lens (Plated (..), rewrite)
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
import Language.Marlowe.Protocol.Query.Types (RoleCurrencyFilter (..))
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
  { burnedTokens :: Chain.Tokens
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
  = BurnEraUnsupported AnyCardanoEra
  | BurnRolesActive (Set PolicyId)
  | BurnInvalidPolicyId (Set PolicyId)
  | BurnNoTokens
  | BurnFromCardanoError
  | -- FIXME most of this error is not relevant to burning, but due to the current
    -- constraint solving being too marlowe-specific, and because we use the
    -- final balancing pipeline for burning, we sadly need to use this type here.
    BurnConstraintError ConstraintError
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON, Variations)

data RoleTokenFilter' c p t
  = RoleTokensOr (RoleTokenFilter' c p t) (RoleTokenFilter' c p t)
  | RoleTokensAnd (RoleTokenFilter' c p t) (RoleTokenFilter' c p t)
  | RoleTokensNot (RoleTokenFilter' c p t)
  | RoleTokenFilterAny
  | RoleTokenFilterNone
  | RoleTokenFilterByContracts (Set c)
  | RoleTokenFilterByPolicyIds (Set p)
  | RoleTokenFilterByTokens (Set t)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

instance (Variations c, Variations p, Variations t) => Variations (RoleTokenFilter' c p t) where
  variations =
    join
      [ pure (RoleTokensOr RoleTokenFilterNone RoleTokenFilterNone)
      , pure (RoleTokensAnd RoleTokenFilterNone RoleTokenFilterNone)
      , pure (RoleTokensNot RoleTokenFilterNone)
      , pure RoleTokenFilterNone
      , pure RoleTokenFilterAny
      , RoleTokenFilterByContracts <$> variations
      , RoleTokenFilterByPolicyIds <$> variations
      , RoleTokenFilterByTokens <$> variations
      ]

instance Plated (RoleTokenFilter' c p t) where
  plate f = \case
    RoleTokensOr f1 f2 -> RoleTokensOr <$> f f1 <*> f f2
    RoleTokensAnd f1 f2 -> RoleTokensAnd <$> f f1 <*> f f2
    RoleTokensNot f' -> RoleTokensNot <$> f f'
    rf -> pure rf

type RoleTokenFilter = RoleTokenFilter' ContractId PolicyId AssetId

class IsToken t p | t -> p where
  tokenPolicyId :: t -> p

instance IsToken AssetId PolicyId where
  tokenPolicyId = policyId

evalRoleTokenFilter :: (Ord c, Ord p, Ord t, IsToken t p) => RoleTokenFilter' c p t -> c -> t -> Bool
evalRoleTokenFilter f roleTokenContract roleToken = go f
  where
    go = \case
      RoleTokensOr f1 f2 -> go f1 || go f2
      RoleTokensAnd f1 f2 -> go f1 && go f2
      RoleTokensNot f' -> not $ go f'
      RoleTokenFilterAny -> True
      RoleTokenFilterNone -> False
      RoleTokenFilterByContracts contracts -> Set.member roleTokenContract contracts
      RoleTokenFilterByPolicyIds policies -> Set.member (tokenPolicyId roleToken) policies
      RoleTokenFilterByTokens tokens -> Set.member roleToken tokens

optimizeRoleTokenFilter :: (Ord c, Ord p, Ord t, IsToken t p) => RoleTokenFilter' c p t -> RoleTokenFilter' c p t
optimizeRoleTokenFilter = rewrite rewriteRoleTokenFilter

roleTokenFilterToRoleCurrencyFilter :: RoleTokenFilter -> RoleCurrencyFilter
roleTokenFilterToRoleCurrencyFilter = go
  where
    go :: RoleTokenFilter -> RoleCurrencyFilter
    go = \case
      RoleTokensOr f1 f2 -> go f1 `RoleCurrencyOr` go f2
      RoleTokensAnd f1 f2 -> go f1 `RoleCurrencyAnd` go f2
      RoleTokensNot f' -> RoleCurrencyNot $ go f'
      RoleTokenFilterAny -> RoleCurrencyFilterAny
      RoleTokenFilterNone -> RoleCurrencyFilterNone
      RoleTokenFilterByContracts contracts -> RoleCurrencyFilterByContract contracts
      RoleTokenFilterByPolicyIds policies -> RoleCurrencyFilterByPolicy policies
      RoleTokenFilterByTokens tokens -> RoleCurrencyFilterByPolicy (Set.map policyId tokens)

rewriteRoleTokenFilter :: (Ord c, Ord p, Ord t, IsToken t p) => RoleTokenFilter' c p t -> Maybe (RoleTokenFilter' c p t)
rewriteRoleTokenFilter = \case
  RoleTokenFilterAny -> Nothing
  RoleTokenFilterNone -> Nothing
  -- rule double-negation
  RoleTokensNot (RoleTokensNot a) -> Just a
  -- rule not-any
  RoleTokensNot RoleTokenFilterAny -> Just RoleTokenFilterNone
  -- rule not-none
  RoleTokensNot RoleTokenFilterNone -> Just RoleTokenFilterAny
  RoleTokenFilterByContracts contracts
    -- rule null-contracts
    | Set.null contracts -> Just RoleTokenFilterNone
    | otherwise -> Nothing
  RoleTokenFilterByPolicyIds policies
    -- rule null-policy-ids
    | Set.null policies -> Just RoleTokenFilterNone
    | otherwise -> Nothing
  RoleTokenFilterByTokens tokens
    -- rule null-tokens
    | Set.null tokens -> Just RoleTokenFilterNone
    | otherwise -> Nothing
  RoleTokensAnd a b -> rewriteRoleTokensAnd a b <|> rewriteRoleTokensAnd b a
  RoleTokensOr a b -> rewriteRoleTokensOr a b <|> rewriteRoleTokensOr b a
  _ -> Nothing

rewriteRoleTokensAnd
  :: (Ord c, Ord p, Ord t, IsToken t p)
  => RoleTokenFilter' c p t
  -> RoleTokenFilter' c p t
  -> Maybe (RoleTokenFilter' c p t)
rewriteRoleTokensAnd = curry \case
  -- rule and-annulment
  (_, RoleTokenFilterNone) -> Just RoleTokenFilterNone
  -- rule and-identity
  (a, RoleTokenFilterAny) -> Just a
  -- rule de-morgan
  (RoleTokensNot a, RoleTokensNot b) -> Just $ RoleTokensNot $ a `RoleTokensOr` b
  -- and-distribute
  (RoleTokensOr a b, RoleTokensOr c d)
    | a == c -> Just $ a `RoleTokensAnd` (b `RoleTokensOr` d)
    | a == d -> Just $ a `RoleTokensAnd` (b `RoleTokensOr` c)
    | b == c -> Just $ b `RoleTokensAnd` (a `RoleTokensOr` d)
    | b == d -> Just $ b `RoleTokensAnd` (a `RoleTokensOr` c)
  -- rule and-contracts
  (RoleTokenFilterByContracts a, RoleTokenFilterByContracts b) ->
    Just $ RoleTokenFilterByContracts $ Set.intersection a b
  -- rule and-policies
  (RoleTokenFilterByPolicyIds a, RoleTokenFilterByPolicyIds b) ->
    Just $ RoleTokenFilterByPolicyIds $ Set.intersection a b
  -- rule and-tokens
  (RoleTokenFilterByTokens a, RoleTokenFilterByTokens b) ->
    Just $ RoleTokenFilterByTokens $ Set.intersection a b
  -- rule and-policies-tokens
  (RoleTokenFilterByPolicyIds p, RoleTokenFilterByTokens t) ->
    Just $ RoleTokenFilterByTokens $ Set.filter (flip Set.member p . tokenPolicyId) t
  -- rule and-complement
  (a, RoleTokensNot a') | a == a' -> Just RoleTokenFilterNone
  -- rule and-absorption
  (a, RoleTokensOr b c)
    | a == b || a == c -> Just a
    | b == RoleTokensNot a -> Just $ a `RoleTokensAnd` c
    | c == RoleTokensNot a -> Just $ a `RoleTokensAnd` b
  -- rule and-idempotent
  (a, a') | a == a' -> Just a
  _ -> Nothing

rewriteRoleTokensOr
  :: (Ord c, Ord p, Ord t, IsToken t p)
  => RoleTokenFilter' c p t
  -> RoleTokenFilter' c p t
  -> Maybe (RoleTokenFilter' c p t)
rewriteRoleTokensOr = curry \case
  -- rule or-annulment
  (_, RoleTokenFilterAny) -> Just RoleTokenFilterAny
  -- rule or-identity
  (a, RoleTokenFilterNone) -> Just a
  -- rule de-morgan
  (RoleTokensNot a, RoleTokensNot b) -> Just $ RoleTokensNot $ a `RoleTokensAnd` b
  -- or-distribute
  (RoleTokensOr a b, RoleTokensOr c d)
    | a == c -> Just $ a `RoleTokensOr` (b `RoleTokensOr` d)
    | a == d -> Just $ a `RoleTokensOr` (b `RoleTokensOr` c)
    | b == c -> Just $ b `RoleTokensOr` (a `RoleTokensOr` d)
    | b == d -> Just $ b `RoleTokensOr` (a `RoleTokensOr` c)
  -- rule or-contracts
  (RoleTokenFilterByContracts a, RoleTokenFilterByContracts b) ->
    Just $ RoleTokenFilterByContracts $ Set.union a b
  -- rule or-policies
  (RoleTokenFilterByPolicyIds a, RoleTokenFilterByPolicyIds b) ->
    Just $ RoleTokenFilterByPolicyIds $ Set.union a b
  -- rule or-tokens
  (RoleTokenFilterByTokens a, RoleTokenFilterByTokens b) ->
    Just $ RoleTokenFilterByTokens $ Set.union a b
  -- rule or-policies-tokens
  (RoleTokenFilterByPolicyIds p, RoleTokenFilterByTokens t) ->
    let t' = Set.filter (not . flip Set.member p . tokenPolicyId) t
     in if t == t'
          then Nothing
          else Just $ RoleTokensOr (RoleTokenFilterByPolicyIds p) (RoleTokenFilterByTokens t')
  -- rule or-complement
  (a, RoleTokensNot a') | a == a' -> Just RoleTokenFilterAny
  -- rule or-absorption
  (a, RoleTokensAnd b c)
    | a == b || a == c -> Just a
    | b == RoleTokensNot a -> Just $ a `RoleTokensOr` c
    | c == RoleTokensNot a -> Just $ a `RoleTokensOr` b
  -- rule or-idempotent
  (a, a') | a == a' -> Just a
  _ -> Nothing

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
