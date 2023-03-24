{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsConstraintsBuildupError(..)
  , ApplyInputsError(..)
  , ConstraintError(..)
  , ContractCreated(..)
  , CreateBuildupError(..)
  , CreateError(..)
  , InputsApplied(..)
  , JobId(..)
  , LoadMarloweContextError(..)
  , MarloweTxCommand(..)
  , Mint(unMint)
  , NFTMetadataFile(..)
  , RoleTokenMetadata(..)
  , RoleTokensConfig(..)
  , SubmitError(..)
  , SubmitStatus(..)
  , Tag(..)
  , WalletAddresses(..)
  , WithdrawError(..)
  , decodeRoleTokenMetadata
  , encodeRoleTokenMetadata
  , mkMint
  ) where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , IsCardanoEra
  , Tx
  , TxBody
  , cardanoEra
  , deserialiseFromCBOR
  , serialiseToCBOR
  , serialiseToTextEnvelope
  )
import Control.Applicative ((<|>))
import Data.Aeson (ToJSON(..), object, (.!=), (.:!), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import Data.Aeson.Types ((.:))
import qualified Data.Aeson.Types as Aeson.Types
import Data.Binary (Binary, Get, get, getWord8, put)
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , AssetId
  , Assets
  , BlockHeader
  , Lovelace
  , Metadata(..)
  , PlutusScript
  , PolicyId(..)
  , ScriptHash
  , SlotNo
  , StakeCredential
  , TokenName(..)
  , TxId
  , TxOutRef
  , getUTCTime
  , parseMetadataList
  , parseMetadataMap
  , parseMetadataText
  , putUTCTime
  )
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api (ExtractCreationError, ExtractMarloweTransactionError)
import Network.HTTP.Media (MediaType)
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.Protocol.Job.Types
import qualified Network.URI as Network

instance Binary Network.URIAuth
instance Binary Network.URI

instance Binary MediaType where
  put = put . show
  get = fromString <$> get

instance Aeson.ToJSON MediaType where
  toJSON = Aeson.String . Text.pack . show

instance Aeson.FromJSON MediaType where
  parseJSON = fmap fromString . Aeson.parseJSON

data NFTMetadataFile = NFTMetadataFile
  { name :: Text
  , mediaType :: MediaType
  , src :: Network.URI
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Binary)

instance Aeson.ToJSON NFTMetadataFile where
  toJSON NFTMetadataFile {..} = Aeson.object
    [ ("name", toJSON name)
    , ("mediaType", toJSON mediaType)
    , ("src", toJSON $ show src)
    ]

parseJSONURI :: Text -> Aeson.Types.Parser Network.URI
parseJSONURI (Text.unpack -> s) =
  maybe (fail $ s <> " is not a valid URI!") pure $ Network.parseURI s

instance Aeson.FromJSON NFTMetadataFile where
  parseJSON = Aeson.withObject "NFTMetadataFile" \x ->
    NFTMetadataFile
      <$> x .: "name"
      <*> x .: "mediaType"
      <*> (parseJSONURI =<< x .: "src")

data RoleTokenMetadata = RoleTokenMetadata
  { name :: Text
  , image :: Network.URI
  , mediaType :: Maybe MediaType
  , description :: Maybe Text
  , files :: [NFTMetadataFile]
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving (Binary)

instance Aeson.ToJSON RoleTokenMetadata where
  toJSON RoleTokenMetadata {..} = Aeson.Object $ Aeson.KeyMap.fromList $
    [ ("name", toJSON name)
    , ("image", toJSON $ show image)
    ]
    <> maybeToList (fmap (("mediaType",) . toJSON) mediaType)
    <> maybeToList (fmap (("description",) . Aeson.String) description)
    <> case files of
      [] -> []
      _ -> [("files", toJSON files)]

instance Aeson.FromJSON RoleTokenMetadata where
  parseJSON = Aeson.withObject "RoleTokenMetadata" \x ->
    RoleTokenMetadata
      <$> x .: "name"
      <*> (parseJSONURI =<<  x .: "image")
      <*> x .:! "mediaType"
      <*> x .:! "description"
      <*> x .:! "files" .!= []

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
          parseSingleFileDetails = fmap (:[]) . parseNFTMetadataFile
          parseManyFileDetails = parseMetadataList parseNFTMetadataFile
          parseFileDetails md = parseSingleFileDetails md <|> parseManyFileDetails md
          files = fromMaybe [] $ parseFileDetails =<< Map.lookup "files" textKeyMap
      Just $ RoleTokenMetadata {..}

    parseNFTMetadataFile :: Metadata -> Maybe NFTMetadataFile
    parseNFTMetadataFile metadata = do
      textKeyMap <- parseMetadataRecord metadata
      name <- parseMetadataText =<< Map.lookup "name" textKeyMap
      mediaType <- parseMediaType =<< Map.lookup "mediaType" textKeyMap
      src <- Network.parseURI . Text.unpack =<< parseSplittableText =<< Map.lookup "src" textKeyMap
      Just $ NFTMetadataFile {..}

    parseMediaType :: Metadata -> Maybe MediaType
    parseMediaType = fmap (fromString . Text.unpack) . parseMetadataText

    parseMetadataRecord :: Metadata -> Maybe (Map Text Metadata)
    parseMetadataRecord = parseMetadataMap parseMetadataText Just

    parseSplittableText :: Metadata -> Maybe Text
    parseSplittableText md = parseMetadataText md <|> (mconcat <$> parseMetadataList parseMetadataText md)

encodeRoleTokenMetadata :: RoleTokenMetadata -> Metadata
encodeRoleTokenMetadata = encodeNFTMetadataDetails
  where
  encodeNFTMetadataDetails :: RoleTokenMetadata -> Metadata
  encodeNFTMetadataDetails RoleTokenMetadata {..} = MetadataMap $
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

  encodeNFTMetadataFile :: NFTMetadataFile -> Metadata
  encodeNFTMetadataFile NFTMetadataFile {..} = MetadataMap
    [ (MetadataText "name", MetadataText name)
    , (MetadataText "mediaType", encodeMediaType mediaType)
    , (MetadataText "src", encodeText $ fromString $ Network.uriToString id src "")
    ]

  encodeMediaType :: MediaType -> Metadata
  encodeMediaType = MetadataText . Text.pack . show

  encodeText :: Text -> Metadata
  encodeText = MetadataList . fmap MetadataText . Text.chunksOf 64

-- | Non empty mint request.
newtype Mint = Mint { unMint :: Map TokenName (Address, Maybe RoleTokenMetadata) }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Binary, Semigroup, Monoid, ToJSON)

mkMint :: NonEmpty (TokenName, (Address, Maybe RoleTokenMetadata)) -> Mint
mkMint = Mint . Map.fromList . NonEmpty.toList

data RoleTokensConfig
  = RoleTokensNone
  | RoleTokensUsePolicy PolicyId
  | RoleTokensMint Mint
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON)

data ContractCreated era v = ContractCreated
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
  }

deriving instance Show (ContractCreated BabbageEra 'V1)
deriving instance Eq (ContractCreated BabbageEra 'V1)

instance IsCardanoEra era => ToJSON (ContractCreated era 'V1) where
  toJSON ContractCreated{..} = object
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
    ]

instance IsCardanoEra era => Binary (ContractCreated era 'V1) where
  put ContractCreated{..} = do
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
    let version = MarloweV1
    pure ContractCreated{..}

data InputsApplied era v = InputsApplied
  { version :: MarloweVersion v
  , contractId :: ContractId
  , metadata :: MarloweTransactionMetadata
  , input :: TransactionScriptOutput v
  , output :: Maybe (TransactionScriptOutput v)
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , inputs :: Inputs v
  , txBody :: TxBody era
  }

deriving instance Show (InputsApplied BabbageEra 'V1)
deriving instance Eq (InputsApplied BabbageEra 'V1)

instance IsCardanoEra era => Binary (InputsApplied era 'V1) where
  put InputsApplied{..} = do
    put contractId
    put metadata
    put input
    put output
    putUTCTime invalidBefore
    putUTCTime invalidHereafter
    putInputs MarloweV1 inputs
    putTxBody txBody
  get = do
    let version = MarloweV1
    contractId <- get
    metadata <- get
    input <- get
    output <- get
    invalidBefore <- getUTCTime
    invalidHereafter <- getUTCTime
    inputs <- getInputs MarloweV1
    txBody <- getTxBody
    pure InputsApplied{..}

instance IsCardanoEra era => ToJSON (InputsApplied era 'V1) where
  toJSON InputsApplied{..} = object
    [ "contract-id" .= contractId
    , "input" .= input
    , "output" .= output
    , "invalid-before" .= invalidBefore
    , "invalid-hereafter" .= invalidHereafter
    , "inputs" .= inputs
    , "tx-body" .= serialiseToTextEnvelope Nothing txBody
    ]

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
    -> RoleTokensConfig
    -- ^ How to initialize role tokens
    -> MarloweTransactionMetadata
    -- ^ Optional metadata to attach to the transaction
    -> Lovelace
    -- ^ Min Lovelace which should be used for the contract output.
    -> Contract v
    -- ^ The contract to run
    -> MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v)

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
    -> MarloweTxCommand Void (ApplyInputsError v) (InputsApplied BabbageEra v)

  -- | Construct a transaction that withdraws available assets from an active
  -- Marlowe contract for a set of roles in the contract. The resulting,
  -- unsigned transaction can be signed via the cardano API or a wallet
  -- provider. When signed, the 'Submit' command can be used to submit the
  -- transaction to the attached Cardano node.
  Withdraw
    :: MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> ContractId
    -- ^ The ID of the contract to apply the inputs to.
    -> TokenName
    -- ^ The names of the roles whose assets to withdraw.
    -> MarloweTxCommand Void (WithdrawError v)
        ( TxBody BabbageEra -- The unsigned tx body, to be signed by a wallet.
        )

  -- | Submits a signed transaction to the attached Cardano node.
  Submit
    :: Tx BabbageEra
    -- ^ A signed transaction to submit
    -> MarloweTxCommand
        SubmitStatus -- This job reports the status of the tx submission, which can take some time.
        SubmitError
        BlockHeader  -- The block header of the block this transaction was added to.


instance HasSignature MarloweTxCommand where
  signature _ = "MarloweTxCommand"

instance CommandToJSON MarloweTxCommand where
  commandToJSON = \case
    Create mStakeCredential version walletAddresses roles metadata minAda contract -> object
      [ "create" .= object
          [ "stake-credential" .= mStakeCredential
          , "wallet-addresses" .= walletAddresses
          , "roles" .= roles
          , "metadata" .= metadata
          , "min-ada-deposit" .= minAda
          , "contract" .= contractToJSON version contract
          , "version" .= version
          ]
      ]
    ApplyInputs MarloweV1 walletAddresses contractId metadata invalidBefore invalidHereafter redeemer -> object
      [ "apply-inputs" .= object
          [ "wallet-addresses" .= walletAddresses
          , "contract-id" .= contractId
          , "metadata" .= metadata
          , "invalid-before" .= invalidBefore
          , "invalid-hereafter" .= invalidHereafter
          , "redeemer" .= toJSON redeemer
          , "version" .= MarloweV1
          ]
      ]
    Withdraw version walletAddresses contractId tokenName -> object
      [ "withdraw" .= object
          [ "wallet-addresses" .= walletAddresses
          , "contract-id" .= contractId
          , "token-name" .= tokenName
          , "version" .= version
          ]
      ]
    Submit tx -> object [ "submit" .= serialiseToTextEnvelope Nothing tx ]
  jobIdToJSON = \case
    JobIdSubmit i -> object [ "submit" .= i ]
  errToJSON = \case
    TagCreate MarloweV1 -> toJSON
    TagApplyInputs MarloweV1 -> toJSON
    TagWithdraw MarloweV1 -> toJSON
    TagSubmit -> toJSON
  resultToJSON = \case
    TagCreate MarloweV1 -> toJSON
    TagApplyInputs MarloweV1 -> toJSON
    TagWithdraw MarloweV1 -> toJSON . serialiseToTextEnvelope Nothing
    TagSubmit -> toJSON
  statusToJSON = \case
    TagCreate _ -> toJSON
    TagApplyInputs _ -> toJSON
    TagWithdraw _ -> toJSON
    TagSubmit -> toJSON


instance Command MarloweTxCommand where
  data Tag MarloweTxCommand status err result where
    TagCreate :: MarloweVersion v -> Tag MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v)
    TagApplyInputs :: MarloweVersion v -> Tag MarloweTxCommand Void (ApplyInputsError v) (InputsApplied BabbageEra v)
    TagWithdraw :: MarloweVersion v -> Tag MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra)
    TagSubmit :: Tag MarloweTxCommand SubmitStatus SubmitError BlockHeader

  data JobId MarloweTxCommand stats err result where
    JobIdSubmit :: TxId -> JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader

  tagFromCommand = \case
    Create _ version _ _ _ _ _ -> TagCreate version
    ApplyInputs version _ _ _ _ _ _ -> TagApplyInputs version
    Withdraw version _ _ _        -> TagWithdraw version
    Submit _                -> TagSubmit

  tagFromJobId = \case
    JobIdSubmit _ -> TagSubmit

  tagEq t1 t2 = case (t1, t2) of
    (TagCreate MarloweV1, TagCreate MarloweV1) -> pure (Refl, Refl, Refl)
    (TagCreate _, _) -> Nothing
    (TagApplyInputs MarloweV1, TagApplyInputs MarloweV1) -> pure (Refl, Refl, Refl)
    (TagApplyInputs _, _) -> Nothing
    (TagWithdraw MarloweV1, TagWithdraw MarloweV1) -> pure (Refl, Refl, Refl)
    (TagWithdraw _, _) -> Nothing
    (TagSubmit, TagSubmit) -> pure (Refl, Refl, Refl)
    (TagSubmit, _) -> Nothing

  putTag = \case
    TagCreate version -> putWord8 0x01 *> put (SomeMarloweVersion version)
    TagApplyInputs version -> putWord8 0x02 *> put (SomeMarloweVersion version)
    TagWithdraw version -> putWord8 0x03 *> put (SomeMarloweVersion version)
    TagSubmit -> putWord8 0x04

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
      _    -> fail $ "Invalid command tag: " <> show tag

  putJobId = \case
    JobIdSubmit txId -> put txId

  getJobId = \case
    TagCreate _ -> fail "create has no job ID"
    TagApplyInputs _ -> fail "apply inputs has no job ID"
    TagWithdraw _ -> fail "withdraw has no job ID"
    TagSubmit -> JobIdSubmit <$> get

  putCommand = \case
    Create mStakeCredential version walletAddresses roles metadata minAda contract -> do
      put mStakeCredential
      put walletAddresses
      put roles
      put metadata
      put minAda
      putContract version contract
    ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer -> do
      put walletAddresses
      put contractId
      put metadata
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidHereafter
      putInputs version redeemer
    Withdraw _ walletAddresses contractId tokenName -> do
      put walletAddresses
      put contractId
      put tokenName
    Submit tx -> put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate version -> do
      mStakeCredential <- get
      walletAddresses <- get
      roles <- get
      metadata <- get
      minAda <- get
      contract <- getContract version
      pure $ Create mStakeCredential version walletAddresses roles metadata minAda contract

    TagApplyInputs version -> do
      walletAddresses <- get
      contractId <- get
      metadata <- get
      invalidBefore <- getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> getUTCTime
        t -> fail $ "Invalid Maybe tag: " <> show t
      invalidHereafter <- getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> getUTCTime
        t -> fail $ "Invalid Maybe tag: " <> show t
      redeemer <- getInputs version
      pure $ ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer

    TagWithdraw version -> do
      walletAddresses <- get
      contractId <- get
      tokenName <- get
      pure $ Withdraw version walletAddresses contractId tokenName

    TagSubmit -> do
      bytes <- get @ByteString
      Submit <$> case deserialiseFromCBOR (AsTx AsBabbage) bytes of
        Left err -> fail $ show err
        Right tx -> pure tx

  putStatus = \case
    TagCreate _ -> absurd
    TagApplyInputs _ -> absurd
    TagWithdraw _ -> absurd
    TagSubmit -> put

  getStatus = \case
    TagCreate _ -> fail "create has no status"
    TagApplyInputs _ -> fail "apply inputs has no status"
    TagWithdraw _ -> fail "withdraw has no status"
    TagSubmit -> get

  putErr = \case
    TagCreate MarloweV1 -> put
    TagApplyInputs MarloweV1 -> put
    TagWithdraw MarloweV1 -> put
    TagSubmit -> put

  getErr = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw MarloweV1 -> get
    TagSubmit -> get

  putResult = \case
    TagCreate MarloweV1 -> put
    TagApplyInputs MarloweV1 -> put
    TagWithdraw _ -> putTxBody
    TagSubmit -> put

  getResult = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw _ -> getTxBody
    TagSubmit -> get

putTxBody :: IsCardanoEra era => TxBody era -> Put
putTxBody = put . serialiseToCBOR

getTxBody :: forall era. IsCardanoEra era => Get (TxBody era)
getTxBody = do
  bytes <- get @ByteString
  case deserialiseFromCBOR (AsTxBody $ cardanoEraToAsType $ cardanoEra @era) bytes of
    Left err     -> fail $ show err
    Right txBody -> pure txBody

data WalletAddresses = WalletAddresses
  { changeAddress  :: Address
  , extraAddresses :: Set Address
  , collateralUtxos :: Set TxOutRef
  }
  deriving (Eq, Show, Generic, Binary, ToJSON)

-- | Errors that can occur when trying to solve the constraints.
data ConstraintError v
  = MintingUtxoNotFound TxOutRef
  | RoleTokenNotFound AssetId
  | ToCardanoError
  | MissingMarloweInput
  | PayoutInputNotFound (PayoutDatum v)
  | CalculateMinUtxoFailed String
  | CoinSelectionFailed String
  | BalancingError String
  deriving (Generic)

deriving instance Eq (ConstraintError 'V1)
deriving instance Ord (ConstraintError 'V1)
deriving instance Show (ConstraintError 'V1)
deriving instance Binary (ConstraintError 'V1)
deriving instance ToJSON (ConstraintError 'V1)

data CreateError v
  = CreateConstraintError (ConstraintError v)
  | CreateLoadMarloweContextFailed LoadMarloweContextError
  | CreateBuildupFailed CreateBuildupError
  | CreateToCardanoError
  deriving (Generic)

deriving instance Eq (CreateError 'V1)
deriving instance Show (CreateError 'V1)
deriving instance Ord (CreateError 'V1)
instance Binary (CreateError 'V1)
instance ToJSON (CreateError 'V1)

data CreateBuildupError
  = MintingUtxoSelectionFailed
  | AddressDecodingFailed Address
  | MintingScriptDecodingFailed PlutusScript
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, ToJSON)

data ApplyInputsError v
  = ApplyInputsConstraintError (ConstraintError v)
  | ScriptOutputNotFound
  | ApplyInputsLoadMarloweContextFailed LoadMarloweContextError
  | ApplyInputsConstraintsBuildupFailed ApplyInputsConstraintsBuildupError
  | SlotConversionFailed String
  | TipAtGenesis
  | ValidityLowerBoundTooHigh SlotNo SlotNo

deriving instance Eq (ApplyInputsError 'V1)
deriving instance Show (ApplyInputsError 'V1)
deriving instance Generic (ApplyInputsError 'V1)
instance Binary (ApplyInputsError 'V1)
instance ToJSON (ApplyInputsError 'V1)

data ApplyInputsConstraintsBuildupError
  = MarloweComputeTransactionFailed String
  | UnableToDetermineTransactionTimeout
  deriving (Eq, Show, Generic)
  deriving anyclass (Binary, ToJSON)

data WithdrawError v
  = WithdrawConstraintError (ConstraintError v)
  | WithdrawLoadMarloweContextFailed LoadMarloweContextError
  | UnableToFindPayoutForAGivenRole TokenName
  deriving (Generic)

deriving instance Eq (WithdrawError 'V1)
deriving instance Show (WithdrawError 'V1)
instance Binary (WithdrawError 'V1)
instance ToJSON (WithdrawError 'V1)

data LoadMarloweContextError
  = LoadMarloweContextErrorNotFound
  | LoadMarloweContextErrorVersionMismatch SomeMarloweVersion
  | LoadMarloweContextToCardanoError
  | MarloweScriptNotPublished ScriptHash
  | PayoutScriptNotPublished ScriptHash
  | ExtractCreationError ExtractCreationError
  | ExtractMarloweTransactionError ExtractMarloweTransactionError
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass (Binary, ToJSON)

data SubmitError
  = SubmitException String
  | SubmitFailed String -- should be from show TxValidationErrorInMode
  | TxDiscarded
  deriving (Eq, Show, Generic, Binary, ToJSON)

data SubmitStatus
  = Submitting
  | Accepted
  deriving (Eq, Show, Generic, Binary, ToJSON)
