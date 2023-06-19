-- editorconfig-checker-disable-file

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
  , WithdrawTx(..)
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
import GHC.Show (showSpace)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , AssetId
  , Assets
  , BlockHeader
  , DatumHash
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
  , parseMetadataList
  , parseMetadataMap
  , parseMetadataText
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
  , safetyErrors :: [SafetyError]
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
    , "safety-errors" .= safetyErrors
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
    pure InputsApplied{..}

data WithdrawTx era v = WithdrawTx
  { version :: MarloweVersion v
  , inputs :: Map TxOutRef (Payout v)
  , roleToken :: AssetId
  , txBody :: TxBody era
  }

deriving instance Show (WithdrawTx BabbageEra 'V1)
deriving instance Eq (WithdrawTx BabbageEra 'V1)

instance IsCardanoEra era => Binary (WithdrawTx era 'V1) where
  put WithdrawTx{..} = do
    put inputs
    put roleToken
    putTxBody txBody
  get = do
    let version = MarloweV1
    inputs <- get
    roleToken <- get
    txBody <- getTxBody
    pure WithdrawTx{..}

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
    -> Either (Contract v) DatumHash
    -- ^ The contract to run, or the hash of the contract to load from the store.
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
        ( WithdrawTx BabbageEra v -- The unsigned tx body, to be signed by a wallet.
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

instance OTelCommand MarloweTxCommand where
  commandTypeName _ = "marlowe_tx_command"
  commandName = \case
    TagCreate _ -> "create"
    TagApplyInputs _ -> "apply_inputs"
    TagWithdraw _ -> "withdraw"
    TagSubmit -> "submit"

instance Command MarloweTxCommand where
  data Tag MarloweTxCommand status err result where
    TagCreate :: MarloweVersion v -> Tag MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v)
    TagApplyInputs :: MarloweVersion v -> Tag MarloweTxCommand Void (ApplyInputsError v) (InputsApplied BabbageEra v)
    TagWithdraw :: MarloweVersion v -> Tag MarloweTxCommand Void (WithdrawError v) (WithdrawTx BabbageEra v)
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
    Create mStakeCredential MarloweV1 walletAddresses roles metadata minAda contract -> do
      put mStakeCredential
      put walletAddresses
      put roles
      put metadata
      put minAda
      put contract
    ApplyInputs version walletAddresses contractId metadata invalidBefore invalidHereafter redeemer -> do
      put walletAddresses
      put contractId
      put metadata
      maybe (putWord8 0) (\t -> putWord8 1 *> put t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> put t) invalidHereafter
      putInputs version redeemer
    Withdraw _ walletAddresses contractId tokenName -> do
      put walletAddresses
      put contractId
      put tokenName
    Submit tx -> put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate MarloweV1 -> do
      mStakeCredential <- get
      walletAddresses <- get
      roles <- get
      metadata <- get
      minAda <- get
      contract <- get
      pure $ Create mStakeCredential MarloweV1 walletAddresses roles metadata minAda contract

    TagApplyInputs version -> do
      walletAddresses <- get
      contractId <- get
      metadata <- get
      invalidBefore <- getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> get
        t -> fail $ "Invalid Maybe tag: " <> show t
      invalidHereafter <- getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> get
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
    TagWithdraw MarloweV1 -> put
    TagSubmit -> put

  getResult = \case
    TagCreate MarloweV1 -> get
    TagApplyInputs MarloweV1 -> get
    TagWithdraw MarloweV1 -> get
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
  | CreateSafetyAnalysisError String  -- FIXME: This is a placeholder, pending design of error handling for safety analysis.
  | CreateContractNotFound
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

instance CommandEq MarloweTxCommand where
  commandEq = \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract -> \case
      Create stake' MarloweV1 wallet' roleTokenConfig' metadata' minAda' contract' ->
        stake == stake'
          && wallet == wallet'
          && roleTokenConfig == roleTokenConfig'
          && metadata == metadata'
          && minAda == minAda'
          && contract == contract'
    ApplyInputs MarloweV1 wallet contractId metadata invalidBefore invalidHereafter inputs -> \case
      ApplyInputs MarloweV1 wallet' contractId' metadata' invalidBefore' invalidHereafter' inputs' ->
        wallet == wallet'
          && contractId == contractId'
          && metadata == metadata'
          && invalidBefore == invalidBefore'
          && invalidHereafter == invalidHereafter'
          && inputs == inputs'
    Withdraw MarloweV1 wallet contractId role -> \case
      Withdraw MarloweV1 wallet' contractId' role' ->
        wallet == wallet'
          && contractId == contractId'
          && role == role'
    Submit tx -> \case
      Submit tx' -> tx == tx'

  jobIdEq = \case
    JobIdSubmit txId -> \case
      JobIdSubmit txId' -> txId == txId'

  statusEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  errEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

  resultEq = \case
    TagCreate MarloweV1 -> (==)
    TagApplyInputs MarloweV1 -> (==)
    TagWithdraw MarloweV1 -> (==)
    TagSubmit -> (==)

instance ShowCommand MarloweTxCommand where
  showsPrecTag p = \case
    TagCreate MarloweV1 -> showParen (p >= 11)
      ( showString "TagCreate"
      . showSpace
      . showString "MarloweV1"
      )
    TagApplyInputs MarloweV1 -> showParen (p >= 11)
      ( showString "TagApplyInputs"
      . showSpace
      . showString "MarloweV1"
      )
    TagWithdraw MarloweV1 -> showParen (p >= 11)
      ( showString "TagWithdraw"
      . showSpace
      . showString "MarloweV1"
      )
    TagSubmit -> showString "TagSubmit"

  showsPrecCommand p = showParen (p >= 11) . \case
    Create stake MarloweV1 wallet roleTokenConfig metadata minAda contract ->
      ( showString "Create"
      . showSpace
      . showsPrec 11 stake
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 roleTokenConfig
      . showSpace
      . showsPrec 11 metadata
      . showSpace
      . showsPrec 11 minAda
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
    Withdraw MarloweV1 wallet contractId role ->
      ( showString "Withdraw"
      . showSpace
      . showsPrec 11 MarloweV1
      . showSpace
      . showsPrec 11 wallet
      . showSpace
      . showsPrec 11 contractId
      . showSpace
      . showsPrec 11 role
      )
    Submit tx ->
      ( showString "Submit"
      . showSpace
      . showsPrec 11 tx
      )

  showsPrecJobId p = \case
    JobIdSubmit txId -> showParen (p >= 11)
      ( showString "JobIdSubmit"
      . showSpace
      . showsPrec 11 txId
      )

  showsPrecStatus p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecErr p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p

  showsPrecResult p = \case
    TagCreate MarloweV1 -> showsPrec p
    TagApplyInputs MarloweV1 -> showsPrec p
    TagWithdraw MarloweV1 -> showsPrec p
    TagSubmit -> showsPrec p
