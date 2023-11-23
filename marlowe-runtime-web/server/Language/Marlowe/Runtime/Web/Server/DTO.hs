{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the data-transfer object (DTO) translation layer for
-- the web server. DTOs are the types served by the API, which notably include
-- no cardano-api dependencies and have nice JSON representations. This module
-- describes how they are mapped to the internal API types of the runtime.
module Language.Marlowe.Runtime.Web.Server.DTO where

import Cardano.Api (
  AsType (..),
  HasTextEnvelope,
  HasTypeProxy,
  IsCardanoEra (..),
  IsShelleyBasedEra (..),
  NetworkId (..),
  NetworkMagic (..),
  SerialiseAsCBOR,
  ShelleyBasedEra (..),
  TextEnvelope (..),
  TextEnvelopeType (..),
  Tx,
  TxBody,
  deserialiseAddress,
  deserialiseFromCBOR,
  deserialiseFromTextEnvelope,
  getTxId,
  metadataValueToJsonNoSchema,
  proxyToAsType,
  serialiseToCBOR,
  serialiseToTextEnvelope,
 )
import Cardano.Api.Byron (HasTextEnvelope (textEnvelopeType))
import Cardano.Api.Shelley (
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (..),
  ShelleyLedgerEra,
  StakeAddress (..),
  fromShelleyStakeCredential,
 )
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo.Scripts
import qualified Cardano.Ledger.Core as Ledger.Core
import Control.Arrow (Arrow (..))
import Control.Error.Util (hush)
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (Value (..))
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import Data.Word (Word16, Word64)
import GHC.TypeLits (KnownSymbol)
import qualified Language.Marlowe.Core.V1.Semantics as Sem
import Language.Marlowe.Runtime.Core.ScriptRegistry as Tx (HelperScript (..))

import qualified Language.Marlowe.Core.V1.Semantics.Types as Sem

import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32, serialiseAddressBech32)
import Language.Marlowe.Protocol.Query.Types (
  ContractState (..),
  PayoutHeader (..),
  PayoutState (..),
  RuntimeStatus (..),
  SomeContractState (..),
  SomePayoutState (..),
  SomeTransaction (..),
  Withdrawal (..),
 )

import Cardano.Ledger.Alonzo.Core (TxWits)
import Cardano.Ledger.Binary (Annotator, DecCBOR (..), Decoder, decodeFullAnnotator, serialize')
import Cardano.Ledger.Core (EraTxWits, eraProtVerLow)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bitraversable (Bitraversable (..))
import Data.Function (on)
import Data.Kind (Type)
import Data.List (groupBy)
import qualified Data.Map.NonEmpty as NEMap
import Data.Set (Set)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType, fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweMetadata (..),
  MarloweMetadataTag (..),
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (Payout),
  SomeMarloweVersion (..),
  Transaction (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api (Payout (..))
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx (..), TempTxStatus (..))
import Network.HTTP.Media (MediaType, parseAccept)
import Servant.Pagination (IsRangeType)
import qualified Servant.Pagination as Pagination
import Unsafe.Coerce (unsafeCoerce)

-- | A class that states a type has a DTO representation.
class HasDTO a where
  -- | The type used in the API to represent this type.
  type DTO a :: Type

-- | States that a type can be encoded as a DTO.
class (HasDTO a) => ToDTO a where
  toDTO :: a -> DTO a

-- | States that a type can be encoded as a DTO given a tx status.
class (HasDTO a) => ToDTOWithTxStatus a where
  toDTOWithTxStatus :: TempTxStatus -> a -> DTO a

-- | States that a type can be decoded from a DTO.
class (HasDTO a) => FromDTO a where
  fromDTO :: DTO a -> Maybe a

-- | States that a type can be encoded as a DTO given a tx status.
class (HasDTO a) => FromDTOWithTxStatus a where
  fromDTOWithTxStatus :: DTO a -> Maybe (TempTxStatus, a)

fromDTOThrow :: (MonadError e m, FromDTO a) => e -> DTO a -> m a
fromDTOThrow e = maybe (throwError e) pure . fromDTO

instance HasDTO (Map k a) where
  type DTO (Map k a) = Map (DTO k) (DTO a)

instance (FromDTO k, FromDTO a) => FromDTO (Map k a) where
  fromDTO = fmap Map.fromDistinctAscList . traverse (bitraverse fromDTO fromDTO) . Map.toAscList

instance (ToDTO k, ToDTO a) => ToDTO (Map k a) where
  toDTO = Map.mapKeysMonotonic toDTO . fmap toDTO

instance HasDTO (Set a) where
  type DTO (Set a) = Set (DTO a)

instance (FromDTO a) => FromDTO (Set a) where
  fromDTO = fmap Set.fromDistinctAscList . traverse fromDTO . Set.toAscList

instance (ToDTO a) => ToDTO (Set a) where
  toDTO = Set.mapMonotonic toDTO

instance HasDTO [a] where
  type DTO [a] = [DTO a]

instance (FromDTO a) => FromDTO [a] where
  fromDTO = traverse fromDTO

instance (ToDTO a) => ToDTO [a] where
  toDTO = fmap toDTO

instance HasDTO (NonEmpty a) where
  type DTO (NonEmpty a) = NonEmpty (DTO a)

instance (FromDTO a) => FromDTO (NonEmpty a) where
  fromDTO = traverse fromDTO

instance (ToDTO a) => ToDTO (NonEmpty a) where
  toDTO = fmap toDTO

instance HasDTO (a, b) where
  type DTO (a, b) = (DTO a, DTO b)

instance (FromDTO a, FromDTO b) => FromDTO (a, b) where
  fromDTO (a, b) = (,) <$> fromDTO a <*> fromDTO b

instance (ToDTO a, ToDTO b) => ToDTO (a, b) where
  toDTO (a, b) = (toDTO a, toDTO b)

instance HasDTO (Either a b) where
  type DTO (Either a b) = Either (DTO a) (DTO b)

instance (FromDTO a, FromDTO b) => FromDTO (Either a b) where
  fromDTO = either (fmap Left . fromDTO) (fmap Right . fromDTO)

instance (ToDTO a, ToDTO b) => ToDTO (Either a b) where
  toDTO = either (Left . toDTO) (Right . toDTO)

instance HasDTO (Maybe a) where
  type DTO (Maybe a) = Maybe (DTO a)

instance (ToDTO a) => ToDTO (Maybe a) where
  toDTO = fmap toDTO

instance (FromDTO a) => FromDTO (Maybe a) where
  fromDTO = traverse fromDTO

instance HasDTO Chain.Tokens where
  type DTO Chain.Tokens = Web.Tokens

instance ToDTO Chain.Tokens where
  toDTO =
    Web.Tokens
      . fmap Map.fromDistinctAscList
      . Map.fromDistinctAscList
      . fmap ((fst . head) &&& fmap snd)
      . groupBy (on (==) fst)
      . fmap (\(Chain.AssetId policy name, quantity) -> (toDTO policy, (toDTO name, fromIntegral quantity)))
      . Map.toAscList
      . Chain.unTokens

instance FromDTO Chain.Tokens where
  fromDTO =
    fmap (Chain.Tokens . Map.fromAscList . concat)
      . traverse
        ( \(policy, tokens) -> for tokens \(name, quantity) ->
            (,fromIntegral quantity) <$> (Chain.AssetId <$> fromDTO policy <*> fromDTO name)
        )
      . Map.toAscList
      . fmap Map.toAscList
      . Web.unTokens

instance HasDTO Chain.Quantity where
  type DTO Chain.Quantity = Word64

instance ToDTO Chain.Quantity where
  toDTO = Chain.unQuantity

instance FromDTO Chain.Quantity where
  fromDTO = pure . Chain.Quantity

instance HasDTO Chain.Assets where
  type DTO Chain.Assets = Web.Assets

instance ToDTO Chain.Assets where
  toDTO Chain.Assets{..} =
    Web.Assets
      { lovelace = fromIntegral ada
      , tokens = toDTO tokens
      }

instance FromDTO Chain.Assets where
  fromDTO Web.Assets{..} =
    Chain.Assets (fromIntegral lovelace) <$> fromDTO tokens

instance HasDTO Discovery.ContractHeader where
  type DTO Discovery.ContractHeader = Web.ContractHeader

instance ToDTO Discovery.ContractHeader where
  toDTO Discovery.ContractHeader{..} =
    Web.ContractHeader
      { contractId = toDTO contractId
      , roleTokenMintingPolicyId = toDTO rolesCurrency
      , version = toDTO marloweVersion
      , tags = foldMap fst $ toDTO $ marloweMetadata metadata
      , continuations = snd =<< toDTO (marloweMetadata metadata)
      , metadata = toDTO $ transactionMetadata metadata
      , status = Web.Confirmed
      , block = Just $ toDTO blockHeader
      }

instance HasDTO Chain.BlockHeader where
  type DTO Chain.BlockHeader = Web.BlockHeader

instance ToDTO Chain.BlockHeader where
  toDTO Chain.BlockHeader{..} =
    Web.BlockHeader
      { slotNo = toDTO slotNo
      , blockNo = toDTO blockNo
      , blockHeaderHash = toDTO headerHash
      }

instance HasDTO ContractId where
  type DTO ContractId = Web.TxOutRef

instance ToDTO ContractId where
  toDTO = toDTO . unContractId

instance FromDTO ContractId where
  fromDTO = fmap ContractId . fromDTO

instance HasDTO SomeMarloweVersion where
  type DTO SomeMarloweVersion = Web.MarloweVersion

instance ToDTO SomeMarloweVersion where
  toDTO (SomeMarloweVersion MarloweV1) = Web.V1

instance FromDTO SomeMarloweVersion where
  fromDTO Web.V1 = pure $ SomeMarloweVersion MarloweV1

instance HasDTO Chain.TxOutRef where
  type DTO Chain.TxOutRef = Web.TxOutRef

instance ToDTO Chain.TxOutRef where
  toDTO Chain.TxOutRef{..} =
    Web.TxOutRef
      { txId = toDTO txId
      , txIx = toDTO txIx
      }

instance FromDTO Chain.TxOutRef where
  fromDTO Web.TxOutRef{..} =
    Chain.TxOutRef
      <$> fromDTO txId
      <*> fromDTO txIx

instance HasDTO Chain.AssetId where
  type DTO Chain.AssetId = Web.AssetId

instance ToDTO Chain.AssetId where
  toDTO Chain.AssetId{..} =
    Web.AssetId
      { policyId = toDTO policyId
      , assetName = toDTO tokenName
      }

instance FromDTO Chain.AssetId where
  fromDTO Web.AssetId{..} =
    Chain.AssetId
      <$> fromDTO policyId
      <*> fromDTO assetName

instance HasDTO Chain.TxId where
  type DTO Chain.TxId = Web.TxId

instance ToDTO Chain.TxId where
  toDTO = coerce

instance FromDTO Chain.TxId where
  fromDTO = pure . coerce

instance HasDTO Chain.PolicyId where
  type DTO Chain.PolicyId = Web.PolicyId

instance ToDTO Chain.PolicyId where
  toDTO = coerce

instance FromDTO Chain.PolicyId where
  fromDTO = Just . coerce

instance HasDTO Chain.TokenName where
  type DTO Chain.TokenName = Text

instance ToDTO Chain.TokenName where
  toDTO = T.pack . read . show . Chain.unTokenName

instance FromDTO Chain.TokenName where
  fromDTO = Just . Chain.TokenName . fromString . T.unpack

instance HasDTO Chain.TxIx where
  type DTO Chain.TxIx = Word16

instance ToDTO Chain.TxIx where
  toDTO = coerce

instance FromDTO Chain.TxIx where
  fromDTO = pure . coerce

instance HasDTO Word64 where
  type DTO Word64 = Word64

instance ToDTO Word64 where
  toDTO = id

instance FromDTO Word64 where
  fromDTO = pure

instance HasDTO Chain.Metadata where
  type DTO Chain.Metadata = Web.Metadata

instance ToDTO Chain.Metadata where
  toDTO = Web.Metadata . metadataValueToJsonNoSchema . Chain.toCardanoMetadata

instance FromDTO Chain.Metadata where
  fromDTO = Chain.fromJSONEncodedMetadata . Web.unMetadata

instance HasDTO Chain.TransactionMetadata where
  type DTO Chain.TransactionMetadata = Map Word64 Web.Metadata

instance ToDTO Chain.TransactionMetadata where
  toDTO = toDTO . Chain.unTransactionMetadata

instance FromDTO Chain.TransactionMetadata where
  fromDTO = fmap Chain.TransactionMetadata . fromDTO

instance HasDTO MarloweMetadata where
  type DTO MarloweMetadata = (Map Text Web.Metadata, Maybe Text)

instance ToDTO MarloweMetadata where
  toDTO MarloweMetadata{..} =
    ( Map.mapKeys getMarloweMetadataTag $ maybe (Web.Metadata Null) toDTO <$> tags
    , continuations
    )

instance FromDTO MarloweMetadata where
  fromDTO (tags, continuations) =
    MarloweMetadata
      <$> ( Map.mapKeys MarloweMetadataTag <$> for tags \case
              Web.Metadata Null -> pure Nothing
              m -> Just <$> fromDTO m
          )
      <*> pure continuations

instance HasDTO Chain.SlotNo where
  type DTO Chain.SlotNo = Word64

instance ToDTO Chain.SlotNo where
  toDTO = coerce

instance HasDTO Chain.BlockNo where
  type DTO Chain.BlockNo = Word64

instance ToDTO Chain.BlockNo where
  toDTO = coerce

instance HasDTO Chain.BlockHeaderHash where
  type DTO Chain.BlockHeaderHash = Web.Base16

instance ToDTO Chain.BlockHeaderHash where
  toDTO = coerce

instance HasDTO PayoutHeader where
  type DTO PayoutHeader = Web.PayoutHeader

instance ToDTO PayoutHeader where
  toDTO PayoutHeader{..} =
    Web.PayoutHeader
      { contractId = toDTO contractId
      , payoutId = toDTO payoutId
      , withdrawalId = toDTO withdrawalId
      , role = toDTO role
      , status = maybe Web.Available (const Web.Withdrawn) withdrawalId
      }

instance HasDTO Withdrawal where
  type DTO Withdrawal = Web.Withdrawal

instance ToDTO Withdrawal where
  toDTO Withdrawal{..} =
    Web.Withdrawal
      { payouts = Set.fromList $ toDTO $ Map.elems withdrawnPayouts
      , withdrawalId = toDTO withdrawalTx
      , status = Web.Confirmed
      , block = Just $ toDTO block
      }

instance HasDTO SomeContractState where
  type DTO SomeContractState = Web.ContractState

instance ToDTO SomeContractState where
  toDTO (SomeContractState MarloweV1 ContractState{..}) =
    Web.ContractState
      { contractId = toDTO contractId
      , roleTokenMintingPolicyId = toDTO roleTokenMintingPolicyId
      , version = Web.V1
      , tags = foldMap fst $ toDTO $ marloweMetadata metadata
      , continuations = snd =<< toDTO (marloweMetadata metadata)
      , metadata = toDTO $ transactionMetadata metadata
      , status = Web.Confirmed
      , block = Just $ toDTO initialBlock
      , initialContract = Sem.marloweContract $ datum initialOutput
      , currentContract = Sem.marloweContract . datum <$> latestOutput
      , state = Sem.marloweState . datum <$> latestOutput
      , assets = maybe emptyAssets (\Core.TransactionScriptOutput{..} -> toDTO assets) latestOutput
      , utxo = toDTO . utxo <$> latestOutput
      , txBody = Nothing
      , unclaimedPayouts =
          (\(payoutId, Core.Api.Payout{..}) -> Web.Payout (toDTO payoutId) (toDTO . tokenName $ datum) (toDTO assets))
            <$> M.toList unclaimedPayouts
      }

instance HasDTO SomePayoutState where
  type DTO SomePayoutState = Web.PayoutState

instance ToDTO SomePayoutState where
  toDTO (SomePayoutState MarloweV1 PayoutState{..}) = case payout of
    Payout address assets role ->
      Web.PayoutState
        { contractId = toDTO contractId
        , payoutId = toDTO payoutId
        , withdrawalId = toDTO withdrawalId
        , role = toDTO role
        , payoutValidatorAddress = toDTO address
        , assets = toDTO assets
        , status = maybe Web.Available (const Web.Withdrawn) withdrawalId
        }

instance HasDTO SomeTransaction where
  type DTO SomeTransaction = Web.Tx

instance ToDTO SomeTransaction where
  toDTO SomeTransaction{..} =
    Web.Tx
      { contractId = toDTO contractId
      , transactionId = toDTO transactionId
      , continuations = snd =<< toDTO (marloweMetadata metadata)
      , tags = foldMap fst $ toDTO $ marloweMetadata metadata
      , metadata = toDTO $ transactionMetadata metadata
      , status = Web.Confirmed
      , block = Just $ toDTO blockHeader
      , inputUtxo = toDTO input
      , inputs = case version of
          MarloweV1 -> inputs
      , outputUtxo = toDTO $ utxo <$> scriptOutput
      , outputContract = case version of
          MarloweV1 -> Sem.marloweContract . datum <$> scriptOutput
      , outputState = case version of
          MarloweV1 -> Sem.marloweState . datum <$> scriptOutput
      , assets = maybe emptyAssets (\Core.TransactionScriptOutput{..} -> toDTO assets) scriptOutput
      , payouts = case version of
          MarloweV1 ->
            (\(payoutId, Core.Api.Payout{..}) -> Web.Payout (toDTO payoutId) (toDTO . tokenName $ datum) (toDTO assets))
              <$> M.toList payouts
      , consumingTx = toDTO consumedBy
      , invalidBefore = validityLowerBound
      , invalidHereafter = validityUpperBound
      , txBody = Nothing
      }
    where
      Transaction{..} = transaction
      TransactionOutput{..} = output

instance HasDTO TempTxStatus where
  type DTO TempTxStatus = Web.TxStatus

instance ToDTO TempTxStatus where
  toDTO Unsigned = Web.Unsigned
  toDTO Submitted = Web.Submitted

instance FromDTO TempTxStatus where
  fromDTO Web.Unsigned = Just Unsigned
  fromDTO Web.Submitted = Just Submitted
  fromDTO _ = Nothing

instance HasDTO (TempTx Tx.ContractCreatedInEra) where
  type DTO (TempTx Tx.ContractCreatedInEra) = Web.ContractState

instance ToDTO (TempTx Tx.ContractCreatedInEra) where
  toDTO (TempTx era _ status tx) = toDTOWithTxStatus status $ Tx.ContractCreated era tx

instance HasDTO (TempTx Tx.InputsAppliedInEra) where
  type DTO (TempTx Tx.InputsAppliedInEra) = Web.Tx

instance ToDTO (TempTx Tx.InputsAppliedInEra) where
  toDTO (TempTx era _ status tx) = toDTOWithTxStatus status $ Tx.InputsApplied era tx

instance HasDTO (TempTx Tx.WithdrawTxInEra) where
  type DTO (TempTx Tx.WithdrawTxInEra) = Web.Withdrawal

instance ToDTO (TempTx Tx.WithdrawTxInEra) where
  toDTO (TempTx era _ status tx) = toDTOWithTxStatus status $ Tx.WithdrawTx era tx

instance HasDTO (Tx.ContractCreated v) where
  type DTO (Tx.ContractCreated v) = Web.ContractState

instance HasDTO (Tx.WithdrawTx v) where
  type DTO (Tx.WithdrawTx v) = Web.Withdrawal

instance ToDTOWithTxStatus (Tx.WithdrawTx v) where
  toDTOWithTxStatus status (Tx.WithdrawTx _ Tx.WithdrawTxInEra{txBody}) =
    Web.Withdrawal
      { withdrawalId = toDTO $ fromCardanoTxId $ getTxId txBody
      , payouts = mempty -- TODO the information cannot be recovered here. Push creating Withdrawn to marlowe-tx.
      , status = toDTO status
      , block = Nothing
      }

instance ToDTOWithTxStatus (Tx.ContractCreated v) where
  toDTOWithTxStatus status (Tx.ContractCreated era Tx.ContractCreatedInEra{..}) =
    Web.ContractState
      { contractId = toDTO contractId
      , roleTokenMintingPolicyId = toDTO rolesCurrency
      , version = case version of
          MarloweV1 -> Web.V1
      , tags = foldMap fst $ toDTO $ marloweMetadata metadata
      , continuations = snd =<< toDTO (marloweMetadata metadata)
      , metadata = toDTO $ transactionMetadata metadata
      , status = toDTO status
      , block = Nothing
      , initialContract = case version of
          MarloweV1 -> Sem.marloweContract datum
      , currentContract = case version of
          MarloweV1 -> Just $ Sem.marloweContract datum
      , state = case version of
          MarloweV1 -> Just $ Sem.marloweState datum
      , assets = toDTO assets
      , utxo = Nothing
      , txBody = case status of
          Unsigned -> Just case era of
            ReferenceTxInsScriptsInlineDatumsInBabbageEra -> toDTO txBody
            ReferenceTxInsScriptsInlineDatumsInConwayEra -> toDTO txBody
          Submitted -> Nothing
      , unclaimedPayouts = []
      }

instance HasDTO (Tx.InputsApplied v) where
  type DTO (Tx.InputsApplied v) = Web.Tx

instance ToDTOWithTxStatus (Tx.InputsApplied v) where
  toDTOWithTxStatus status (Tx.InputsApplied era Tx.InputsAppliedInEra{..}) =
    Web.Tx
      { contractId = toDTO contractId
      , transactionId = toDTO $ fromCardanoTxId $ getTxId txBody
      , continuations = snd =<< toDTO (marloweMetadata metadata)
      , tags = foldMap fst $ toDTO $ marloweMetadata metadata
      , metadata = toDTO $ transactionMetadata metadata
      , status = toDTO status
      , block = Nothing
      , inputUtxo = toDTO $ utxo input
      , inputs = case version of
          MarloweV1 -> inputs
      , outputUtxo = toDTO $ utxo <$> scriptOutput output
      , outputContract = case version of
          MarloweV1 -> Sem.marloweContract . datum <$> scriptOutput output
      , outputState = case version of
          MarloweV1 -> Sem.marloweState . datum <$> scriptOutput output
      , assets = maybe emptyAssets (\Core.TransactionScriptOutput{..} -> toDTO assets) $ scriptOutput output
      , consumingTx = Nothing
      , invalidBefore = invalidBefore
      , invalidHereafter = invalidHereafter
      , payouts = case version of
          MarloweV1 ->
            (\(payoutId, Core.Api.Payout{..}) -> Web.Payout (toDTO payoutId) (toDTO . tokenName $ datum) (toDTO assets))
              <$> M.toList (payouts output)
      , txBody = case status of
          Unsigned -> Just case era of
            ReferenceTxInsScriptsInlineDatumsInBabbageEra -> toDTO txBody
            ReferenceTxInsScriptsInlineDatumsInConwayEra -> toDTO txBody
          Submitted -> Nothing
      }

emptyAssets :: Web.Assets
emptyAssets = Web.Assets 0 $ Web.Tokens mempty

instance HasDTO (Transaction 'V1) where
  type DTO (Transaction 'V1) = Web.TxHeader

instance ToDTO (Transaction 'V1) where
  toDTO Transaction{..} =
    Web.TxHeader
      { contractId = toDTO contractId
      , transactionId = toDTO transactionId
      , continuations = snd =<< toDTO (marloweMetadata metadata)
      , tags = foldMap fst $ toDTO $ marloweMetadata metadata
      , metadata = toDTO $ transactionMetadata metadata
      , status = Web.Confirmed
      , block = Just $ toDTO blockHeader
      , utxo = toDTO . utxo <$> scriptOutput output
      }

instance HasDTO Chain.Address where
  type DTO Chain.Address = Web.Address

instance ToDTO Chain.Address where
  toDTO address = Web.Address $ fromMaybe (T.pack $ read $ show address) $ Chain.toBech32 address

instance FromDTO Chain.Address where
  fromDTO = Chain.fromBech32 . Web.unAddress

instance HasDTO Chain.StakeCredential where
  type DTO Chain.StakeCredential = Web.StakeAddress

-- Note that `instance ToDTO Chain.StakeCredential` is not possible because the stake
-- credential does not contain the network information needed for the stake address.

instance FromDTO Chain.StakeCredential where
  fromDTO =
    fmap (\(StakeAddress _ credential) -> Chain.fromCardanoStakeCredential $ fromShelleyStakeCredential credential)
      . deserialiseAddress AsStakeAddress
      . Web.unStakeAddress

instance HasDTO (TxBody era) where
  type DTO (TxBody era) = Web.TextEnvelope

instance (IsCardanoEra era) => ToDTO (TxBody era) where
  toDTO = toDTO . serialiseToTextEnvelope Nothing

instance (IsCardanoEra era) => FromDTO (TxBody era) where
  fromDTO = hush . deserialiseFromTextEnvelope asType <=< fromDTO
    where
      asType = AsTxBody $ cardanoEraToAsType $ cardanoEra @era

instance HasDTO (Tx era) where
  type DTO (Tx era) = Web.TextEnvelope

instance (IsCardanoEra era) => ToDTO (Tx era) where
  toDTO = toDTO . serialiseToTextEnvelope Nothing

instance (IsCardanoEra era) => FromDTO (Tx era) where
  fromDTO = hush . deserialiseFromTextEnvelope asType <=< fromDTO
    where
      asType = AsTx $ cardanoEraToAsType $ cardanoEra @era

newtype ShelleyTxWitness era = ShelleyTxWitness (TxWits (ShelleyLedgerEra era))

instance (HasTypeProxy era) => HasTypeProxy (ShelleyTxWitness era) where
  data AsType (ShelleyTxWitness era) = AsShelleyTxWitness (AsType era)
  proxyToAsType _ = AsShelleyTxWitness (proxyToAsType (Proxy :: Proxy era))

instance
  ( HasTypeProxy era
  , EraTxWits (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.AlonzoScript (ShelleyLedgerEra era)
  )
  => SerialiseAsCBOR (ShelleyTxWitness era)
  where
  serialiseToCBOR (ShelleyTxWitness wit) = serialize' (eraProtVerLow @(ShelleyLedgerEra era)) wit

  deserialiseFromCBOR _ bs = do
    let lbs = BSL.fromStrict bs

        annotator :: forall s. Decoder s (Annotator (TxWits (ShelleyLedgerEra era)))
        annotator = decCBOR

    (w :: TxWits (ShelleyLedgerEra era)) <-
      decodeFullAnnotator (eraProtVerLow @(ShelleyLedgerEra era)) "Shelley Tx Witness" annotator lbs
    pure $ ShelleyTxWitness w

instance
  ( IsShelleyBasedEra era
  , EraTxWits (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.AlonzoScript (ShelleyLedgerEra era)
  )
  => HasTextEnvelope (ShelleyTxWitness era)
  where
  textEnvelopeType _ = do
    "ShelleyTxWitness " <> case shelleyBasedEra :: ShelleyBasedEra era of
      ShelleyBasedEraAlonzo -> "AlonzoEra"
      ShelleyBasedEraBabbage -> "BabbageEra"
      ShelleyBasedEraConway -> "ConwayEra"

instance HasDTO (ShelleyTxWitness era) where
  type DTO (ShelleyTxWitness era) = Web.TextEnvelope

instance
  ( IsShelleyBasedEra era
  , EraTxWits (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.AlonzoScript (ShelleyLedgerEra era)
  )
  => ToDTO (ShelleyTxWitness era)
  where
  toDTO = toDTO . serialiseToTextEnvelope Nothing

instance
  ( IsShelleyBasedEra era
  , EraTxWits (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.AlonzoScript (ShelleyLedgerEra era)
  )
  => FromDTO (ShelleyTxWitness era)
  where
  fromDTO = hush . deserialiseFromTextEnvelope asType <=< fromDTO
    where
      eraAsType = cardanoEraToAsType $ cardanoEra @era
      asType = AsShelleyTxWitness eraAsType

instance HasDTO TextEnvelope where
  type DTO TextEnvelope = Web.TextEnvelope

instance ToDTO TextEnvelope where
  toDTO
    TextEnvelope
      { teType = TextEnvelopeType teType
      , teDescription
      , teRawCBOR
      } =
      Web.TextEnvelope
        { teType = T.pack teType
        , teDescription = T.pack $ unsafeCoerce teDescription
        , teCborHex = Web.Base16 teRawCBOR
        }

instance FromDTO TextEnvelope where
  fromDTO
    Web.TextEnvelope
      { teType
      , teDescription
      , teCborHex
      } =
      Just
        TextEnvelope
          { teType = TextEnvelopeType $ T.unpack teType
          , teDescription = fromString $ T.unpack teDescription
          , teRawCBOR = Web.unBase16 teCborHex
          }

instance HasDTO Tx.RoleTokensConfig where
  type DTO Tx.RoleTokensConfig = Maybe Web.RolesConfig

instance FromDTO Tx.RoleTokensConfig where
  fromDTO = \case
    Nothing -> pure Tx.RoleTokensNone
    Just (Web.UsePolicy policy) -> Tx.RoleTokensUsePolicy <$> fromDTO policy <*> pure mempty
    Just (Web.UsePolicyWithOpenRoles policy openRoleNames) ->
      Tx.RoleTokensUsePolicy
        <$> fromDTO policy
        <*> (Map.fromList . fmap (,Map.singleton (Tx.ToScript OpenRoleScript) 1) <$> fromDTO openRoleNames)
    Just (Web.Mint mint) -> Tx.RoleTokensMint <$> fromDTO mint

instance HasDTO Tx.Mint where
  type DTO Tx.Mint = Map Text Web.RoleTokenConfig

instance FromDTO Tx.Mint where
  fromDTO = fmap Tx.Mint . NEMap.nonEmptyMap <=< fromDTO

instance HasDTO Tx.Destination where
  type DTO Tx.Destination = Web.RoleTokenRecipient

instance FromDTO Tx.Destination where
  fromDTO = \case
    Web.ClosedRole addr -> Tx.ToAddress <$> fromDTO addr
    Web.OpenRole -> pure $ Tx.ToScript Tx.OpenRoleScript

instance HasDTO Tx.MintRole where
  type DTO Tx.MintRole = Web.RoleTokenConfig

instance FromDTO Tx.MintRole where
  fromDTO (Web.RoleTokenConfig recipients metadata) = do
    recipients' <- NEMap.nonEmptyMap =<< fromDTO recipients
    Tx.MintRole <$> fromDTO metadata <*> pure recipients'

instance HasDTO Tx.RoleTokenMetadata where
  type DTO Tx.RoleTokenMetadata = Web.TokenMetadata

instance FromDTO Tx.RoleTokenMetadata where
  fromDTO Web.TokenMetadata{..} =
    Tx.RoleTokenMetadata name image
      <$> fromDTO mediaType
      <*> pure description
      <*> case files of
        Nothing -> pure []
        Just files' -> fromDTO files'
      <*> traverse fromDTO (Map.mapKeys Key.toText $ KeyMap.toMap $ Web.Metadata <$> additionalProps)

instance HasDTO MediaType where
  type DTO MediaType = Text

instance FromDTO MediaType where
  fromDTO = parseAccept . encodeUtf8

instance HasDTO Tx.NFTMetadataFile where
  type DTO Tx.NFTMetadataFile = Web.TokenMetadataFile

instance FromDTO Tx.NFTMetadataFile where
  fromDTO Web.TokenMetadataFile{..} =
    Tx.NFTMetadataFile name <$> fromDTO mediaType <*> pure src

instance HasDTO Query.Order where
  type DTO Query.Order = Pagination.RangeOrder

instance ToDTO Query.Order where
  toDTO = \case
    Query.Ascending -> Pagination.RangeAsc
    Query.Descending -> Pagination.RangeDesc

instance FromDTO Query.Order where
  fromDTO =
    pure . \case
      Pagination.RangeAsc -> Query.Ascending
      Pagination.RangeDesc -> Query.Descending

toPaginationRange
  :: (KnownSymbol field, ToDTO a, IsRangeType (DTO a))
  => Query.Range a
  -> Pagination.Range field (DTO a)
toPaginationRange Query.Range{..} =
  Pagination.Range
    { rangeValue = toDTO rangeStart
    , rangeOrder = toDTO rangeDirection
    , rangeField = Proxy
    , ..
    }

fromPaginationRange
  :: (FromDTO a)
  => Pagination.Range field (DTO a)
  -> Maybe (Query.Range a)
fromPaginationRange Pagination.Range{..} = do
  rangeStart <- fromDTO rangeValue
  rangeDirection <- fromDTO rangeOrder
  pure Query.Range{rangeStart, rangeDirection, ..}

tokenNameToText :: Text -> Chain.TokenName
tokenNameToText = Chain.TokenName . fromString . T.unpack

toNonEmpty :: [a] -> Maybe (NonEmpty a)
toNonEmpty [] = Nothing
toNonEmpty (a : as) = Just $ a :| as

instance HasDTO RuntimeStatus where
  type DTO RuntimeStatus = Web.RuntimeStatus

instance ToDTO RuntimeStatus where
  toDTO RuntimeStatus{..} =
    Web.RuntimeStatus
      { nodeTip = case nodeTip of
          Chain.Genesis -> Web.ChainTipGenesis nodeTipUTC
          Chain.At blockHeader -> Web.ChainTip (toDTO blockHeader) nodeTipUTC
      , runtimeChainTip = case runtimeChainTip of
          Chain.Genesis -> Web.ChainTipGenesis runtimeChainTipUTC
          Chain.At blockHeader -> Web.ChainTip (toDTO blockHeader) runtimeChainTipUTC
      , runtimeTip = case runtimeTip of
          Chain.Genesis -> Web.ChainTipGenesis runtimeTipUTC
          Chain.At blockHeader -> Web.ChainTip (toDTO blockHeader) runtimeTipUTC
      , networkId = case networkId of
          Mainnet -> Web.Mainnet
          Testnet (NetworkMagic n) -> Web.Testnet n
      , ..
      }

instance HasDTO Sem.Party where
  type DTO Sem.Party = Web.Party

instance ToDTO Sem.Party where
  toDTO (Sem.Address networkId address) = Web.Party . serialiseAddressBech32 networkId $ address
  toDTO (Sem.Role tokenName) = Web.Party . T.pack . read . show . Sem.unTokenName $ tokenName

instance FromDTO Sem.Party where
  fromDTO a = case deserialiseAddressBech32 (Web.unParty a) of
    Just (network, address) -> Just $ Sem.Address network address
    Nothing -> Just $ Sem.Role $ fromString . T.unpack . Web.unParty $ a
