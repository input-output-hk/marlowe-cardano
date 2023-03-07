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

module Language.Marlowe.Runtime.Web.Server.DTO
  where

import Cardano.Api
  ( AsType(AsTx, AsTxBody)
  , HasTextEnvelope
  , HasTypeProxy
  , IsCardanoEra(cardanoEra)
  , IsShelleyBasedEra(shelleyBasedEra)
  , SerialiseAsCBOR
  , ShelleyBasedEra(ShelleyBasedEraAlonzo, ShelleyBasedEraBabbage)
  , TextEnvelope(..)
  , TextEnvelopeType(..)
  , Tx
  , TxBody
  , deserialiseFromCBOR
  , deserialiseFromTextEnvelope
  , getTxId
  , metadataValueToJsonNoSchema
  , proxyToAsType
  , serialiseToCBOR
  , serialiseToTextEnvelope
  )
import Cardano.Api.Byron (HasTextEnvelope(textEnvelopeType))
import Cardano.Api.SerialiseTextEnvelope (TextEnvelopeDescr(..))
import Cardano.Api.Shelley (ShelleyLedgerEra)
import qualified Cardano.Binary as Binary
import qualified Cardano.Ledger.Alonzo.Scripts as Ledger.Alonzo.Scripts
import Cardano.Ledger.Alonzo.TxWitness (TxWitness)
import qualified Cardano.Ledger.Core as Ledger.Core
import Cardano.Ledger.Era (ValidateScript)
import Control.Arrow (second)
import Control.Error.Util (hush)
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (Value(..))
import Data.Bifunctor (bimap)
import qualified Data.ByteString.Lazy as BSL
import Data.Coerce (coerce)
import Data.Data (Typeable)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Traversable (for)
import Data.Word (Word16, Word64)
import GHC.TypeLits (KnownSymbol)
import qualified Language.Marlowe.Core.V1.Semantics as Sem
import Language.Marlowe.Protocol.Query.Types
  (ContractState(..), PayoutRef(..), SomeContractState(..), SomeTransaction(..), Withdrawal(..))
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType, fromCardanoTxId)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweMetadata(..)
  , MarloweMetadataTag(..)
  , MarloweTransactionMetadata(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , SomeMarloweVersion(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  )
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.TxClient (TempTx(..), TempTxStatus(..), Withdrawn(..))
import Network.HTTP.Media (MediaType, parseAccept)
import Servant.Pagination (IsRangeType)
import qualified Servant.Pagination as Pagination

-- | A class that states a type has a DTO representation.
class HasDTO a where
  -- | The type used in the API to represent this type.
  type DTO a :: *

-- | States that a type can be encoded as a DTO.
class HasDTO a => ToDTO a where
  toDTO :: a -> DTO a

-- | States that a type can be encoded as a DTO given a tx status.
class HasDTO a => ToDTOWithTxStatus a where
  toDTOWithTxStatus :: TempTxStatus -> a -> DTO a

-- | States that a type can be decoded from a DTO.
class HasDTO a => FromDTO a where
  fromDTO :: DTO a -> Maybe a

-- | States that a type can be encoded as a DTO given a tx status.
class HasDTO a => FromDTOWithTxStatus a where
  fromDTOWithTxStatus :: DTO a -> Maybe (TempTxStatus, a)

fromDTOThrow :: (MonadError e m, FromDTO a) => e -> DTO a -> m a
fromDTOThrow e = maybe (throwError e) pure . fromDTO

instance HasDTO (Map k a) where
  type DTO (Map k a) = Map k (DTO a)

instance FromDTO a => FromDTO (Map k a) where
  fromDTO = traverse fromDTO

instance ToDTO a => ToDTO (Map k a) where
  toDTO = fmap toDTO

instance HasDTO [a] where
  type DTO [a] = [DTO a]

instance FromDTO a => FromDTO [a] where
  fromDTO = traverse fromDTO

instance ToDTO a => ToDTO [a] where
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

instance ToDTO a => ToDTO (Maybe a) where
  toDTO = fmap toDTO

instance FromDTO a => FromDTO (Maybe a) where
  fromDTO = traverse fromDTO

instance HasDTO Discovery.ContractHeader where
  type DTO Discovery.ContractHeader = Web.ContractHeader

instance ToDTO Discovery.ContractHeader where
  toDTO Discovery.ContractHeader{..} = Web.ContractHeader
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
  toDTO Chain.BlockHeader{..} = Web.BlockHeader
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
  toDTO Chain.TxOutRef{..} = Web.TxOutRef
    { txId = toDTO txId
    , txIx = toDTO txIx
    }

instance FromDTO Chain.TxOutRef where
  fromDTO Web.TxOutRef{..} = Chain.TxOutRef
    <$> fromDTO txId
    <*> fromDTO txIx

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
  fromDTO (tags, continuations) = MarloweMetadata
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

instance HasDTO PayoutRef where
  type DTO PayoutRef = Web.PayoutRef

instance ToDTO PayoutRef where
  toDTO PayoutRef{..} = Web.PayoutRef
    { contractId = toDTO contractId
    , payout = toDTO payout
    , roleTokenMintingPolicyId = toDTO rolesCurrency
    , role = toDTO role
    }

instance HasDTO Withdrawal where
  type DTO Withdrawal = Web.Withdrawal

instance ToDTO Withdrawal where
  toDTO Withdrawal{..} = Web.Withdrawal
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
      , utxo = toDTO . utxo <$> latestOutput
      , txBody = Nothing
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

instance HasDTO (TempTx Tx.ContractCreated) where
  type DTO (TempTx Tx.ContractCreated) = Web.ContractState

instance ToDTO (TempTx Tx.ContractCreated) where
  toDTO (TempTx _ status tx) = toDTOWithTxStatus status tx

instance HasDTO (TempTx Tx.InputsApplied) where
  type DTO (TempTx Tx.InputsApplied) = Web.Tx

instance ToDTO (TempTx Tx.InputsApplied) where
  toDTO (TempTx _ status tx) = toDTOWithTxStatus status tx

instance HasDTO (TempTx Withdrawn) where
  type DTO (TempTx Withdrawn) = Web.Withdrawal

instance ToDTO (TempTx Withdrawn) where
  toDTO (TempTx _ status tx) = toDTOWithTxStatus status tx

instance HasDTO (Tx.ContractCreated era v) where
  type DTO (Tx.ContractCreated era v) = Web.ContractState

instance HasDTO (Withdrawn era v) where
  type DTO (Withdrawn era v) = Web.Withdrawal

instance ToDTOWithTxStatus (Withdrawn era v) where
  toDTOWithTxStatus status (Withdrawn txBody) =
    Web.Withdrawal
      { withdrawalId = toDTO $ fromCardanoTxId $ getTxId txBody
      , payouts = mempty -- TODO the information cannot be recovered here. Push creating Withdrawn to marlowe-tx.
      , status = toDTO status
      , block = Nothing
      }

instance IsCardanoEra era => ToDTOWithTxStatus (Tx.ContractCreated era v) where
  toDTOWithTxStatus status Tx.ContractCreated{..} =
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
      , utxo = Nothing
      , txBody = case status of
          Unsigned -> Just $ toDTO txBody
          Submitted -> Nothing
      }

instance HasDTO (Tx.InputsApplied era v) where
  type DTO (Tx.InputsApplied era v) = Web.Tx

instance IsCardanoEra era => ToDTOWithTxStatus (Tx.InputsApplied era v) where
  toDTOWithTxStatus status Tx.InputsApplied{..} =
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
      , outputUtxo = toDTO $ utxo <$> output
      , outputContract = case version of
          MarloweV1 -> Sem.marloweContract . datum <$> output
      , outputState = case version of
          MarloweV1 -> Sem.marloweState . datum <$> output
      , consumingTx = Nothing
      , invalidBefore = invalidBefore
      , invalidHereafter = invalidHereafter
      , txBody = case status of
          Unsigned -> Just $ toDTO txBody
          Submitted -> Nothing
      }

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

instance HasDTO (TxBody era) where
  type DTO (TxBody era) = Web.TextEnvelope

instance IsCardanoEra era => ToDTO (TxBody era) where
  toDTO = toDTO . serialiseToTextEnvelope Nothing

instance IsCardanoEra era => FromDTO (TxBody era) where
  fromDTO = hush . deserialiseFromTextEnvelope asType <=< fromDTO
    where
      asType = AsTxBody $ cardanoEraToAsType $ cardanoEra @era

instance HasDTO (Tx era) where
  type DTO (Tx era) = Web.TextEnvelope

instance IsCardanoEra era => ToDTO (Tx era) where
  toDTO = toDTO . serialiseToTextEnvelope Nothing

instance IsCardanoEra era => FromDTO (Tx era) where
  fromDTO = hush . deserialiseFromTextEnvelope asType <=< fromDTO
    where
      asType = AsTx $ cardanoEraToAsType $ cardanoEra @era

newtype ShelleyTxWitness era = ShelleyTxWitness (TxWitness (ShelleyLedgerEra era))

instance HasTypeProxy era => HasTypeProxy (ShelleyTxWitness era) where
    data AsType (ShelleyTxWitness era) = AsShelleyTxWitness (AsType era)
    proxyToAsType _ = AsShelleyTxWitness (proxyToAsType (Proxy :: Proxy era))

instance
  ( HasTypeProxy era
  , Typeable (ShelleyLedgerEra era)
  , ValidateScript (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.Script (ShelleyLedgerEra era)
  ) => SerialiseAsCBOR (ShelleyTxWitness era) where

  serialiseToCBOR (ShelleyTxWitness wit) = Binary.serialize' wit

  deserialiseFromCBOR _ bs = do
    let
      lbs = BSL.fromStrict bs

      annotator :: forall s. Binary.Decoder s (Binary.Annotator (TxWitness (ShelleyLedgerEra era)))
      annotator = Binary.fromCBOR

    (w :: TxWitness (ShelleyLedgerEra era)) <- Binary.decodeAnnotator "Shelley Tx Witness" annotator lbs
    pure $ ShelleyTxWitness w

instance
  ( IsShelleyBasedEra era
  , ValidateScript (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.Script (ShelleyLedgerEra era)
  ) => HasTextEnvelope (ShelleyTxWitness era) where
  textEnvelopeType _ = do
    "ShelleyTxWitness " <> case shelleyBasedEra :: ShelleyBasedEra era of
       ShelleyBasedEraAlonzo -> "AlonzoEra"
       ShelleyBasedEraBabbage -> "BabbageEra"

instance HasDTO (ShelleyTxWitness era) where
  type DTO (ShelleyTxWitness era) = Web.TextEnvelope

instance
  ( IsShelleyBasedEra era
  , ValidateScript (ShelleyLedgerEra era)
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.Script (ShelleyLedgerEra era)
  ) => ToDTO (ShelleyTxWitness era) where
  toDTO = toDTO . serialiseToTextEnvelope Nothing

instance
  ( IsShelleyBasedEra era
  , ValidateScript (ShelleyLedgerEra era)
  , IsCardanoEra era
  , Ledger.Core.Script (ShelleyLedgerEra era) ~ Ledger.Alonzo.Scripts.Script (ShelleyLedgerEra era))
  => FromDTO (ShelleyTxWitness era) where
  fromDTO = hush . deserialiseFromTextEnvelope asType <=< fromDTO
    where
      eraAsType = cardanoEraToAsType $ cardanoEra @era
      asType = AsShelleyTxWitness eraAsType

instance HasDTO TextEnvelope where
  type DTO TextEnvelope = Web.TextEnvelope

instance ToDTO TextEnvelope where
  toDTO TextEnvelope
    { teType = TextEnvelopeType teType
    , teDescription = TextEnvelopeDescr teDescription
    , teRawCBOR
    } = Web.TextEnvelope
      { teType = T.pack teType
      , teDescription = T.pack teDescription
      , teCborHex = Web.Base16 teRawCBOR
      }

instance FromDTO TextEnvelope where
  fromDTO Web.TextEnvelope
    { teType
    , teDescription
    , teCborHex
    } = Just TextEnvelope
      { teType = TextEnvelopeType $ T.unpack teType
      , teDescription = TextEnvelopeDescr $ T.unpack teDescription
      , teRawCBOR = Web.unBase16 teCborHex
      }

instance HasDTO Tx.RoleTokensConfig where
  type DTO Tx.RoleTokensConfig = Maybe Web.RolesConfig

instance FromDTO Tx.RoleTokensConfig where
  fromDTO = \case
    Nothing -> pure Tx.RoleTokensNone
    Just (Web.UsePolicy policy) -> Tx.RoleTokensUsePolicy <$> fromDTO policy
    Just (Web.Mint mint) -> Tx.RoleTokensMint <$> fromDTO mint

instance HasDTO Tx.Mint where
  type DTO Tx.Mint = Map Text Web.RoleTokenConfig

instance FromDTO Tx.Mint where
  fromDTO = fmap Tx.mkMint
    . traverse (sequence . bimap tokenNameToText convertConfig)
    <=< toNonEmpty
    . Map.toList
    where
      convertConfig = \case
        Web.RoleTokenSimple address -> (,Nothing) <$> fromDTO address
        Web.RoleTokenAdvanced address metadata -> curry (second Just)
          <$> fromDTO address
          <*> fromDTO metadata

instance HasDTO Tx.RoleTokenMetadata where
  type DTO Tx.RoleTokenMetadata = Web.TokenMetadata

instance FromDTO Tx.RoleTokenMetadata where
  fromDTO Web.TokenMetadata{..} = Tx.RoleTokenMetadata name image
    <$> fromDTO mediaType
    <*> pure description
    <*> case files of
          Nothing -> pure []
          Just files' -> fromDTO files'

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
  fromDTO = pure . \case
    Pagination.RangeAsc -> Query.Ascending
    Pagination.RangeDesc -> Query.Descending

toPaginationRange
  :: (KnownSymbol field, ToDTO a, IsRangeType (DTO a))
  => Query.Range a
  -> Pagination.Range field (DTO a)
toPaginationRange Query.Range{..} = Pagination.Range
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
  pure Query.Range { rangeStart, rangeDirection, .. }

tokenNameToText :: Text -> Chain.TokenName
tokenNameToText = Chain.TokenName . fromString . T.unpack

toNonEmpty :: [a] -> Maybe (NonEmpty a)
toNonEmpty [] = Nothing
toNonEmpty (a : as) = Just $ a :| as
