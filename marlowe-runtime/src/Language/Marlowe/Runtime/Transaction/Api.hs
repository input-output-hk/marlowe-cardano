{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsConstraintsBuildupError(..)
  , ApplyInputsError(..)
  , CreateBuildupError(..)
  , CreateError(..)
  , JobId(..)
  , LoadMarloweContextError(..)
  , MarloweTxCommand(..)
  , Mint(unMint)
  , NFTMetadata(unNFTMetadata)
  , SubmitError(..)
  , SubmitStatus(..)
  , WalletAddresses(..)
  , WithdrawError(..)
  , mkMint
  , mkNFTMetadata
  ) where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , SerialiseAsRawBytes(serialiseToRawBytes)
  , Tx
  , TxBody
  , deserialiseFromCBOR
  , deserialiseFromRawBytes
  , serialiseToCBOR
  )
import Cardano.Api.Shelley (StakeCredential(..))
import Data.Binary (Binary, Get, get, getWord8, put)
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address
  , BlockHeader
  , Lovelace
  , Metadata
  , PlutusScript
  , PolicyId
  , ScriptHash
  , TokenName
  , TransactionMetadata
  , TxId
  , TxOutRef
  , getUTCTime
  , putUTCTime
  )
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Transaction.Constraints (ConstraintError)
import Network.Protocol.Job.Types (Command(..), SomeTag(..))

-- CIP-25 metadata
newtype NFTMetadata = NFTMetadata { unNFTMetadata :: Metadata }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary)

-- FIXME: Validate the metadata format
mkNFTMetadata :: Metadata -> Maybe NFTMetadata
mkNFTMetadata = Just . NFTMetadata

-- | Non empty mint request.
newtype Mint = Mint { unMint :: Map TokenName (Address, Either Natural (Maybe NFTMetadata)) }
  deriving stock (Eq, Ord, Generic)
  deriving newtype (Binary, Semigroup, Monoid)

mkMint :: NonEmpty (TokenName, (Address, Either Natural (Maybe NFTMetadata))) -> Mint
mkMint = Mint . Map.fromList . NonEmpty.toList

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
    -> Maybe (Either PolicyId Mint)
    -- ^ The initial distribution of role tokens
    -> TransactionMetadata
    -- ^ Optional metadata to attach to the transaction
    -> Lovelace
    -- ^ Min Lovelace which should be used for the contract output.
    -> Contract v
    -- ^ The contract to run
    -> MarloweTxCommand Void (CreateError v)
        ( ContractId -- The ID of the contract (tx output that carries the datum)
        , TxBody BabbageEra -- The unsigned tx body, to be signed by a wallet.
        )

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
    -> Maybe UTCTime
    -- ^ The "invalid before" bound of the validity interval. If omitted, this
    -- is computed from the contract.
    -> Maybe UTCTime
    -- ^ The "invalid hereafter" bound of the validity interval. If omitted, this
    -- is computed from the contract.
    -> Redeemer v
    -- ^ The inputs to apply.
    -> MarloweTxCommand Void (ApplyInputsError v)
        ( TxBody BabbageEra -- The unsigned tx body, to be signed by a wallet.
        )

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

instance Command MarloweTxCommand where
  data Tag MarloweTxCommand status err result where
    TagCreate :: MarloweVersion v -> Tag MarloweTxCommand Void (CreateError v) (ContractId, TxBody BabbageEra)
    TagApplyInputs :: MarloweVersion v -> Tag MarloweTxCommand Void (ApplyInputsError v) (TxBody BabbageEra)
    TagWithdraw :: MarloweVersion v -> Tag MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra)
    TagSubmit :: Tag MarloweTxCommand SubmitStatus SubmitError BlockHeader

  data JobId MarloweTxCommand stats err result where
    JobIdSubmit :: TxId -> JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader

  tagFromCommand = \case
    Create _ version _ _ _ _ _ -> TagCreate version
    ApplyInputs version _ _ _ _ _ -> TagApplyInputs version
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
      case mStakeCredential of
        Nothing -> putWord8 0x01
        Just credential -> do
          putWord8 0x02
          case credential of
            StakeCredentialByKey stakeKeyHash -> do
              putWord8 0x01
              put $ serialiseToRawBytes stakeKeyHash
            StakeCredentialByScript scriptHash -> do
              putWord8 0x02
              put $ serialiseToRawBytes scriptHash
      put walletAddresses
      put roles
      put metadata
      put minAda
      putContract version contract
    ApplyInputs version walletAddresses contractId invalidBefore invalidHereafter redeemer -> do
      put walletAddresses
      put contractId
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidHereafter
      putRedeemer version redeemer
    Withdraw _ walletAddresses contractId tokenName -> do
      put walletAddresses
      put contractId
      put tokenName
    Submit tx -> put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate version -> do
      mStakeCredentialTag <- getWord8
      mStakeCredential <- case mStakeCredentialTag of
        0x01 -> pure Nothing
        0x02 -> Just <$> do
          stakeCredentialTag <- getWord8
          case stakeCredentialTag  of
            0x01 -> do
              bytes <- get
              case deserialiseFromRawBytes (AsHash AsStakeKey) bytes of
                Nothing -> fail "invalid stake key hash bytes"
                Just stakeKeyHash -> pure $ StakeCredentialByKey stakeKeyHash
            0x02 -> do
              bytes <- get
              case deserialiseFromRawBytes AsScriptHash bytes of
                Nothing -> fail "invalid stake key hash bytes"
                Just scriptHash -> pure $ StakeCredentialByScript scriptHash
            _ -> fail $ "Invalid stake credential tag " <> show stakeCredentialTag
        _ -> fail $ "Invalid Maybe tag " <> show mStakeCredentialTag
      walletAddresses <- get
      roles <- get
      metadata <- get
      minAda <- get
      contract <- getContract version
      pure $ Create mStakeCredential version walletAddresses roles metadata minAda contract

    TagApplyInputs version -> do
      walletAddresses <- get
      contractId <- get
      invalidBefore <- getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> getUTCTime
        t -> fail $ "Invalid Maybe tag: " <> show t
      invalidHereafter <- getWord8 >>= \case
        0 -> pure Nothing
        1 -> Just <$> getUTCTime
        t -> fail $ "Invalid Maybe tag: " <> show t
      redeemer <- getRedeemer version
      pure $ ApplyInputs version walletAddresses contractId invalidBefore invalidHereafter redeemer

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
    TagCreate _ -> \(contractId, txBody) -> put contractId *> putTxBody txBody
    TagApplyInputs _ -> putTxBody
    TagWithdraw _ -> putTxBody
    TagSubmit -> put

  getResult = \case
    TagCreate _ -> (,) <$> get <*> getTxBody
    TagApplyInputs _ -> getTxBody
    TagWithdraw _ -> getTxBody
    TagSubmit -> get

putTxBody :: TxBody BabbageEra -> Put
putTxBody = put . serialiseToCBOR

getTxBody :: Get (TxBody BabbageEra)
getTxBody = do
  bytes <- get @ByteString
  case deserialiseFromCBOR (AsTxBody AsBabbage) bytes of
    Left err     -> fail $ show err
    Right txBody -> pure txBody

data WalletAddresses = WalletAddresses
  { changeAddress  :: Address
  , extraAddresses :: Set Address
  , collateralUtxos :: Set TxOutRef
  }
  deriving (Eq, Show, Generic, Binary)

data CreateError v
  = CreateConstraintError (ConstraintError v)
  | CreateLoadMarloweContextFailed LoadMarloweContextError
  | CreateBuildupFailed CreateBuildupError
  deriving (Generic)

data CreateBuildupError
  = MintingUtxoSelectionFailed
  | AddressDecodingFailed Address
  | MintingScriptDecodingFailed PlutusScript
  deriving (Eq, Show, Generic)
  deriving anyclass Binary

deriving instance Eq (CreateError 'V1)
deriving instance Show (CreateError 'V1)
instance Binary (CreateError 'V1)

data ApplyInputsError v
  = ApplyInputsConstraintError (ConstraintError v)
  | ScriptOutputNotFound
  | ApplyInputsLoadMarloweContextFailed LoadMarloweContextError
  | ApplyInputsConstraintsBuildupFailed ApplyInputsConstraintsBuildupError
  | SlotConversionFailed String

deriving instance Eq (ApplyInputsError 'V1)
deriving instance Show (ApplyInputsError 'V1)
deriving instance Generic (ApplyInputsError 'V1)
instance Binary (ApplyInputsError 'V1)

data ApplyInputsConstraintsBuildupError
  = MarloweComputeTransactionFailed String
  | UnableToDetermineTransactionTimeout
  deriving (Eq, Show, Generic)
  deriving anyclass Binary

data WithdrawError v
  = WithdrawConstraintError (ConstraintError v)
  | WithdrawLoadMarloweContextFailed LoadMarloweContextError
  | UnableToFindPayoutForAGivenRole TokenName
  deriving (Generic)

deriving instance Eq (WithdrawError 'V1)
deriving instance Show (WithdrawError 'V1)
instance Binary (WithdrawError 'V1)

data LoadMarloweContextError
  = LoadMarloweContextErrorNotFound
  | LoadMarloweContextErrorVersionMismatch SomeMarloweVersion
  | LoadMarloweContextToCardanoError
  | MarloweScriptNotPublished ScriptHash
  | PayoutScriptNotPublished ScriptHash
  | InvalidScriptAddress Address
  | UnknownMarloweScript ScriptHash
  deriving (Eq, Show, Ord, Generic)
  deriving anyclass Binary

data SubmitError
  = SubmitException
  | SubmitFailed String -- should be from show TxValidationErrorInMode
  | TxDiscarded
  deriving (Eq, Show, Generic, Binary)

data SubmitStatus
  = Submitting
  | Accepted
  deriving (Eq, Show, Generic, Binary)
