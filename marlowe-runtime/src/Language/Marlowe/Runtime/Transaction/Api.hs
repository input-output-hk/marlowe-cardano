{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Transaction.Api
  where

import Cardano.Api
  ( AsType(..)
  , BabbageEra
  , ScriptDataSupportedInEra(..)
  , SerialiseAsRawBytes(serialiseToRawBytes)
  , Tx
  , TxBody
  , deserialiseFromCBOR
  , deserialiseFromRawBytes
  , serialiseToCBOR
  )
import Cardano.Api.Shelley (StakeCredential(..))
import qualified Data.Aeson as Aeson
import Data.Binary (Binary, Get, get, getWord8, put)
import Data.Binary.Put (Put, putWord8)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Some (Some(..))
import Data.Time (UTCTime)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api
  (Address, BlockHeader, ScriptHash, TokenName, TxId, TxOutRef, getUTCTime, putUTCTime)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Transaction.Constraints (ConstraintError)
import Network.Protocol.Job.Types (Command(..), SomeTag(..))

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
    -> Map TokenName Address
    -- ^ The initial distribution of role tokens
    -> Map Int Aeson.Value
    -- ^ Optional metadata to attach to the transaction
    -> Contract v
    -- ^ The contract to run
    -> MarloweTxCommand Void CreateError
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
    -> MarloweTxCommand Void ApplyInputsError
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
    -> PayoutDatum v
    -- ^ The names of the roles whose assets to withdraw.
    -> MarloweTxCommand Void WithdrawError
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
    TagCreate :: Tag MarloweTxCommand Void CreateError (ContractId, TxBody BabbageEra)
    TagApplyInputs :: Tag MarloweTxCommand Void ApplyInputsError (TxBody BabbageEra)
    TagWithdraw :: Tag MarloweTxCommand Void WithdrawError (TxBody BabbageEra)
    TagSubmit :: Tag MarloweTxCommand SubmitStatus SubmitError BlockHeader

  data JobId MarloweTxCommand stats err result where
    JobIdSubmit :: TxId -> JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader

  tagFromCommand = \case
    Create {}        -> TagCreate
    ApplyInputs {} -> TagApplyInputs
    Withdraw {}        -> TagWithdraw
    Submit _                -> TagSubmit

  tagFromJobId = \case
    JobIdSubmit _ -> TagSubmit

  tagEq t1 t2 = case (t1, t2) of
    (TagCreate, TagCreate) -> pure (Refl, Refl, Refl)
    (TagCreate, _) -> Nothing
    (TagApplyInputs, TagApplyInputs) -> pure (Refl, Refl, Refl)
    (TagApplyInputs, _) -> Nothing
    (TagWithdraw, TagWithdraw) -> pure (Refl, Refl, Refl)
    (TagWithdraw, _) -> Nothing
    (TagSubmit, TagSubmit) -> pure (Refl, Refl, Refl)
    (TagSubmit, _) -> Nothing

  putTag = \case
    TagCreate -> putWord8 0x01
    TagApplyInputs -> putWord8 0x02
    TagWithdraw -> putWord8 0x03
    TagSubmit -> putWord8 0x04

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> pure $ SomeTag TagCreate
      0x02 -> pure $ SomeTag TagApplyInputs
      0x03 -> pure $ SomeTag TagWithdraw
      0x04 -> pure $ SomeTag TagSubmit
      _    -> fail $ "Invalid command tag: " <> show tag

  putJobId = \case
    JobIdSubmit txId -> put txId

  getJobId = \case
    TagCreate -> fail "create has no job ID"
    TagApplyInputs -> fail "apply inputs has no job ID"
    TagWithdraw -> fail "withdraw has no job ID"
    TagSubmit -> JobIdSubmit <$> get

  putCommand = \case
    Create mStakeCredential version walletAddresses roles metadata contract -> do
      put $ SomeMarloweVersion version
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
      put $ fmap Aeson.encode metadata
      putContract version contract
    ApplyInputs version walletAddresses contractId invalidBefore invalidHereafter redeemer -> do
      put $ SomeMarloweVersion version
      put walletAddresses
      put contractId
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidHereafter
      putRedeemer version redeemer
    Withdraw version walletAddresses contractId payoutDatum -> do
      put $ SomeMarloweVersion version
      put walletAddresses
      put contractId
      putPayoutDatum version payoutDatum
    Submit tx -> put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate -> do
      SomeMarloweVersion version <- get
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
      metadataRaw <- get
      contract <- getContract version
      metadata <- case traverse Aeson.decode metadataRaw of
        Nothing       -> fail "failed to parse metadata JSON"
        Just metadata -> pure metadata
      pure $ Create mStakeCredential version walletAddresses roles metadata contract
    TagApplyInputs -> do
      SomeMarloweVersion version <- get
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
    TagWithdraw -> do
      SomeMarloweVersion version <- get
      walletAddresses <- get
      contractId <- get
      payoutDatum <- getPayoutDatum version
      pure $ Withdraw version walletAddresses contractId payoutDatum
    TagSubmit -> do
      bytes <- get @ByteString
      Submit <$> case deserialiseFromCBOR (AsTx AsBabbage) bytes of
        Left err -> fail $ show err
        Right tx -> pure tx

  putStatus = \case
    TagCreate -> absurd
    TagApplyInputs -> absurd
    TagWithdraw -> absurd
    TagSubmit -> put

  getStatus = \case
    TagCreate -> fail "create has no status"
    TagApplyInputs -> fail "apply inputs has no status"
    TagWithdraw -> fail "withdraw has no status"
    TagSubmit -> get

  putErr = \case
    TagCreate -> put
    TagApplyInputs -> put
    TagWithdraw -> put
    TagSubmit -> put

  getErr = \case
    TagCreate -> get
    TagApplyInputs -> get
    TagWithdraw -> get
    TagSubmit -> get

  putResult = \case
    TagCreate -> \(contractId, txBody) -> put contractId *> putTxBody txBody
    TagApplyInputs -> putTxBody
    TagWithdraw -> putTxBody
    TagSubmit -> put

  getResult = \case
    TagCreate -> (,) <$> get <*> getTxBody
    TagApplyInputs -> getTxBody
    TagWithdraw -> getTxBody
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

data CreateError
  = CreateConstraintError ConstraintError
  | CreateLoadMarloweContextFailed LoadMarloweContextError
  deriving (Eq, Show, Generic)
  deriving anyclass Binary

data ApplyInputsError
  = ApplyInputsConstraintError ConstraintError
  | ScriptOutputNotFound
  | ApplyInputsLoadMarloweContextFailed LoadMarloweContextError
  deriving (Eq, Show, Generic, Binary)

data WithdrawError
  = WithdrawConstraintError ConstraintError
  | WithdrawLoadMarloweContextFailed LoadMarloweContextError
  deriving (Eq, Show, Generic)
  deriving anyclass Binary

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
