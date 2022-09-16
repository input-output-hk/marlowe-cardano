{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Transaction.Api
  where

import Cardano.Api (AsType(..), ScriptDataSupportedInEra(..), Tx, TxBody, deserialiseFromCBOR, serialiseToCBOR)
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
import Language.Marlowe.Runtime.ChainSync.Api (Address, BlockHeader, TokenName, TxId, getUTCTime, putUTCTime)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Transaction.Constraints (UnsolvableConstraintsError)
import Network.Protocol.Job.Types (Command(..), SomeTag(..))

-- | The low-level runtime API for building and submitting transactions.
data MarloweTxCommand status err result where
  -- | Construct a transaction that starts a new Marlowe contract. The
  -- resulting, unsigned transaction can be signed via the cardano API or a
  -- wallet provider. When signed, the 'Submit' command can be used to submit
  -- the transaction to the attached Cardano node.
  Create
    :: ScriptDataSupportedInEra era
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
        , TxBody era -- The unsigned tx body, to be signed by a wallet.
        )

  -- | Construct a transaction that advances an active Marlowe contract by
  -- applying a sequence of inputs. The resulting, unsigned transaction can be
  -- signed via the cardano API or a wallet provider. When signed, the 'Submit'
  -- command can be used to submit the transaction to the attached Cardano node.
  ApplyInputs
    :: ScriptDataSupportedInEra era
    -> MarloweVersion v
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
        ( TxBody era -- The unsigned tx body, to be signed by a wallet.
        )

  -- | Construct a transaction that withdraws available assets from an active
  -- Marlowe contract for a set of roles in the contract. The resulting,
  -- unsigned transaction can be signed via the cardano API or a wallet
  -- provider. When signed, the 'Submit' command can be used to submit the
  -- transaction to the attached Cardano node.
  Withdraw
    :: ScriptDataSupportedInEra era
    -> MarloweVersion v
    -- ^ The Marlowe version to use
    -> WalletAddresses
    -- ^ The wallet addresses to use when constructing the transaction
    -> PayoutDatum v
    -- ^ The names of the roles whose assets to withdraw.
    -> MarloweTxCommand Void WithdrawError
        ( TxBody era -- The unsigned tx body, to be signed by a wallet.
        )

  -- | Submits a signed transaction to the attached Cardano node.
  Submit
    :: ScriptDataSupportedInEra era
    -> Tx era
    -- ^ A signed transaction to submit
    -> MarloweTxCommand
        SubmitStatus -- This job reports the status of the tx submission, which can take some time.
        SubmitError
        BlockHeader  -- The block header of the block this transaction was added to.

instance Command MarloweTxCommand where
  data Tag MarloweTxCommand status err result where
    TagCreate :: ScriptDataSupportedInEra era -> Tag MarloweTxCommand Void CreateError ( ContractId, TxBody era)
    TagApplyInputs :: ScriptDataSupportedInEra era -> Tag MarloweTxCommand Void ApplyInputsError (TxBody era)
    TagWithdraw :: ScriptDataSupportedInEra era -> Tag MarloweTxCommand Void WithdrawError (TxBody era)
    TagSubmit :: ScriptDataSupportedInEra era -> Tag MarloweTxCommand SubmitStatus SubmitError BlockHeader

  data JobId MarloweTxCommand stats err result where
    JobIdSubmit :: ScriptDataSupportedInEra era -> TxId -> JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader

  tagFromCommand = \case
    Create era _ _ _ _ _        -> TagCreate era
    ApplyInputs era _ _ _ _ _ _ -> TagApplyInputs era
    Withdraw era _ _ _        -> TagWithdraw era
    Submit era _                -> TagSubmit era

  tagFromJobId = \case
    JobIdSubmit era _ -> TagSubmit era

  tagEq t1 t2 = case (t1, t2) of
    (TagCreate era1, TagCreate era2) -> do
      Refl <- eraEq era1 era2
      pure (Refl, Refl, Refl)
    (TagCreate _, _) -> Nothing
    (TagApplyInputs era1, TagApplyInputs era2) -> do
      Refl <- eraEq era1 era2
      pure (Refl, Refl, Refl)
    (TagApplyInputs _, _) -> Nothing
    (TagWithdraw era1, TagWithdraw era2) -> do
      Refl <- eraEq era1 era2
      pure (Refl, Refl, Refl)
    (TagWithdraw _, _) -> Nothing
    (TagSubmit era1, TagSubmit era2) -> do
      Refl <- eraEq era1 era2
      pure (Refl, Refl, Refl)
    (TagSubmit _, _) -> Nothing

  putTag = \case
    TagCreate era      -> putWord8 0x01 *> putEra era
    TagApplyInputs era -> putWord8 0x02 *> putEra era
    TagWithdraw era    -> putWord8 0x03 *> putEra era
    TagSubmit era      -> putWord8 0x04 *> putEra era

  getTag = do
    tag <- getWord8
    Some era <- getEra
    case tag of
      0x01 -> pure $ SomeTag $ TagCreate era
      0x02 -> pure $ SomeTag $ TagCreate era
      0x03 -> pure $ SomeTag $ TagCreate era
      0x04 -> pure $ SomeTag $ TagSubmit era
      _    -> fail $ "Invalid era tag: " <> show tag

  putJobId = \case
    JobIdSubmit _ txId -> put txId

  getJobId = \case
    TagCreate _      -> fail "create has no job ID"
    TagApplyInputs _ -> fail "apply inputs has no job ID"
    TagWithdraw _    -> fail "withdraw has no job ID"
    TagSubmit era    -> JobIdSubmit era <$> get

  putCommand = \case
    Create _ version walletAddresses roles metadata contract -> do
      put $ SomeMarloweVersion version
      put walletAddresses
      put roles
      put $ fmap Aeson.encode metadata
      putContract version contract
    ApplyInputs _ version walletAddresses contractId invalidBefore invalidHereafter redeemer -> do
      put $ SomeMarloweVersion version
      put walletAddresses
      put contractId
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidBefore
      maybe (putWord8 0) (\t -> putWord8 1 *> putUTCTime t) invalidHereafter
      putRedeemer version redeemer
    Withdraw _ version walletAddresses payoutDatum -> do
      put $ SomeMarloweVersion version
      put walletAddresses
      putPayoutDatum version payoutDatum
    Submit era tx -> case era of
      ScriptDataInAlonzoEra  -> put $ serialiseToCBOR tx
      ScriptDataInBabbageEra -> put $ serialiseToCBOR tx

  getCommand = \case
    TagCreate era      -> do
      SomeMarloweVersion version <- get
      walletAddresses <- get
      roles <- get
      metadataRaw <- get
      contract <- getContract version
      metadata <- case traverse Aeson.decode metadataRaw of
        Nothing       -> fail "failed to parse metadata JSON"
        Just metadata -> pure metadata
      pure $ Create era version walletAddresses roles metadata contract
    TagApplyInputs era -> do
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
      pure $ ApplyInputs era version walletAddresses contractId invalidBefore invalidHereafter redeemer
    TagWithdraw era    -> do
      SomeMarloweVersion version <- get
      walletAddresses <- get
      payoutDatum <- getPayoutDatum version
      pure $ Withdraw era version walletAddresses payoutDatum
    TagSubmit era      -> do
      bytes <- get @ByteString
      Submit era <$> case era of
        ScriptDataInAlonzoEra -> case deserialiseFromCBOR (AsTx AsAlonzo) bytes of
          Left err -> fail $ show err
          Right tx -> pure tx
        ScriptDataInBabbageEra -> case deserialiseFromCBOR (AsTx AsBabbage) bytes of
          Left err -> fail $ show err
          Right tx -> pure tx

  putStatus = \case
    TagCreate _      -> absurd
    TagApplyInputs _ -> absurd
    TagWithdraw _    -> absurd
    TagSubmit _      -> put

  getStatus = \case
    TagCreate _      -> fail "create has no status"
    TagApplyInputs _ -> fail "apply inputs has no status"
    TagWithdraw _    -> fail "withdraw has no status"
    TagSubmit _      -> get

  putErr = \case
    TagCreate _      -> put
    TagApplyInputs _ -> put
    TagWithdraw _    -> put
    TagSubmit _      -> put

  getErr = \case
    TagCreate _      -> get
    TagApplyInputs _ -> get
    TagWithdraw _    -> get
    TagSubmit _      -> get

  putResult = \case
    TagCreate era      -> \(contractId, txBody) -> put contractId *> putTxBody era txBody
    TagApplyInputs era -> putTxBody era
    TagWithdraw era    -> putTxBody era
    TagSubmit _        -> put

  getResult = \case
    TagCreate era      -> (,) <$> get <*> getTxBody era
    TagApplyInputs era -> getTxBody era
    TagWithdraw era    -> getTxBody era
    TagSubmit _        -> get

putTxBody :: ScriptDataSupportedInEra era -> TxBody era -> Put
putTxBody = \case
  ScriptDataInAlonzoEra  -> put . serialiseToCBOR
  ScriptDataInBabbageEra -> put . serialiseToCBOR

getTxBody :: ScriptDataSupportedInEra era -> Get (TxBody era)
getTxBody era = do
  bytes <- get @ByteString
  case era of
    ScriptDataInAlonzoEra -> case deserialiseFromCBOR (AsTxBody AsAlonzo) bytes of
      Left err     -> fail $ show err
      Right txBody -> pure txBody
    ScriptDataInBabbageEra -> case deserialiseFromCBOR (AsTxBody AsBabbage) bytes of
      Left err     -> fail $ show err
      Right txBody -> pure txBody

eraEq :: ScriptDataSupportedInEra era1 -> ScriptDataSupportedInEra era2 -> Maybe (era1 :~: era2)
eraEq ScriptDataInAlonzoEra ScriptDataInAlonzoEra   = Just Refl
eraEq ScriptDataInAlonzoEra _                       = Nothing
eraEq ScriptDataInBabbageEra ScriptDataInBabbageEra = Just Refl
eraEq ScriptDataInBabbageEra _                      = Nothing

putEra :: ScriptDataSupportedInEra era -> Put
putEra = putWord8 . \case
  ScriptDataInAlonzoEra  -> 0x01
  ScriptDataInBabbageEra -> 0x02

getEra :: Get (Some ScriptDataSupportedInEra)
getEra = getWord8 >>= \case
  0x01 -> pure $ Some ScriptDataInAlonzoEra
  0x02 -> pure $ Some ScriptDataInBabbageEra
  tag  -> fail $ "Invalid era tag: " <> show tag

data WalletAddresses = WalletAddresses
  { changeAddress  :: Address
  , extraAddresses :: Set Address
  }
  deriving (Eq, Show, Generic, Binary)

newtype CreateError
  = CreateUnsolvableConstraints UnsolvableConstraintsError
  deriving (Eq, Show, Generic)
  deriving anyclass Binary

data ApplyInputsError
  = ApplyInputsUnsolvableConstraints UnsolvableConstraintsError
  | ScriptOutputNotFound
  deriving (Eq, Show, Generic, Binary)

newtype WithdrawError
  = WithdrawUnsolvableConstraints UnsolvableConstraintsError
  deriving (Eq, Show, Generic)
  deriving anyclass Binary

data SubmitError
  = SubmitException
  | SubmitFailed String -- should be from show TxValidationErrorInMode
  deriving (Eq, Show, Generic, Binary)

data SubmitStatus
  = Submitting
  | Accepted
  deriving (Eq, Show, Generic, Binary)
