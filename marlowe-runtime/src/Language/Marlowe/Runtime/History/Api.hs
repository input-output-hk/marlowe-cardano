{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Marlowe.Runtime.History.Api where

import Data.Binary (Binary, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (ScriptHash, TxError, TxId, TxOutRef, UTxOError)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
import Network.Protocol.ChainSeek.Codec (DeserializeError)
import Network.Protocol.Job.Client
import Network.Protocol.Job.Codec
import Network.Protocol.Job.Server
import Network.Protocol.Job.Types
import Network.Protocol.Query.Client (QueryClient)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (QueryServer)
import qualified Network.Protocol.Query.Types as Query
import Network.TypedProtocol.Codec

data ContractHistoryError
  = HansdshakeFailed
  | FindTxFailed TxError
  | ExtractContractFailed ExtractCreationError
  | FollowScriptUTxOFailed UTxOError
  | FollowPayoutUTxOsFailed (Map Chain.TxOutRef UTxOError)
  | ExtractMarloweTransactionFailed ExtractMarloweTransactionError
  | PayoutUTxONotFound Chain.TxOutRef
  | CreateTxRolledBack
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Binary

data ExtractCreationError
  = TxIxNotFound
  | ByronAddress
  | NonScriptAddress
  | InvalidScriptHash
  | NoCreateDatum
  | InvalidCreateDatum
  | NotCreationTransaction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Binary

data ExtractMarloweTransactionError
  = TxInNotFound
  | NoRedeemer
  | InvalidRedeemer
  | NoTransactionDatum
  | InvalidTransactionDatum
  | NoPayoutDatum TxOutRef
  | InvalidPayoutDatum TxOutRef
  | InvalidValidityRange
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Binary

data FollowerStatus
  = Pending
  | Following SomeMarloweVersion
  | Waiting SomeMarloweVersion
  | Finished SomeMarloweVersion
  | Failed ContractHistoryError
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Binary

data CreateStep v = CreateStep
  { datum               :: Datum v
  , scriptAddress       :: Chain.Address
  , payoutValidatorHash :: ScriptHash
  }

deriving instance Show (CreateStep 'V1)
deriving instance Eq (CreateStep 'V1)

instance Binary (CreateStep 'V1) where
  put CreateStep{..} = do
    putDatum MarloweV1 datum
    put scriptAddress
    put payoutValidatorHash
  get = CreateStep <$> getDatum MarloweV1 <*> get <*> get

data RedeemStep v = RedeemStep
  { utxo        :: TxOutRef
  , redeemingTx :: TxId
  , datum       :: PayoutDatum v
  }

deriving instance Show (RedeemStep 'V1)
deriving instance Eq (RedeemStep 'V1)

instance Binary (RedeemStep 'V1) where
  put RedeemStep{..} = do
    put utxo
    put redeemingTx
    putPayoutDatum MarloweV1 datum
  get = RedeemStep <$> get <*> get <*> getPayoutDatum MarloweV1

data ContractStep v
  = Create (CreateStep v)
  | ApplyTransaction (Transaction v)
  | RedeemPayout (RedeemStep v)
  -- TODO add TimeoutElapsed
  deriving (Generic)

deriving instance Show (ContractStep 'V1)
deriving instance Eq (ContractStep 'V1)
instance Binary (ContractStep 'V1)

data HistoryCommand status err result where
  FollowContract :: ContractId -> HistoryCommand Void ContractHistoryError Bool
  StopFollowingContract :: ContractId -> HistoryCommand Void Void Bool

instance Command HistoryCommand where
  data JobId HistoryCommand status err result where

  data Tag HistoryCommand status err result where
    TagFollowContract :: Tag HistoryCommand Void ContractHistoryError Bool
    TagStopFollowingContract :: Tag HistoryCommand Void Void Bool

  tagFromCommand = \case
    FollowContract _        -> TagFollowContract
    StopFollowingContract _ -> TagStopFollowingContract

  tagFromJobId = \case

  tagEq = curry \case
    (TagFollowContract, TagFollowContract)               -> Just Refl
    (TagFollowContract, _)                               -> Nothing
    (TagStopFollowingContract, TagStopFollowingContract) -> Just Refl
    (TagStopFollowingContract, _)                        -> Nothing

  putTag = \case
    TagFollowContract        -> putWord8 0x01
    TagStopFollowingContract -> putWord8 0x02

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> pure $ SomeTag TagFollowContract
      0x02 -> pure $ SomeTag TagStopFollowingContract
      _    -> fail $ "Invalid HistoryCommand tag: " <> show tag

  putCommand = \case
    FollowContract contractId        -> put contractId

    StopFollowingContract contractId -> put contractId

  getCommand = \case
    TagFollowContract        -> FollowContract <$> get
    TagStopFollowingContract -> StopFollowingContract <$> get

  putJobId = \case

  getJobId _ = fail "History commands have no job IDs"

  putStatus = \case
    TagFollowContract        -> absurd
    TagStopFollowingContract -> absurd

  getStatus _ = fail "History commands have no statuses"

  putErr = \case
    TagFollowContract        -> put
    TagStopFollowingContract -> absurd

  getErr = \case
    TagFollowContract        -> get
    TagStopFollowingContract -> fail "StopFollowingContract has no error"

  putResult = \case
    TagFollowContract        -> put
    TagStopFollowingContract -> put

  getResult = \case
    TagFollowContract        -> get
    TagStopFollowingContract -> get

type RuntimeHistoryJob = Job HistoryCommand

type RuntimeHistoryJobClient = JobClient HistoryCommand

type RuntimeHistoryJobServer = JobServer HistoryCommand

type RuntimeHistoryJobCodec m = Codec RuntimeHistoryJob DeserializeError m LBS.ByteString

historyJobCodec :: Applicative m => RuntimeHistoryJobCodec m
historyJobCodec = codecJob

type HistoryPage v = Map Chain.BlockHeader [ContractStep v]

data SomeHistoryPage = forall v. SomeHistoryPage (MarloweVersion v) (HistoryPage v)

instance Binary SomeHistoryPage where
  put (SomeHistoryPage version page) = do
    put $ SomeMarloweVersion version
    case version of
      MarloweV1 -> put page
  get = do
    SomeMarloweVersion version <- get
    case version of
      MarloweV1 -> SomeHistoryPage version <$> get

data HistoryQuery delimiter err results where
  GetFollowedContracts :: HistoryQuery ContractId Void (Map ContractId FollowerStatus)
  GetHistory :: ContractId -> HistoryQuery () ContractHistoryError SomeHistoryPage

instance Query.IsQuery HistoryQuery where
  data Tag HistoryQuery delimiter err result where
    TagGetFollowedContracts :: Query.Tag HistoryQuery ContractId Void (Map ContractId FollowerStatus)
    TagGetHistory :: Query.Tag HistoryQuery () ContractHistoryError SomeHistoryPage

  tagFromQuery = \case
    GetFollowedContracts -> TagGetFollowedContracts
    GetHistory _         -> TagGetHistory

  tagEq TagGetFollowedContracts TagGetFollowedContracts = Just Query.Refl
  tagEq TagGetFollowedContracts _                       = Nothing
  tagEq TagGetHistory TagGetHistory                     = Just Query.Refl
  tagEq TagGetHistory _                                 = Nothing

  putTag = \case
    TagGetFollowedContracts -> putWord8 0x01
    TagGetHistory           -> putWord8 0x02

  getTag = do
    tagWord <- getWord8
    case tagWord of
      0x01 -> pure $ Query.SomeTag TagGetFollowedContracts
      0x02 -> pure $ Query.SomeTag TagGetHistory
      _    -> fail "invalid tag bytes"

  putQuery = \case
    GetFollowedContracts  -> mempty
    GetHistory contractId -> put contractId

  getQuery = \case
    TagGetFollowedContracts -> pure GetFollowedContracts
    TagGetHistory           -> GetHistory <$> get

  putDelimiter = \case
    TagGetFollowedContracts -> put
    TagGetHistory           -> put

  getDelimiter = \case
    TagGetFollowedContracts -> get
    TagGetHistory           -> get

  putErr = \case
    TagGetFollowedContracts -> put
    TagGetHistory           -> put

  getErr = \case
    TagGetFollowedContracts -> get
    TagGetHistory           -> get

  putResult = \case
    TagGetFollowedContracts -> put
    TagGetHistory           -> put

  getResult = \case
    TagGetFollowedContracts -> get
    TagGetHistory           -> get

type RuntimeHistoryQuery = Query.Query HistoryQuery

type RuntimeHistoryQueryClient = QueryClient HistoryQuery

type RuntimeHistoryQueryServer = QueryServer HistoryQuery

type RuntimeHistoryQueryCodec m = Codec RuntimeHistoryQuery DeserializeError m LBS.ByteString

historyQueryCodec :: Applicative m => RuntimeHistoryQueryCodec m
historyQueryCodec = codecQuery
