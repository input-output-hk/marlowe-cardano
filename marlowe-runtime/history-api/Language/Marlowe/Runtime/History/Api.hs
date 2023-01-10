{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.History.Api
  where

import Cardano.Api (CardanoMode, EraHistory(EraHistory))
import Control.Error (listToMaybe, note, runMaybeT)
import Control.Error.Util (hoistMaybe)
import Control.Monad (guard, when)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (ToJSON, Value(..), object, toJSON, (.=))
import Data.Bifunctor (first)
import Data.Binary (Binary, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (find, for_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Traversable (for)
import Data.Type.Equality (type (:~:)(Refl))
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (ScriptHash, TxError, TxId, TxOutRef(..), UTxOError)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..), getMarloweVersion)
import qualified Language.Marlowe.Scripts as V1
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
import Ouroboros.Consensus.BlockchainTime (SystemStart, fromRelativeTime)
import Ouroboros.Consensus.HardFork.History (interpretQuery, slotToWallclock)
import qualified Ouroboros.Network.Block as O
import qualified Plutus.V2.Ledger.Api as PV2

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
  deriving anyclass (Binary, ToJSON)

data ExtractCreationError
  = TxIxNotFound
  | ByronAddress
  | NonScriptAddress
  | InvalidScriptHash
  | NoCreateDatum
  | InvalidCreateDatum
  | NotCreationTransaction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON)

data ExtractMarloweTransactionError
  = TxInNotFound
  | NoRedeemer
  | InvalidRedeemer
  | NoTransactionDatum
  | InvalidTransactionDatum
  | NoPayoutDatum TxOutRef
  | InvalidPayoutDatum TxOutRef
  | InvalidValidityRange
  | SlotConversionFailed
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, ToJSON)

data FollowerStatus
  = Pending
  | Following SomeMarloweVersion
  | Waiting SomeMarloweVersion
  | Finished SomeMarloweVersion
  | Failed ContractHistoryError
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Binary, ToJSON)

data CreateStep v = CreateStep
  { createOutput :: TransactionScriptOutput v
  , payoutValidatorHash :: ScriptHash
  } deriving (Generic)

deriving instance Show (CreateStep 'V1)
deriving instance Eq (CreateStep 'V1)
instance ToJSON (CreateStep 'V1)

data SomeCreateStep = forall v. SomeCreateStep (MarloweVersion v) (CreateStep v)

instance Eq SomeCreateStep where
  SomeCreateStep MarloweV1 a == SomeCreateStep MarloweV1 b = a == b

instance Show SomeCreateStep where
  show (SomeCreateStep MarloweV1 a) = show a

instance Binary (CreateStep 'V1) where
  put CreateStep{..} = do
    put createOutput
    put payoutValidatorHash
  get = CreateStep <$> get <*> get

data RedeemStep v = RedeemStep
  { utxo        :: TxOutRef
  , redeemingTx :: TxId
  , datum       :: PayoutDatum v
  } deriving Generic

deriving instance Show (RedeemStep 'V1)
deriving instance Eq (RedeemStep 'V1)
instance ToJSON (RedeemStep 'V1)

instance Binary (RedeemStep 'V1) where
  put RedeemStep{..} = do
    put utxo
    put redeemingTx
    putPayoutDatum MarloweV1 datum
  get = RedeemStep <$> get <*> get <*> getPayoutDatum MarloweV1

data ContractStep v
  = ApplyTransaction (Transaction v)
  | RedeemPayout (RedeemStep v)
  deriving (Generic)

deriving instance Show (ContractStep 'V1)
deriving instance Eq (ContractStep 'V1)
instance Binary (ContractStep 'V1)
instance ToJSON (ContractStep 'V1)

extractCreation :: ContractId -> Chain.Transaction -> Either ExtractCreationError SomeCreateStep
extractCreation contractId tx@Chain.Transaction{inputs} = do
  Chain.TransactionOutput{ assets, address = scriptAddress, datum = mdatum } <-
    getOutput (txIx $ unContractId contractId) tx
  marloweScriptHash <- getScriptHash scriptAddress
  (SomeMarloweVersion version, MarloweScripts{..}) <- note InvalidScriptHash $ getMarloweVersion marloweScriptHash
  let payoutValidatorHash = payoutScript
  for_ inputs \Chain.TransactionInput{..} ->
    when (isScriptAddress marloweScriptHash address) $ Left NotCreationTransaction
  txDatum <- note NoCreateDatum mdatum
  datum <- note InvalidCreateDatum $ fromChainDatum version txDatum
  let createOutput = TransactionScriptOutput scriptAddress assets (unContractId contractId) datum
  pure $ SomeCreateStep version CreateStep{..}

getScriptHash :: Chain.Address -> Either ExtractCreationError ScriptHash
getScriptHash address = do
  credential <- note ByronAddress $ Chain.paymentCredential address
  case credential of
    Chain.ScriptCredential scriptHash -> pure scriptHash
    _                                 -> Left NonScriptAddress

isScriptAddress :: ScriptHash -> Chain.Address -> Bool
isScriptAddress scriptHash address = getScriptHash address == Right scriptHash

getOutput :: Chain.TxIx -> Chain.Transaction -> Either ExtractCreationError Chain.TransactionOutput
getOutput (Chain.TxIx i) Chain.Transaction{..} = go i outputs
  where
    go _ []        = Left TxIxNotFound
    go 0 (x : _)   = Right x
    go i' (_ : xs) = go (i' - 1) xs

data HistoryCommand status err result where
  FollowContract :: ContractId -> HistoryCommand Void ContractHistoryError Bool
  StopFollowingContract :: ContractId -> HistoryCommand Void Void Bool

instance CommandToJSON HistoryCommand where
  commandToJSON = \case
    FollowContract contractId -> object [ "follow-contract" .= contractId ]
    StopFollowingContract contractId -> object [ "stop-following-contract" .= contractId ]
  jobIdToJSON = \case
  errToJSON = \case
    TagFollowContract -> toJSON
    TagStopFollowingContract -> toJSON
  resultToJSON = \case
    TagFollowContract -> toJSON
    TagStopFollowingContract -> toJSON
  statusToJSON = \case
    TagFollowContract -> toJSON
    TagStopFollowingContract -> toJSON

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
    (TagFollowContract, TagFollowContract)               -> Just (Refl, Refl, Refl)
    (TagFollowContract, _)                               -> Nothing
    (TagStopFollowingContract, TagStopFollowingContract) -> Just (Refl, Refl, Refl)
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

data History v = History
  { create      :: CreateStep v
  , createBlock :: Chain.BlockHeader
  , steps       :: Map Chain.BlockHeader [ContractStep v]
  } deriving Generic

deriving instance Show (History 'V1)
deriving instance Eq (History 'V1)
instance Binary (History 'V1)

extractMarloweTransaction
  :: MarloweVersion v
  -> SystemStart
  -> EraHistory CardanoMode
  -> ContractId
  -> Chain.Address
  -> Chain.ScriptHash
  -> TxOutRef
  -> Chain.BlockHeader
  -> Chain.Transaction
  -> Either ExtractMarloweTransactionError (Transaction v)
extractMarloweTransaction version systemStart eraHistory contractId scriptAddress payoutValidatorHash consumedUTxO blockHeader Chain.Transaction{..} = do
  let transactionId = txId
  Chain.TransactionInput { redeemer = mRedeemer } <-
    note TxInNotFound $ find (consumesUTxO consumedUTxO) inputs
  rawRedeemer <- note NoRedeemer mRedeemer
  marloweInputs <- case version of
    MarloweV1 -> do
      redeemer <- note InvalidRedeemer $ Chain.fromRedeemer rawRedeemer
      for redeemer \case
        V1.Input content -> pure $ V1.NormalInput content
        V1.MerkleizedTxInput content continuationHash ->
          fmap (V1.MerkleizedInput content continuationHash)
            $ note InvalidRedeemer
            $ listToMaybe
            $ flip mapMaybe outputs \Chain.TransactionOutput{..} -> do
              guard $ datumHash == Just (Chain.DatumHash $ PV2.fromBuiltin continuationHash)
              Chain.fromDatum =<< datum
  (minSlot, maxSlot) <- case validityRange of
    Chain.MinMaxBound minSlot maxSlot -> pure (minSlot, maxSlot)
    _                                 -> Left InvalidValidityRange
  validityLowerBound <- slotStartTime minSlot
  validityUpperBound <- slotStartTime maxSlot
  scriptOutput <- runMaybeT do
    (ix, Chain.TransactionOutput{ assets, datum = mDatum }) <-
      hoistMaybe $ find (isToAddress scriptAddress . snd) $ zip [0..] outputs
    lift do
      rawDatum <- note NoTransactionDatum mDatum
      datum <- note InvalidTransactionDatum $ fromChainDatum version rawDatum
      let txIx = Chain.TxIx ix
      let utxo = Chain.TxOutRef{..}
      let address = scriptAddress
      pure TransactionScriptOutput{..}
  let
    payoutOutputs = Map.filter (isToScriptHash payoutValidatorHash)
      $ Map.fromList
      $ (\(txIx, output) -> (Chain.TxOutRef{..}, output)) <$> zip [0..] outputs
  payouts <- flip Map.traverseWithKey payoutOutputs \txOut Chain.TransactionOutput{address, datum=mPayoutDatum, assets} -> do
    rawPayoutDatum <- note (NoPayoutDatum txOut) mPayoutDatum
    payoutDatum <- note (InvalidPayoutDatum txOut) $ fromChainPayoutDatum version rawPayoutDatum
    pure $ Payout address assets payoutDatum
  let output = TransactionOutput{..}
  pure Transaction
    { transactionId
    , contractId
    , metadata
    , blockHeader
    , validityLowerBound
    , validityUpperBound
    , inputs = marloweInputs
    , output
    }
  where
    EraHistory _ interpreter = eraHistory
    slotStartTime (Chain.SlotNo slotNo) = do
      (relativeTime, _) <- first (const SlotConversionFailed)
        $ interpretQuery interpreter
        $ slotToWallclock
        $ O.SlotNo slotNo
      pure $ fromRelativeTime systemStart relativeTime

isToScriptHash :: Chain.ScriptHash -> Chain.TransactionOutput -> Bool
isToScriptHash toScriptHash Chain.TransactionOutput{..} = case Chain.paymentCredential address of
  Just (Chain.ScriptCredential hash) -> hash == toScriptHash
  _                                  -> False

isToAddress :: Chain.Address -> Chain.TransactionOutput -> Bool
isToAddress toAddress Chain.TransactionOutput{..} = address == toAddress

consumesUTxO :: TxOutRef -> Chain.TransactionInput -> Bool
consumesUTxO TxOutRef{..} Chain.TransactionInput { txId = txInId, txIx = txInIx } =
  txId == txInId && txIx == txInIx

data SomeHistory = forall v. SomeHistory (MarloweVersion v) (History v)

data HistoryQuery delimiter err results where
  GetFollowedContracts :: HistoryQuery ContractId Void (Map ContractId FollowerStatus)
  GetStatuses :: Set ContractId -> HistoryQuery Void Void (Map ContractId FollowerStatus)

instance Query.QueryToJSON HistoryQuery where
  queryToJSON = \case
    GetFollowedContracts -> String "get-followed-contracts"
    GetStatuses contractIds -> object [ "get-statuses" .= contractIds ]
  errToJSON = \case
    TagGetFollowedContracts -> toJSON
    TagGetStatuses -> toJSON
  resultToJSON = \case
    TagGetFollowedContracts -> toJSON
    TagGetStatuses -> toJSON
  delimiterToJSON = \case
    TagGetFollowedContracts -> toJSON
    TagGetStatuses -> toJSON

instance Query.IsQuery HistoryQuery where
  data Tag HistoryQuery delimiter err result where
    TagGetFollowedContracts :: Query.Tag HistoryQuery ContractId Void (Map ContractId FollowerStatus)
    TagGetStatuses :: Query.Tag HistoryQuery Void Void (Map ContractId FollowerStatus)

  tagFromQuery = \case
    GetFollowedContracts -> TagGetFollowedContracts
    GetStatuses _ -> TagGetStatuses

  tagEq TagGetFollowedContracts TagGetFollowedContracts = Just (Refl, Refl, Refl)
  tagEq TagGetFollowedContracts _ = Nothing
  tagEq TagGetStatuses TagGetStatuses = Just (Refl, Refl, Refl)
  tagEq TagGetStatuses _ = Nothing

  putTag = \case
    TagGetFollowedContracts -> putWord8 0x01
    TagGetStatuses -> putWord8 0x02

  getTag = do
    tagWord <- getWord8
    case tagWord of
      0x01 -> pure $ Query.SomeTag TagGetFollowedContracts
      0x02 -> pure $ Query.SomeTag TagGetStatuses
      _    -> fail "invalid tag bytes"

  putQuery = \case
    GetFollowedContracts  -> mempty
    GetStatuses contractIds  -> put contractIds

  getQuery = \case
    TagGetFollowedContracts -> pure GetFollowedContracts
    TagGetStatuses -> GetStatuses <$> get

  putDelimiter = \case
    TagGetFollowedContracts -> put
    TagGetStatuses -> put

  getDelimiter = \case
    TagGetFollowedContracts -> get
    TagGetStatuses -> get

  putErr = \case
    TagGetFollowedContracts -> put
    TagGetStatuses -> put

  getErr = \case
    TagGetFollowedContracts -> get
    TagGetStatuses -> get

  putResult = \case
    TagGetFollowedContracts -> put
    TagGetStatuses -> put

  getResult = \case
    TagGetFollowedContracts -> get
    TagGetStatuses -> get

type RuntimeHistoryQuery = Query.Query HistoryQuery

type RuntimeHistoryQueryClient = QueryClient HistoryQuery

type RuntimeHistoryQueryServer = QueryServer HistoryQuery

type RuntimeHistoryQueryCodec m = Codec RuntimeHistoryQuery DeserializeError m LBS.ByteString

historyQueryCodec :: Applicative m => RuntimeHistoryQueryCodec m
historyQueryCodec = codecQuery
