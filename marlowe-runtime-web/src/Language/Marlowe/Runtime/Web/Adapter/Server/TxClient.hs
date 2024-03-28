{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Marlowe.Runtime.Web.Adapter.Server.TxClient where

import Cardano.Api (BabbageEraOnwards, Tx, getTxId)
import Colog (Message, WithLog)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Component (Component, component)
import Control.Concurrent.STM (
  STM,
  TMVar,
  TVar,
  atomically,
  modifyTVar,
  newEmptyTMVar,
  newTQueue,
  newTVar,
  putTMVar,
  readTQueue,
  readTVar,
  takeTMVar,
  writeTQueue,
 )
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when, (<=<))
import Control.Monad.Event.Class (MonadEvent (localBackend))
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO, withRunInIO)
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Time (UTCTime)
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (Assets, DatumHash, Lovelace, StakeCredential, TokenName, TxId, TxOutRef)
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  ContractId,
  Inputs,
  IsMarloweVersion (..),
  MarloweTransactionMetadata,
  MarloweVersion,
  MarloweVersionTag,
 )
import Language.Marlowe.Runtime.Transaction.Api (
  Account,
  ApplyInputsError,
  ContractCreated (..),
  ContractCreatedInEra (..),
  CreateError,
  InputsApplied (..),
  InputsAppliedInEra (..),
  MarloweTxCommand (..),
  RoleTokensConfig,
  SubmitError,
  SubmitStatus (..),
  WalletAddresses,
  WithdrawError,
  WithdrawTx (..),
  WithdrawTxInEra (..),
 )
import qualified Language.Marlowe.Runtime.Transaction.Api as Tx
import qualified Language.Marlowe.Runtime.Web.Contract.API as Web
import qualified Language.Marlowe.Runtime.Web.Core.Tx as Web
import qualified Language.Marlowe.Runtime.Web.Tx.API as Web
import qualified Language.Marlowe.Runtime.Web.Withdrawal.API as Web
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Job.Client (
  ClientStAwait (SendMsgPoll),
  ClientStCmd (
    ClientStCmd,
    recvMsgAwait,
    recvMsgFail,
    recvMsgSucceed
  ),
  ClientStInit (SendMsgExec),
  JobClient (JobClient),
  liftCommand,
 )
import Observe.Event.Backend (setAncestorEventBackend)

import Cardano.Api (
  BabbageEraOnwards (..),
 )
import qualified Data.Map as M
import qualified Language.Marlowe.Core.V1.Semantics as Sem

import Data.Foldable (Foldable (..))
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..))
import Language.Marlowe.Runtime.Core.Api as Runetime.Core.Api (
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
 )
import qualified Language.Marlowe.Runtime.Core.Api as Core
import qualified Language.Marlowe.Runtime.Core.Api as Core.Api (Payout (..))
import qualified Language.Marlowe.Runtime.Web.Core.Asset as Web
import qualified Language.Marlowe.Runtime.Web.Core.MarloweVersion as Web
import qualified Language.Marlowe.Runtime.Web.Payout.API as Web

import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (FromDTO (..), HasDTO (..), ToDTO (..))
import qualified PlutusLedgerApi.V2 as PV2

newtype TxClientDependencies m = TxClientDependencies
  { connector :: Connector MarloweRuntimeClient m
  }

type CreateContract m =
  forall v
   . Maybe StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> Maybe TokenName
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Maybe Lovelace
  -> Map Account Assets
  -> Either (Contract v) DatumHash
  -> m (Either CreateError (ContractCreated v))

type ApplyInputs m =
  forall v
   . MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> MarloweTransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs v
  -> m (Either ApplyInputsError (InputsApplied v))

type Withdraw m =
  forall v
   . MarloweVersion v
  -> WalletAddresses
  -> Set TxOutRef
  -> m (Either WithdrawError (WithdrawTx v))

data TempTxStatus = Unsigned | Submitted

type Submit r m = r -> Submit' m

type Submit' m = forall era. BabbageEraOnwards era -> Tx era -> m (Maybe SubmitError)

data TempTx (tx :: Type -> MarloweVersionTag -> Type) where
  TempTx
    :: BabbageEraOnwards era -> MarloweVersion v -> TempTxStatus -> tx era v -> TempTx tx

-- | Public API of the TxClient
data TxClient r m = TxClient
  { createContract :: CreateContract m
  , applyInputs :: ApplyInputs m
  , withdraw :: Withdraw m
  , submitContract :: ContractId -> Submit r m
  , submitTransaction :: ContractId -> TxId -> Submit r m
  , submitWithdrawal :: TxId -> Submit r m
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreatedInEra))
  , getTempContracts :: STM [TempTx ContractCreatedInEra]
  , lookupTempTransaction :: ContractId -> TxId -> STM (Maybe (TempTx InputsAppliedInEra))
  , getTempTransactions :: ContractId -> STM [TempTx InputsAppliedInEra]
  , lookupTempWithdrawal :: TxId -> STM (Maybe (TempTx WithdrawTxInEra))
  , getTempWithdrawals :: STM [TempTx WithdrawTxInEra]
  }

-- Basically a lens to the actual map of temp txs to modify within a structure.
-- For example, tempTransactions is a (Map ContractId (Map TxId (TempTx InputsApplied))),
-- so to apply the given map modification to the inner map, we provide
-- \update -> Map.update (Just . update txId) contractId
type WithMapUpdate k tx a =
  (k -> Map.Map k (TempTx tx) -> Map.Map k (TempTx tx))
  -- ^ Takes a function that, given a key in a map, modifies a map of temp txs
  -> (a -> a)
  -- ^ And turns it into a function that modifies values of some type a

-- Pack a TVar of a's with a lens for modifying some inner map in the TVar and
-- existentialize the type parameters.
data SomeTVarWithMapUpdate = forall k tx a. (Ord k) => SomeTVarWithMapUpdate (TVar a) (WithMapUpdate k tx a)

data TxInEraWithReferenceScripts where
  TxInEraWithReferenceScripts
    :: BabbageEraOnwards era
    -> Tx era
    -> TxInEraWithReferenceScripts

txClient
  :: forall r s env m
   . (MonadEvent r s m, MonadUnliftIO m, WithLog env Message m)
  => Component m (TxClientDependencies m) (TxClient r m)
txClient = component "web-tx-client" \TxClientDependencies{..} -> do
  tempContracts <- newTVar mempty
  tempTransactions <- newTVar mempty
  tempWithdrawals <- newTVar mempty
  submitQueue <- newTQueue

  let runSubmitGeneric
        :: SomeTVarWithMapUpdate
        -> BabbageEraOnwards era
        -> Tx era
        -> TMVar (Maybe SubmitError)
        -> m ()
      runSubmitGeneric (SomeTVarWithMapUpdate tempVar updateTemp) era tx sender = do
        let cmd = Submit era tx
            client = JobClient $ pure $ SendMsgExec cmd $ clientCmd True
            clientCmd report =
              ClientStCmd
                { recvMsgFail = \err -> liftIO $ atomically do
                    when report $ putTMVar sender $ Just err
                    modifyTVar tempVar $ updateTemp $ Map.update (Just . toUnsigned)
                , recvMsgSucceed = \_ -> liftIO $ atomically do
                    modifyTVar tempVar $ updateTemp Map.delete
                    when report $ putTMVar sender Nothing
                , recvMsgAwait = \status _ -> do
                    report' <- liftIO case status of
                      Accepted -> do
                        when report $ atomically do
                          putTMVar sender Nothing
                          modifyTVar tempVar $ updateTemp $ Map.update (Just . toSubmitted)
                        pure False
                      _ -> pure report
                    delay <- liftIO $ newDelay 1_000_000
                    liftIO $ atomically $ waitDelay delay
                    pure $ SendMsgPoll $ clientCmd report'
                }
        runConnector connector $ RunTxClient client

      genericSubmit
        :: SomeTVarWithMapUpdate
        -> r
        -> BabbageEraOnwards era
        -> Tx era
        -> m (Maybe SubmitError)
      genericSubmit tVarWithUpdate currentParent era tx = do
        liftIO do
          sender <- atomically do
            sender <- newEmptyTMVar
            writeTQueue submitQueue (tVarWithUpdate, TxInEraWithReferenceScripts era tx, sender, currentParent)
            pure sender
          atomically $ takeTMVar sender

      runTxClient = withRunInIO \runInIO -> do
        (tVarWithUpdate, TxInEraWithReferenceScripts era tx, sender, currentParent) <- atomically $ readTQueue submitQueue
        concurrently_
          ( try @SomeException $
              runInIO $
                localBackend (setAncestorEventBackend currentParent) $
                  runSubmitGeneric tVarWithUpdate era tx sender
          )
          (runInIO runTxClient)

  pure
    ( runTxClient
    , TxClient
        { createContract = \stakeCredential version addresses threadName roles metadata minUTxODeposit accounts contract -> do
            response <-
              runConnector connector $
                RunTxClient $
                  liftCommand $
                    Create stakeCredential version addresses threadName roles metadata minUTxODeposit accounts contract
            liftIO $ for_ response \(ContractCreated era creation@ContractCreatedInEra{contractId}) ->
              atomically $
                modifyTVar tempContracts $
                  Map.insert contractId $
                    TempTx era version Unsigned creation
            pure response
        , applyInputs = \version addresses contractId metadata invalidBefore invalidHereafter inputs -> do
            response <-
              runConnector connector $
                RunTxClient $
                  liftCommand $
                    ApplyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs
            liftIO $ for_ response \(InputsApplied era application@InputsAppliedInEra{txBody}) -> do
              let txId = fromCardanoTxId $ getTxId txBody
              let tempTx = TempTx era version Unsigned application
              atomically $
                modifyTVar tempTransactions $
                  Map.alter (Just . maybe (Map.singleton txId tempTx) (Map.insert txId tempTx)) contractId
            pure response
        , withdraw = \version addresses payouts -> do
            response <- runConnector connector $ RunTxClient $ liftCommand $ Withdraw version addresses payouts
            liftIO $ for_ response \(WithdrawTx era withdrawal@WithdrawTxInEra{txBody}) ->
              atomically $
                modifyTVar tempWithdrawals $
                  Map.insert (fromCardanoTxId $ getTxId txBody) $
                    TempTx era version Unsigned withdrawal
            pure response
        , submitContract = \contractId -> genericSubmit $ SomeTVarWithMapUpdate tempContracts ($ contractId)
        , submitTransaction = \contractId txId -> genericSubmit $ SomeTVarWithMapUpdate tempTransactions \update -> Map.update (Just . update txId) contractId
        , submitWithdrawal = \txId -> genericSubmit $ SomeTVarWithMapUpdate tempWithdrawals ($ txId)
        , lookupTempContract = \contractId -> Map.lookup contractId <$> readTVar tempContracts
        , getTempContracts = fmap snd . Map.toAscList <$> readTVar tempContracts
        , lookupTempTransaction = \contractId txId -> (Map.lookup txId <=< Map.lookup contractId) <$> readTVar tempTransactions
        , getTempTransactions = \contractId -> fmap snd . foldMap Map.toList . Map.lookup contractId <$> readTVar tempTransactions
        , lookupTempWithdrawal = \txId -> Map.lookup txId <$> readTVar tempWithdrawals
        , getTempWithdrawals = fmap snd . Map.toAscList <$> readTVar tempWithdrawals
        }
    )

toSubmitted :: TempTx tx -> TempTx tx
toSubmitted (TempTx era v _ tx) = TempTx era v Submitted tx

toUnsigned :: TempTx tx -> TempTx tx
toUnsigned (TempTx era v _ tx) = TempTx era v Unsigned tx

-- | States that a type can be encoded as a DTO given a tx status.
class (HasDTO a) => ToDTOWithTxStatus a where
  toDTOWithTxStatus :: TempTxStatus -> a -> DTO a

-- | States that a type can be encoded as a DTO given a tx status.
class (HasDTO a) => FromDTOWithTxStatus a where
  fromDTOWithTxStatus :: DTO a -> Maybe (TempTxStatus, a)

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
      , tags = fold $ toDTO $ marloweMetadata metadata
      , metadata = toDTO $ transactionMetadata metadata
      , status = toDTO status
      , block = Nothing
      , initialContract = case version of
          MarloweV1 -> Sem.marloweContract datum
      , initialState = case version of
          MarloweV1 -> Sem.marloweState datum
      , currentContract = case version of
          MarloweV1 -> Just $ Sem.marloweContract datum
      , state = case version of
          MarloweV1 -> Just $ Sem.marloweState datum
      , assets = toDTO assets
      , utxo = Nothing
      , txBody = case status of
          Unsigned -> Just case era of
            BabbageEraOnwardsBabbage -> toDTO txBody
            BabbageEraOnwardsConway -> toDTO txBody
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
      , tags = fold $ toDTO $ marloweMetadata metadata
      , metadata = toDTO $ transactionMetadata metadata
      , status = toDTO status
      , block = Nothing
      , inputUtxo = toDTO $ utxo input
      , inputContract = case (version, input) of
          (MarloweV1, TransactionScriptOutput{..}) -> Sem.marloweContract datum
      , inputState = case (version, input) of
          (MarloweV1, TransactionScriptOutput{..}) -> Sem.marloweState datum
      , inputs = case version of
          MarloweV1 -> inputs
      , outputUtxo = toDTO $ utxo <$> scriptOutput output
      , outputContract = case version of
          MarloweV1 -> Sem.marloweContract . Runetime.Core.Api.datum <$> scriptOutput output
      , outputState = case version of
          MarloweV1 -> Sem.marloweState . Runetime.Core.Api.datum <$> scriptOutput output
      , assets = maybe emptyAssets (\Core.TransactionScriptOutput{..} -> toDTO assets) $ scriptOutput output
      , consumingTx = Nothing
      , invalidBefore = invalidBefore
      , invalidHereafter = invalidHereafter
      , payouts = case version of
          MarloweV1 ->
            (\(payoutId, Core.Api.Payout{..}) -> Web.Payout (toDTO payoutId) (toDTO . tokenName $ datum) (toDTO assets))
              <$> M.toList (payouts output)
      , reconstructedSemanticInput
      , reconstructedSemanticOutput = case (version, input) of
          (MarloweV1, TransactionScriptOutput{..}) ->
            V1.computeTransaction
              reconstructedSemanticInput
              (Sem.marloweState datum)
              (Sem.marloweContract datum)
      , txBody = case status of
          Unsigned -> Just case era of
            BabbageEraOnwardsBabbage -> toDTO txBody
            BabbageEraOnwardsConway -> toDTO txBody
          Submitted -> Nothing
      }
    where
      reconstructedSemanticInput = case version of
        MarloweV1 ->
          V1.TransactionInput
            { txInputs = inputs
            , txInterval =
                ( utcToPOSIXTime invalidBefore
                , utcToPOSIXTime invalidHereafter - 1
                )
            }
emptyAssets :: Web.Assets
emptyAssets = Web.Assets 0 $ Web.Tokens mempty

utcToPOSIXTime :: UTCTime -> PV2.POSIXTime
utcToPOSIXTime = PV2.POSIXTime . floor . (1000 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
