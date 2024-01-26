{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Server.TxClient where

import Cardano.Api (BabbageEraOnwards, Tx, getTxId)
import Colog (Message, WithLog)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Component
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
import Control.Monad.Event.Class
import Control.Monad.IO.Unlift (MonadUnliftIO, liftIO, withRunInIO)
import Data.Foldable (for_)
import Data.Kind (Type)
import qualified Data.Map as Map
import Data.Set (Set)
import Data.Time (UTCTime)
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash, Lovelace, StakeCredential, TokenName, TxId, TxOutRef)
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
  Accounts,
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
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Job.Client
import Observe.Event.Backend (setAncestorEventBackend)

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
  -> Accounts
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
