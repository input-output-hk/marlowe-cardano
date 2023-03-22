{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Sync.Database
  where

import Control.Monad.With (MonadWithExceptable)
import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.Query.Types
  (ContractFilter, Page, Range, SomeContractState, SomeTransaction, SomeTransactions, Withdrawal, WithdrawalFilter)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion(..), SomeMarloweVersion)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.History.Api (ContractStep, SomeCreateStep)
import Observe.Event.Component (FieldConfig(..), GetSelectorConfig, SelectorConfig(..), SomeJSON(..))
import Observe.Event.Explicit (EventBackend, addField, withEvent)

data DatabaseSelector f where
  GetTip :: DatabaseSelector (QueryField Void ChainPoint)
  GetTipForContract :: DatabaseSelector (QueryField ContractId ChainPoint)
  GetCreateStep :: DatabaseSelector (QueryField ContractId (Maybe GetCreateStepResult))
  GetIntersectionForContract :: DatabaseSelector (QueryField GetIntersectionForContractArguments (Maybe GetIntersectionForContractResult))
  GetIntersection :: DatabaseSelector (QueryField [BlockHeader] (Maybe BlockHeader))
  GetNextHeaders :: DatabaseSelector (QueryField ChainPoint (Next ContractHeader))
  GetNextSteps :: MarloweVersion v -> DatabaseSelector (QueryField (GetNextStepsArguments v) (Next (ContractStep v)))
  GetHeaders :: DatabaseSelector (QueryField GetHeadersArguments (Maybe (Page ContractId ContractHeader)))
  GetContractState :: DatabaseSelector (QueryField ContractId (Maybe SomeContractState))
  GetTransaction :: DatabaseSelector (QueryField TxId (Maybe SomeTransaction))
  GetTransactions :: DatabaseSelector (QueryField ContractId (Maybe SomeTransactions))
  GetWithdrawal :: DatabaseSelector (QueryField TxId (Maybe Withdrawal))
  GetWithdrawals :: DatabaseSelector (QueryField GetWithdrawalsArguments (Maybe (Page TxId Withdrawal)))

data GetWithdrawalsArguments = GetWithdrawalsArguments
  { filter :: WithdrawalFilter
  , range :: Range TxId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetHeadersArguments = GetHeadersArguments
  { filter :: ContractFilter
  , range :: Range ContractId
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data QueryField p r
  = Arguments p
  | Result r

data GetCreateStepResult = GetCreateStepResult
  { block :: BlockHeader
  , createStep :: SomeCreateStep
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetIntersectionForContractArguments = GetIntersectionForContractArguments
  { contractId :: ContractId
  , points :: [BlockHeader]
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetIntersectionForContractResult = GetIntersectionForContractResult
  { block :: BlockHeader
  , version :: SomeMarloweVersion
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

data GetNextStepsArguments v = GetNextStepsArguments
  { version :: MarloweVersion v
  , contractId :: ContractId
  , fromPoint :: ChainPoint
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

logDatabaseQueries :: MonadWithExceptable m => EventBackend m r DatabaseSelector -> DatabaseQueries m -> DatabaseQueries m
logDatabaseQueries eventBackend DatabaseQueries{..} = DatabaseQueries
  { getTip = withEvent eventBackend GetTip \ev -> do
      result <- getTip
      addField ev $ Result result
      pure result
  , getTipForContract = \contractId -> withEvent eventBackend GetTipForContract \ev -> do
      addField ev $ Arguments contractId
      result <- getTipForContract contractId
      addField ev $ Result result
      pure result
  , getCreateStep = \contractId -> withEvent eventBackend GetCreateStep \ev -> do
      addField ev $ Arguments contractId
      result <- getCreateStep contractId
      addField ev $ Result $ uncurry GetCreateStepResult <$> result
      pure result
  , getIntersection = \points -> withEvent eventBackend GetIntersection \ev -> do
      addField ev $ Arguments points
      result <- getIntersection points
      addField ev $ Result result
      pure result
  , getIntersectionForContract = \contractId points -> withEvent eventBackend GetIntersectionForContract \ev -> do
      addField ev $ Arguments $ GetIntersectionForContractArguments{..}
      result <- getIntersectionForContract contractId points
      addField ev $ Result $ uncurry GetIntersectionForContractResult <$> result
      pure result
  , getNextHeaders = \fromPoint -> withEvent eventBackend GetNextHeaders \ev -> do
      addField ev $ Arguments fromPoint
      result <- getNextHeaders fromPoint
      addField ev $ Result result
      pure result
  , getNextSteps = \version contractId fromPoint -> withEvent eventBackend (GetNextSteps version) \ev -> do
      addField ev $ Arguments $ GetNextStepsArguments{..}
      result <- getNextSteps version contractId fromPoint
      addField ev $ Result result
      pure result
  , getHeaders = \cFilter range -> withEvent eventBackend GetHeaders \ev -> do
      addField ev $ Arguments $ GetHeadersArguments cFilter range
      result <- getHeaders cFilter range
      addField ev $ Result result
      pure result
  , getContractState = \contractId -> withEvent eventBackend GetContractState \ev -> do
      addField ev $ Arguments contractId
      result <- getContractState contractId
      addField ev $ Result result
      pure result
  , getTransaction = \txId -> withEvent eventBackend GetTransaction \ev -> do
      addField ev $ Arguments txId
      result <- getTransaction txId
      addField ev $ Result result
      pure result
  , getTransactions = \contractId -> withEvent eventBackend GetTransactions \ev -> do
      addField ev $ Arguments contractId
      result <- getTransactions contractId
      addField ev $ Result result
      pure result
  , getWithdrawal = \txId -> withEvent eventBackend GetWithdrawal \ev -> do
      addField ev $ Arguments txId
      result <- getWithdrawal txId
      addField ev $ Result result
      pure result
  , getWithdrawals = \wFilter range -> withEvent eventBackend GetWithdrawals \ev -> do
      addField ev $ Arguments $ GetWithdrawalsArguments wFilter range
      result <- getWithdrawals wFilter range
      addField ev $ Result result
      pure result
  }

hoistDatabaseQueries :: (forall x. m x -> n x) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries f DatabaseQueries{..} = DatabaseQueries
  { getTip = f getTip
  , getTipForContract = f . getTipForContract
  , getCreateStep = f . getCreateStep
  , getIntersectionForContract = fmap f . getIntersectionForContract
  , getIntersection = f . getIntersection
  , getNextHeaders = f . getNextHeaders
  , getNextSteps = (fmap . fmap) f . getNextSteps
  , getHeaders = fmap f . getHeaders
  , getContractState = f . getContractState
  , getTransaction = f . getTransaction
  , getTransactions = f . getTransactions
  , getWithdrawal = f . getWithdrawal
  , getWithdrawals = fmap f . getWithdrawals
  }

data DatabaseQueries m = DatabaseQueries
  { getTip :: m ChainPoint
  , getTipForContract :: ContractId -> m ChainPoint
  , getCreateStep :: ContractId -> m (Maybe (BlockHeader, SomeCreateStep))
  , getIntersection :: [BlockHeader] -> m (Maybe BlockHeader)
  , getIntersectionForContract :: ContractId -> [BlockHeader] -> m (Maybe (BlockHeader, SomeMarloweVersion))
  , getNextHeaders :: ChainPoint -> m (Next ContractHeader)
  , getNextSteps :: forall v. MarloweVersion v -> ContractId -> ChainPoint -> m (Next (ContractStep v))
  , getHeaders :: ContractFilter -> Range ContractId -> m (Maybe (Page ContractId ContractHeader))
  , getContractState :: ContractId -> m (Maybe SomeContractState)
  , getTransaction :: TxId -> m (Maybe SomeTransaction)
  , getTransactions :: ContractId -> m (Maybe SomeTransactions)
  , getWithdrawal :: TxId -> m (Maybe Withdrawal)
  , getWithdrawals :: WithdrawalFilter -> Range TxId -> m (Maybe (Page TxId Withdrawal))
  }

data Next a
  = Rollback ChainPoint
  | Wait
  | Next BlockHeader [a]
  deriving stock (Generic, Functor)
  deriving anyclass (ToJSON)

getDatabaseSelectorConfig :: GetSelectorConfig DatabaseSelector
getDatabaseSelectorConfig = \case
  GetTip -> getQuerySelectorConfig "get-tip"
  GetTipForContract -> getQuerySelectorConfig "get-tip-for-contract"
  GetCreateStep -> getQuerySelectorConfig "get-create-step"
  GetIntersectionForContract -> getQuerySelectorConfig "get-intersection-for-contract"
  GetIntersection -> getQuerySelectorConfig "get-intersection"
  GetNextHeaders -> getQuerySelectorConfig "get-next-headers"
  GetNextSteps MarloweV1 -> getQuerySelectorConfig "get-next-steps"
  GetHeaders -> getQuerySelectorConfig "get-headers"
  GetContractState -> getQuerySelectorConfig "get-contract-state"
  GetTransaction -> getQuerySelectorConfig "get-transaction"
  GetTransactions -> getQuerySelectorConfig "get-transactions"
  GetWithdrawal -> getQuerySelectorConfig "get-withdrawal"
  GetWithdrawals -> getQuerySelectorConfig "get-withdrawals"

getQuerySelectorConfig :: (ToJSON p, ToJSON r) => Text -> SelectorConfig (QueryField p r)
getQuerySelectorConfig key = SelectorConfig key True FieldConfig
  { fieldKey = \case
      Arguments _ -> "arguments"
      Result _ -> "result"
  , fieldDefaultEnabled = const True
  , toSomeJSON = \case
      Arguments args -> SomeJSON args
      Result result -> SomeJSON result
  }
