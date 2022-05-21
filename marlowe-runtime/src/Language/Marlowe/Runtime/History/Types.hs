{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData                 #-}
module Language.Marlowe.Runtime.History.Types where

import Data.Binary (Binary)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Marlowe (Contract)
import Language.Marlowe.Runtime.Chain.Types (MarloweAddress, MarloweTxIn)
import Type.Reflection (Typeable)

data HistoryEvent
  = Create (HistoryChainEvent CreateContractError CreateContract ContractCreated)
  | Deposit (HistoryChainEvent DepositError Deposit Deposited)
  | Choice (HistoryChainEvent ChoiceError Choose Chosen)
  | Closed
  deriving (Generic, Typeable, Show, Eq)

data HistoryChainEvent err request fulfilled
  = Accepted request
  | Confirmed fulfilled
  | Rejected request err
  deriving (Generic, Typeable, Show, Eq)

data CreateContractError deriving (Show, Eq)
data DepositError deriving (Show, Eq)
data ChoiceError deriving (Show, Eq)

data CreateContract = CreateContract
  { contractCreation_contract :: Contract
  , contractCreation_roles    :: Map RoleName MarloweAddress
  }
  deriving (Generic, Typeable, Show, Eq)

data ContractCreated = ContractCreated
  { contractCreated_datum :: MarloweDatum
  , contractCreated_txOut :: MarloweTxIn
  }
  deriving (Generic, Typeable, Show, Eq)

newtype RoleName = RoleName Text
  deriving (Generic, Typeable, Show, Eq, Binary)

data MarloweDatum = MarloweDatum
  { marloweDatum_state    :: MarloweState
  , marloweDatum_contract :: Contract
  }
  deriving (Generic, Typeable, Show, Eq)

data MarloweState = MarloweState
  { marloweState_accounts :: MarloweState
  , marloweState_contract :: Contract
  }
  deriving (Generic, Typeable, Show, Eq)

data Deposit deriving (Show, Eq)
data Deposited deriving (Show, Eq)
data Choose deriving (Show, Eq)
data Chosen deriving (Show, Eq)
