{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Marlowe.ACTUS.Domain.ContractState where

import Data.Aeson.Types (ToJSON)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Language.Marlowe (Observation, Value)
import Language.Marlowe.ACTUS.Domain.ContractTerms (PRF)

{-| ACTUS contract states are defined in
    https://github.com/actusfrf/actus-dictionary/blob/master/actus-dictionary-states.json
-}
data ContractStatePoly a b = ContractStatePoly
  {
    tmd   :: Maybe b -- ^ Maturity Date (MD): The timestamp as per which the contract matures according to the initial terms or as per unscheduled events
  , nt    :: a       -- ^ Notional Principal (NT): The outstanding nominal value
  , ipnr  :: a       -- ^ Nominal Interest Rate (IPNR) : The applicable nominal rate
  , ipac  :: a       -- ^ Accrued Interest (IPAC): The current value of accrued interest
  , feac  :: a       -- ^ Fee Accrued (FEAC): The current value of accrued fees
  , nsc   :: a       -- ^ Notional Scaling Multiplier (SCNT): The multiplier being applied to principal cash flows
  , isc   :: a       -- ^ InterestScalingMultiplier (SCIP): The multiplier being applied to interest cash flows
  , prf   :: PRF     -- ^ Contract Performance (PRF)
  , sd    :: b       -- ^ Status Date (MD): The timestamp as per which the state is captured at any point in time
  , prnxt :: a       -- ^ Next Principal Redemption Payment (PRNXT): The value at which principal is being repaid
  , ipcb  :: a       -- ^ Interest Calculation Base (IPCB)
  , xd    :: Maybe b -- ^ Exercise Date (XD)
  , xa    :: Maybe a -- ^ Exercise Amount (XA)
  }
  deriving stock (Show, Eq)

type ContractState = ContractStatePoly Double LocalTime
type ContractStateMarlowe = ContractStatePoly (Value Observation) (Value Observation)

deriving instance Generic ContractState
deriving instance ToJSON ContractState
