{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Wallet.Types where

import           Cardano.Prelude
import           Data.Aeson.Types       (ToJSON)
import           Plutus.V1.Ledger.Value (Value)
data GetTotalFunds =
    GetTotalFunds
        { assets :: Value
        , sync   :: Double
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

