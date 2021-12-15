{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Wallet.Types where

import           Cardano.Prelude
import           Data.Aeson.Types (ToJSON)

data GetTotalFunds =
    GetTotalFunds
        { balance :: Integer
        , sync    :: Double
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)

