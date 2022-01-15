{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Wallet.CentralizedTestnet.Types where

import Cardano.Prelude
import Data.Aeson.Types (FromJSON, ToJSON)

newtype CheckPostData = CheckPostData [Text]
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data RestorePostData =
    RestorePostData
        { getMnemonicPhrase :: [Text]
        , getPassphrase     :: Text
        , getWalletName     :: Text
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data RestoreError =
    InvalidMnemonic
    | RestoreWalletError
    | FetchPubKeyHashError
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)
