{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Types where

import           Cardano.Prelude
import           Data.Aeson.Types (FromJSON, ToJSON)
import           Servant.Client   (ClientEnv)


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

type Env = ClientEnv
