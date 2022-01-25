{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Wallet.V1.CentralizedTestnet.Types where

import Cardano.Prelude
import Cardano.Wallet.Mock.Types (WalletInfo (..))
import Data.Aeson.Types (FromJSON, ToJSON)

newtype WalletName = WalletName Text

data RestorePostData =
    RestorePostData
        { getRestoreMnemonicPhrase :: [Text]
        , getRestorePassphrase     :: Text
        , getRestoreWalletName     :: Text
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data CreatePostData =
    CreatePostData
        { getCreatePassphrase :: Text
        , getCreateWalletName :: Text
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data CreateResponse = CreateResponse
  { mnemonic   :: [Text]
  , walletInfo :: WalletInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data RestoreError =
    InvalidMnemonic
    | RestoreWalletError
    | FetchPubKeyHashError
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)
