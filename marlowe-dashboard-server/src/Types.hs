{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Types where

import qualified Data.Aeson       as Aeson
import           Data.Aeson.Types (FromJSON, ToJSON, genericToJSON)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)


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
    | CantFetchPubKeyHash
    | UnknownRestoreError
    deriving stock (Eq, Generic, Show)
    deriving anyclass (ToJSON)
