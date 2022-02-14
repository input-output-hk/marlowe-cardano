{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Marlowe.Run.Wallet.V1.CentralizedTestnet.Types where

import Cardano.Prelude
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON), parseFail)

import Cardano.Mnemonic (MkSomeMnemonic (mkSomeMnemonic), MkSomeMnemonicError (getMkSomeMnemonicError), Mnemonic,
                         SomeMnemonic, mnemonicToText)
import qualified Cardano.Wallet.Primitive.AddressDerivation as Cardano
import Data.Text.Class (FromText (fromText), TextDecodingError (getTextDecodingError), ToText (toText))
import Marlowe.Run.Wallet.V1.Types (WalletInfo, WalletName)

data RestorePostData =
  RestorePostData
    { getRestoreMnemonicPhrase :: RestoreMnemonic
    , getRestorePassphrase     :: Passphrase
    , getRestoreWalletName     :: WalletName
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON)

data CreatePostData =
  CreatePostData
    { getCreatePassphrase :: Passphrase
    , getCreateWalletName :: WalletName
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data CreateResponse = CreateResponse
  { mnemonic   :: CreateMnemonic
  , walletInfo :: WalletInfo
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

newtype Passphrase = Passphrase { unPassphrase :: Cardano.Passphrase "raw" }
  deriving (Eq, Show)

instance ToJSON Passphrase where
  toJSON  = toJSON . toText . unPassphrase

instance FromJSON Passphrase where
  parseJSON value = do
    walletNameText <- parseJSON value
    case fromText walletNameText of
      Left e           -> parseFail  $ getTextDecodingError e
      Right passphrase -> pure $ Passphrase passphrase

newtype RestoreMnemonic = RestoreMnemonic { unRestoreMnemonic :: SomeMnemonic }
  deriving (Eq, Show)

instance FromJSON RestoreMnemonic where
  parseJSON value = do
    arr <- parseJSON value
    case mkSomeMnemonic @'[15, 18, 21, 24] arr of
      Left e           -> parseFail  $ getMkSomeMnemonicError e
      Right passphrase -> pure $ RestoreMnemonic passphrase

newtype CreateMnemonic = CreateMnemonic { unCreateMnemonic :: Mnemonic  24 }
  deriving (Eq, Show)

instance ToJSON CreateMnemonic where
  toJSON  = toJSON . mnemonicToText . unCreateMnemonic
