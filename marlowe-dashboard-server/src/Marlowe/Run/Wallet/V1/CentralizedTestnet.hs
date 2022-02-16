{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans       #-}

module Marlowe.Run.Wallet.V1.CentralizedTestnet
 ( createWallet
 , restoreWallet
 )
 where

import Cardano.Mnemonic (Mnemonic, SomeMnemonic (..), entropyToMnemonic, genEntropy)
import Cardano.Prelude hiding (Handler, log)
import Cardano.Wallet.Primitive.AddressDerivation (Passphrase)
import Cardano.Wallet.Primitive.Types (WalletId, WalletName)
import qualified Cardano.Wallet.Primitive.Types as WBE
import Ledger (PubKeyHash (..))

createWallet
  :: MonadIO m
  => (WBE.WalletId -> m PubKeyHash)
  -> (WBE.WalletId -> m Text)
  -> (WalletName -> Passphrase "raw" -> SomeMnemonic -> m WBE.WalletId)
  -> WalletName
  -> Passphrase "raw"
  -> m (Mnemonic 24, WalletId, PubKeyHash, Text)
createWallet getPubKeyHash getAddress postWallet walletName passphrase = do
  mnemonic <- liftIO $ entropyToMnemonic <$> genEntropy @256
  walletId <- postWallet walletName passphrase (SomeMnemonic mnemonic)
  pubKeyHash <- getPubKeyHash walletId
  address <- getAddress walletId
  pure (mnemonic, walletId, pubKeyHash, address)

-- [UC-WALLET-TESTNET-2][2] Restore a testnet wallet
restoreWallet
  :: Monad m
  => (WBE.WalletId -> m PubKeyHash)
  -> (WBE.WalletId -> m Text)
  -> (WalletName -> Passphrase "raw" -> SomeMnemonic -> m WBE.WalletId)
  -> SomeMnemonic
  -> WalletName
  -> Passphrase "raw"
  -> m (WalletId, PubKeyHash, Text)
restoreWallet getPubKeyHash getAddress postWallet mnemonic walletName passphrase = do
  -- Call the WBE trying to restore the wallet, and take error 409 Conflict as a success
  walletId <- postWallet walletName passphrase mnemonic
  -- Get the pubKeyHash of the first wallet derivation
  pubKeyHash <- getPubKeyHash walletId
  address <- getAddress walletId
  pure (walletId, pubKeyHash, address)
