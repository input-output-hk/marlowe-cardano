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
  -> (WalletName -> Passphrase "raw" -> SomeMnemonic -> m WBE.WalletId)
  -> WalletName
  -> Passphrase "raw"
  -> m (Mnemonic 24, WalletId, PubKeyHash)
createWallet getPubKeyHash postWallet walletName passphrase = do
  mnemonic <- liftIO $ entropyToMnemonic <$> genEntropy @256
  walletId <- postWallet walletName passphrase (SomeMnemonic mnemonic)
  pubKeyHash <- getPubKeyHash walletId
  pure (mnemonic, walletId, pubKeyHash)

-- [UC-WALLET-TESTNET-2][2] Restore a testnet wallet
restoreWallet
  :: Monad m
  => (WBE.WalletId -> m PubKeyHash)
  -> (WalletName -> Passphrase "raw" -> SomeMnemonic -> m WBE.WalletId)
  -> SomeMnemonic
  -> WalletName
  -> Passphrase "raw"
  -> m (WalletId, PubKeyHash)
restoreWallet getPubKeyHash postWallet mnemonic walletName passphrase = do
  -- Call the WBE trying to restore the wallet, and take error 409 Conflict as a success
  walletId <- postWallet walletName passphrase mnemonic
  -- Get the pubKeyHash of the first wallet derivation
  pubKeyHash <- getPubKeyHash walletId
  pure (walletId, pubKeyHash)
