{-# LANGUAGE TemplateHaskell #-}
module Marlowe.Run.Contract.V1 where

import Cardano.Api (NetworkId)
import Cardano.Prelude (hush)
import Data.Maybe (listToMaybe)
import Ledger.Tx (TxOut (..))
import Ledger.Tx.CardanoAPI (toCardanoAddress)
import Marlowe.Run.Contract.V1.Types (RoleToken (RoleToken))
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))
import Prelude

getRoleToken :: Monad m
             => (AssetClass -> m [TxOut ])
             -> NetworkId
             -> CurrencySymbol
             -> TokenName
             -> m (Maybe RoleToken)
getRoleToken getAssetClassUtxos networkId currencySymbol tokenName = do
  txOuts <- getAssetClassUtxos $ AssetClass (currencySymbol, tokenName)
  let mAddress = txOutAddress <$> listToMaybe txOuts
  let mCardanoAddress = hush . toCardanoAddress networkId =<< mAddress
  pure $ RoleToken currencySymbol tokenName <$> mCardanoAddress
