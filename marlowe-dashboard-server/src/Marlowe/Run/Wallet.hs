{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Marlowe.Run.Wallet where

import Cardano.Prelude hiding (Handler)
import qualified Cardano.Wallet.Api.Types as WBE (ApiT (..), ApiWallet (..), ApiWalletAssetsBalance (..),
                                                  ApiWalletBalance (..))
import Cardano.Wallet.Primitive.SyncProgress (SyncProgress (..))
import Cardano.Wallet.Primitive.Types (WalletId)
import Cardano.Wallet.Primitive.Types.Hash (getHash)
import Cardano.Wallet.Primitive.Types.TokenMap (AssetId (..), TokenMap, toFlatList)
import Cardano.Wallet.Primitive.Types.TokenPolicy (unTokenName, unTokenPolicyId)
import Cardano.Wallet.Primitive.Types.TokenQuantity (TokenQuantity (..))
import Data.Quantity (getPercentage, getQuantity)
import GHC.Natural (naturalToInteger)
import qualified Plutus.V1.Ledger.Ada as Ledger
import Plutus.V1.Ledger.Api (Value)
import qualified Plutus.V1.Ledger.Value as Ledger

-- The Wallet BackEnd uses TokenMap while Marlowe Run uses the Ledger MultiAsset Value
wbeTokenMapToLedgerValue ::
    TokenMap -> Ledger.Value
wbeTokenMapToLedgerValue tokenMap =
    let
        convertTokenName = Ledger.tokenName . unTokenName
        convertCurrencySymbol = Ledger.currencySymbol . getHash . unTokenPolicyId
    in
        mconcat $ fmap
            (\(AssetId wbePolicyId wbeTokenName, TokenQuantity quantity) ->
                Ledger.singleton
                    (convertCurrencySymbol wbePolicyId)
                    (convertTokenName wbeTokenName)
                    (naturalToInteger quantity)
            )
            (toFlatList tokenMap)

data GetTotalFunds = GetTotalFunds { assets :: !Value , sync :: !Double }

getTotalFunds ::
    Monad m =>
    (WalletId -> m WBE.ApiWallet) ->
    WalletId ->
    m GetTotalFunds
getTotalFunds getWallet walletId = do
    wallet <- getWallet walletId
    let WBE.ApiT walletId = WBE.id wallet
    let balance = WBE.balance wallet
    let WBE.ApiT state = WBE.state wallet
    let assets = WBE.assets wallet
    let WBE.ApiT tokenMap = WBE.total (assets :: WBE.ApiWalletAssetsBalance)
    let assetsAsValues = wbeTokenMapToLedgerValue tokenMap
    let adaValue = Ledger.lovelaceValueOf $ naturalToInteger $ getQuantity $ WBE.total (balance :: WBE.ApiWalletBalance)
    let
        syncStatus = case state of
            Ready     -> 1
            Syncing q -> fromRational $ getPercentage $ getQuantity q
            _         -> 0
    pure $ GetTotalFunds (adaValue <> assetsAsValues) syncStatus
