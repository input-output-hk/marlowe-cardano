{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Wallet.Server
 ( handlers
 )
 where

import           Cardano.Prelude                                        hiding (Handler)


import qualified Cardano.Wallet.Api.Client                              as WBE.Api
import qualified Cardano.Wallet.Api.Types                               as WBE
import           Marlowe.Run.Webserver.Types                            (Env)
import           Marlowe.Run.Webserver.Wallet.API                       (API)
import qualified Marlowe.Run.Webserver.Wallet.CentralizedTestnet.Server as CentralizedTestnet
import           Marlowe.Run.Webserver.Wallet.Types                     (GetTotalFunds (..))
import           Servant                                                (ServerT, (:<|>) ((:<|>)))
-- FIXME: I don't like to use a Emulator type here, but we'd need to publish some changes upstream to the PAB to fix this
import           Wallet.Emulator                                        (WalletId (..))

import           Cardano.Wallet.Primitive.SyncProgress                  (SyncProgress (..))
import           Cardano.Wallet.Primitive.Types.Hash                    (getHash)
import           Cardano.Wallet.Primitive.Types.TokenMap                (AssetId (..), TokenMap, toFlatList)
import           Cardano.Wallet.Primitive.Types.TokenPolicy             (unTokenName, unTokenPolicyId)
import           Cardano.Wallet.Primitive.Types.TokenQuantity           (TokenQuantity (..))
import           Data.Quantity                                          (getPercentage, getQuantity)
import           GHC.Natural                                            (naturalToInteger)
import           Marlowe.Run.Webserver.Wallet.Client                    (callWBE)
import qualified Plutus.V1.Ledger.Ada                                   as Ledger
import qualified Plutus.V1.Ledger.Value                                 as Ledger

handlers ::
    MonadIO m =>
    MonadReader Env m =>
    ServerT API m
handlers = getTotalFunds :<|> CentralizedTestnet.handlers

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

getTotalFunds ::
    MonadIO m =>
    MonadReader Env m =>
    WalletId ->
    m GetTotalFunds
getTotalFunds (WalletId walletId) = do
    result <- callWBE $ WBE.Api.getWallet WBE.Api.walletClient (WBE.ApiT walletId)
    case result of
        Left err -> pure $ GetTotalFunds mempty 0
        Right WBE.ApiWallet{WBE.id = WBE.ApiT walletId, WBE.balance = balance, WBE.state = WBE.ApiT state, WBE.assets = assets} ->
            let
                WBE.ApiT tokenMap = WBE.total (assets :: WBE.ApiWalletAssetsBalance)

                assetsAsValues = wbeTokenMapToLedgerValue tokenMap

                adaValue = Ledger.lovelaceValueOf $ naturalToInteger $ getQuantity $ WBE.total (balance :: WBE.ApiWalletBalance)

                syncStatus = case state of
                    Ready     -> 1
                    Syncing q -> fromRational $ getPercentage $ getQuantity q
                    _         -> 0
            in
                pure $ GetTotalFunds (adaValue <> assetsAsValues) syncStatus
