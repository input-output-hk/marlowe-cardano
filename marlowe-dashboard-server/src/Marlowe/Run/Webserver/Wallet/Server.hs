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
-- import Cardano.Wallet.Primitive.Types (WalletId)
-- FIXME: I don't like to use a Emulator type here, but we'd need to publish some changes upstream to the PAB to fix this
import           Wallet.Emulator                                        (WalletId (..))

import           Cardano.Wallet.Primitive.SyncProgress                  (SyncProgress (..))
import           Data.Quantity                                          (getPercentage, getQuantity)
import           GHC.Natural                                            (naturalToInteger)
import           Marlowe.Run.Webserver.Wallet.Client                    (callWBE)


handlers ::
    MonadIO m =>
    MonadReader Env m =>
    ServerT API m
handlers = getTotalFunds :<|> CentralizedTestnet.handlers

getTotalFunds ::
    MonadIO m =>
    MonadReader Env m =>
    WalletId ->
    m GetTotalFunds
getTotalFunds (WalletId walletId) = do

    result <- callWBE $ WBE.Api.getWallet WBE.Api.walletClient (WBE.ApiT walletId)
    case result of
        Left err -> pure $ GetTotalFunds 0 0
        Right WBE.ApiWallet{WBE.id = WBE.ApiT walletId, WBE.balance = balance, WBE.state = WBE.ApiT state} ->
            let
                totalBalance = naturalToInteger $ getQuantity $ WBE.total (balance :: WBE.ApiWalletBalance)
                syncStatus = case state of
                    Ready     -> 1
                    Syncing q -> fromRational $ getPercentage $ getQuantity q
                    _         -> 0
            in
                pure $ GetTotalFunds totalBalance syncStatus
