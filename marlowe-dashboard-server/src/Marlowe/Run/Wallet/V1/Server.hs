{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Marlowe.Run.Wallet.V1.Server where

import Cardano.Prelude hiding (Handler)
import qualified Cardano.Wallet.Api.Client as WBE.Api
import qualified Cardano.Wallet.Api.Types as WBE
import Marlowe.Run.Env (HasEnv)
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse, getTotalFunds)
import Marlowe.Run.Wallet.V1.API (API)
import qualified Marlowe.Run.Wallet.V1.CentralizedTestnet.Server as CentralizedTestnet
import Marlowe.Run.Wallet.V1.Client (callWBE)
import Marlowe.Run.Wallet.V1.Types (WalletId (WalletId))
import Servant (ServerError, ServerT, err404, (:<|>) ((:<|>)))

handlers ::
    MonadIO m =>
    HasEnv m =>
    MonadError ServerError m =>
    ServerT API m
handlers = handleGetTotalFunds :<|> CentralizedTestnet.handlers

handleGetTotalFunds ::
    MonadIO m =>
    MonadError ServerError m =>
    HasEnv m =>
    WalletId ->
    m GetTotalFundsResponse
handleGetTotalFunds (WalletId  walletId) =
    getTotalFunds
        ( either (const $ throwError err404) pure
            <=< callWBE . WBE.Api.getWallet WBE.Api.walletClient . WBE.ApiT
        )
        walletId
