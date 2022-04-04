{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Marlowe.Run.Contract.V1.Server where

import Cardano.Prelude hiding (Handler)
import Language.Marlowe.CLI.ChainIndex (queryAssetClassUtxos)
import Marlowe.Run.Contract.V1 (getRoleToken)
import Marlowe.Run.Contract.V1.API (API, ApiT (ApiT))
import Marlowe.Run.Contract.V1.Types (RoleToken)
import Marlowe.Run.Env (Env (..), HasEnv)
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import Servant (ServerError, ServerT, err400, err404)
import Servant.Client (runClientM)

handlers :: MonadIO m => HasEnv m => MonadError ServerError m => ServerT API m
handlers = handleGetRoleToken

handleGetRoleToken :: MonadIO m
                   => HasEnv m
                   => MonadError ServerError m
                   => ApiT CurrencySymbol
                   -> ApiT TokenName
                   -> m (ApiT RoleToken)
handleGetRoleToken (ApiT currencySymbol) (ApiT tokenName) = do
    chainIndexClient <- asks envChainIndexClientEnv
    networkId <- asks envNetworkId
    let
        getUtxos assetClass = do
            result <- liftIO $ runClientM (queryAssetClassUtxos assetClass) chainIndexClient
            case result of
              Left _      -> throwError err400
              Right utxos -> pure utxos
    mRoleToken <- getRoleToken getUtxos networkId currencySymbol tokenName
    case mRoleToken of
      Nothing        -> throwError err404
      Just roleToken -> pure $ ApiT roleToken
