{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Marlowe.Run.Contract.V1.Server where

import Cardano.Prelude hiding (Handler, to)
import Colog (logDebug, logWarning)
import Control.Lens (to, (^.))
import Data.Aeson.Text (encodeToLazyText)
import Language.Marlowe.CLI.ChainIndex (queryAssetClassUtxos)
import Marlowe.Run.Contract.V1 (getRoleToken)
import Marlowe.Run.Contract.V1.API (API, ApiT (..))
import Marlowe.Run.Contract.V1.Types (RoleToken, roleTokenUtxoAddress)
import Marlowe.Run.Env (Env (..), HasEnv, withNamespace)
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import Servant (ServerError, ServerT, err400, err404, hoistServer)
import Servant.Client (runClientM)

handlers :: MonadIO m => HasEnv m => MonadError ServerError m => ServerT API m
handlers = hoistServer (Proxy @API) (withNamespace ["contracts", "v1"]) handlers'
  where
  handlers' = handleGetRoleToken

handleGetRoleToken :: MonadIO m
                   => HasEnv m
                   => MonadError ServerError m
                   => ApiT CurrencySymbol
                   -> ApiT TokenName
                   -> m (ApiT RoleToken)
handleGetRoleToken (ApiT currencySymbol) (ApiT tokenName) = withNamespace namespace do
    chainIndexClient <- asks envChainIndexClientEnv
    networkId <- asks envNetworkId
    let
        getUtxos assetClass = do
            result <- liftIO $ runClientM (queryAssetClassUtxos assetClass) chainIndexClient
            case result of
              Left err      -> do
                logWarning $ "chain link query failed: " <> show err
                throwError err400
              Right utxos -> do
                pure utxos
    mRoleToken <- getRoleToken getUtxos networkId currencySymbol tokenName
    case mRoleToken of
      Nothing        -> do
        logWarning "UTxO not found"
        throwError err404
      Just roleToken -> do
        logDebug $ "Role token address: "
          <> (roleToken ^. roleTokenUtxoAddress . to ApiT . to encodeToLazyText . to toStrict)
        pure $ ApiT roleToken
    where
      namespace = [show currencySymbol, "role-tokens", show tokenName, "get:"]
