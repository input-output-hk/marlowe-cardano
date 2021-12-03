{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Server
 (
    handlers
 )
 where

import           Cardano.Mnemonic                           (MkSomeMnemonicError, mkSomeMnemonic)
import qualified Cardano.Wallet.Api.Client                  as WBE.Api
import           Cardano.Wallet.Api.Types                   (ApiVerificationKeyShelley (..))
import qualified Cardano.Wallet.Api.Types                   as WBE
import           Marlowe.Run.Webserver.API                  (API)

import           Cardano.Wallet.Api                         (WalletKeys)
import           Cardano.Wallet.Mock.Types                  (WalletInfo (..))
import qualified Cardano.Wallet.Primitive.AddressDerivation as WBE
import qualified Cardano.Wallet.Primitive.Types             as WBE

import           Control.Monad.Except                       (ExceptT (ExceptT), runExceptT, withExceptT)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Logger                       (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import           Control.Monad.Reader                       (MonadReader (ask), ReaderT, runReaderT)
import           Data.Aeson                                 (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Aeson                                 as Aeson
import qualified Data.Aeson.Types                           as Aeson

import           Cardano.Wallet.Primitive.AddressDerivation (Passphrase (Passphrase))
import           Control.Monad.Reader.Class
import qualified Data.ByteString.Lazy                       as BL
import           Data.Maybe                                 (fromMaybe)
import           Data.Proxy                                 (Proxy (Proxy))
import           Data.String                                as S
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Text.Class                            (FromText (..))
import qualified Data.Text.Encoding                         as Text
import           GHC.Generics                               (Generic)
import           Ledger                                     (PubKeyHash (..))
import           Marlowe.Run.Webserver.Types                (Env, RestoreError (..), RestorePostData (..))
import qualified Marlowe.Run.Webserver.WebSocket            as WS
import           Network.HTTP.Client                        (defaultManagerSettings, newManager)
import           Network.Wai.Middleware.Cors                (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import           PlutusTx.Builtins.Internal                 (BuiltinByteString (..))
import           Servant                                    (Application, Handler (Handler), Server, ServerError,
                                                             errBody, hoistServer, serve, serveDirectoryFileServer,
                                                             (:<|>) ((:<|>)), (:>))
import           Servant.Client                             (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme),
                                                             ClientEnv (manager), ClientError (FailureResponse),
                                                             ClientM, ResponseF (responseBody), Scheme (Http), client,
                                                             mkClientEnv, runClientM)
import           Text.Regex                                 (Regex)
import qualified Text.Regex                                 as Regex
import qualified Wallet.Emulator.Wallet                     as Pab.Wallet

handlers :: FilePath -> Env -> Server API
handlers staticPath env =
    hoistServer (Proxy @API) liftHandler
        ( WS.handle
            :<|> (handleVersion
                   :<|> restoreWallet
                 )
            :<|> serveDirectoryFileServer staticPath
        )
    where
    liftHandler :: ReaderT Env (ExceptT ServerError IO) a -> Handler a
    liftHandler = Handler . flip runReaderT env

-- TODO: Can we get this from cabal somehow?
handleVersion :: Applicative m => m Text
handleVersion = pure "1.0.0.0"

callWBE :: MonadIO m => MonadReader Env m => ClientM a -> m (Either ClientError a)
callWBE client = do
    clientEnv <- ask
    liftIO $ runClientM client clientEnv

restoreWallet ::
     MonadIO m =>
     MonadReader Env m =>
     RestorePostData ->
     m (Either RestoreError WalletInfo)
restoreWallet postData = runExceptT $ do
    let
        phrase = getMnemonicPhrase postData

    -- Try to decode the passphrase into a mnemonic or fail with InvalidMnemonic
    mnemonic <- withExceptT (const InvalidMnemonic)
        ( ExceptT $
            pure (WBE.ApiMnemonicT <$> mkSomeMnemonic @'[15, 18, 21, 24] phrase)
        )
    -- Call the WBE trying to restore the wallet, and take error 409 Conflict as a success
    walletId <- ExceptT $ createOrRestoreWallet postData mnemonic
    -- Get the pubKeyHash of the first wallet derivation
    pubKeyHash <- withExceptT (const CantFetchPubKeyHash) $
        ExceptT $ getPubKeyHashFromWallet walletId
    pure $ WalletInfo{wiWallet=Pab.Wallet.Wallet (Pab.Wallet.WalletId walletId), wiPubKeyHash = pubKeyHash }

getPubKeyHashFromWallet ::
    MonadIO m =>
    MonadReader Env m =>
    WBE.WalletId ->
    m (Either ClientError PubKeyHash)
getPubKeyHashFromWallet walletId = let
    -- This endpoint is not exposed directly by the WBE, I took this helper from the plutus-pab code.
    getWalletKey :: WBE.ApiT WBE.WalletId -> WBE.ApiT WBE.Role -> WBE.ApiT WBE.DerivationIndex -> Maybe Bool -> ClientM WBE.ApiVerificationKeyShelley
    getWalletKey :<|> _ :<|> _ :<|> _ = client (Proxy @("v2" :> WalletKeys))

    makeRequest = callWBE $
       getWalletKey
        (WBE.ApiT walletId)
        -- Role: External, to receive funds
        (WBE.ApiT WBE.UtxoExternal)
        -- DerivationIndex: first address
        (WBE.ApiT (WBE.DerivationIndex 0))
        -- Return hashed version
        (Just True)

    transformResponse = (fmap . fmap) (PubKeyHash . BuiltinByteString . fst . getApiVerificationKey)
  in
    transformResponse makeRequest

createOrRestoreWallet ::
    MonadIO m =>
    MonadReader Env m =>
    RestorePostData ->
    WBE.ApiMnemonicT '[15, 18, 21, 24] ->
    m (Either RestoreError WBE.WalletId)
createOrRestoreWallet postData mnemonic = do
    let
        passphrase = Text.unpack $ getPassphrase postData
        walletName = getWalletName postData

        walletPostData = WBE.WalletOrAccountPostData $ Left $ WBE.WalletPostData
            Nothing
            mnemonic
            Nothing
            (WBE.ApiT $ WBE.WalletName walletName)
            (WBE.ApiT $ Passphrase $ fromString passphrase )

    result <- callWBE $ WBE.Api.postWallet WBE.Api.walletClient walletPostData
    case result of
        Left (FailureResponse _ r) -> do
            let
                matchDuplicateWallet :: Regex
                matchDuplicateWallet = Regex.mkRegex "wallet with the following id: ([a-z0-9]{40})"

                mWalletIdFromErr :: Maybe WBE.WalletId
                mWalletIdFromErr = do
                    msg <- decodeError $ responseBody r
                    matches <- Regex.matchRegex matchDuplicateWallet msg
                    walletIdStr <- safeHead matches
                    hush $ fromText $ Text.pack walletIdStr
            case mWalletIdFromErr of
                Just walletId -> pure $ Right walletId
                Nothing       -> pure $ Left UnknownRestoreError
        Left err -> do
            -- FIXME: How do we want to handle logging
            liftIO $ putStrLn "restoreWallet failed"
            liftIO $ putStrLn $ "Error: " <> show err
            pure $ Left UnknownRestoreError
        Right (WBE.ApiWallet (WBE.ApiT walletId) _ _ _ _ _ _ _ _) -> do
            liftIO $ putStrLn $ "Restored wallet: " <> show walletId
            pure $ Right walletId

-- NOTE: This was copied from Cardano-wallet/Cardano.Cli and changed return type from Text to String
decodeError
    :: BL.ByteString
    -> Maybe String
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj

-- NOTE: Coppied from plutus-apps/playground-common/Schema.hs with same note
-- We could take this from the `safe` package, but I don't think it's worth the extra dependency.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
