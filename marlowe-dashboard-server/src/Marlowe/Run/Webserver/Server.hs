{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
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
 ( handlers
 , initializeServerContext
 , WBEConfig(..)
 , AppConfig(..)
 )
 where

import           Cardano.Mnemonic                           (mkSomeMnemonic)
import           Cardano.Prelude                            hiding (Handler)
import qualified Cardano.Wallet.Api.Client                  as WBE.Api
import           Cardano.Wallet.Api.Types                   (ApiVerificationKeyShelley (..))
import qualified Cardano.Wallet.Api.Types                   as WBE
import           Marlowe.Run.Webserver.API                  (API)
import           Prelude                                    (userError)

import           Cardano.Wallet.Api                         (WalletKeys)
import           Cardano.Wallet.Mock.Types                  (WalletInfo (..))
import qualified Cardano.Wallet.Primitive.AddressDerivation as WBE
import qualified Cardano.Wallet.Primitive.Types             as WBE

import           Data.Aeson                                 as Aeson
import qualified Data.Aeson.Types                           as Aeson

import           Cardano.Wallet.Primitive.AddressDerivation (Passphrase (Passphrase))
import qualified Data.ByteString.Lazy                       as BL

import           Data.String                                as S
import qualified Data.Text                                  as Text
import           Data.Text.Class                            (FromText (..))
import           Data.Version                               (showVersion)
import           Ledger                                     (PubKeyHash (..))
import           Marlowe.Run.Webserver.Types                (Env, RestoreError (..), RestorePostData (..))
import qualified Marlowe.Run.Webserver.WebSocket            as WS
import qualified Paths_marlowe_dashboard_server             as Package.Paths
import           PlutusTx.Builtins.Internal                 (BuiltinByteString (..))
import           Servant                                    (Handler (Handler), Server, ServerError, hoistServer,
                                                             serveDirectoryFileServer, (:<|>) ((:<|>)), (:>))
import           Servant.Client                             (ClientError (FailureResponse), ClientM,
                                                             ResponseF (responseBody), client, runClientM)
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

handleVersion :: Applicative m => m Text
handleVersion = pure $ Text.pack $ showVersion Package.Paths.version

callWBE :: MonadIO m => MonadReader Env m => ClientM a -> m (Either ClientError a)
callWBE client = do
    clientEnv <- ask
    liftIO $ runClientM client clientEnv

-- [UC-WALLET-TESTNET-2][1] Restore a testnet wallet
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
    walletId <- withExceptT (const RestoreWalletError)
        (ExceptT $ createOrRestoreWallet postData mnemonic)

    -- Get the pubKeyHash of the first wallet derivation
    pubKeyHash <- withExceptT (const FetchPubKeyHashError) $
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
    m (Either ClientError WBE.WalletId)
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
        Left err@(FailureResponse _ r) -> do
            let
                matchDuplicateWallet :: Regex
                matchDuplicateWallet = Regex.mkRegex "wallet with the following id: ([a-z0-9]{40})"

                mWalletIdFromErr :: Maybe WBE.WalletId
                mWalletIdFromErr = do
                    msg <- decodeError $ responseBody r
                    matches <- Regex.matchRegex matchDuplicateWallet $ Text.unpack msg
                    walletIdStr <- headMay matches
                    rightToMaybe $ fromText $ Text.pack walletIdStr
            case mWalletIdFromErr of
                Just walletId -> pure $ Right walletId
                Nothing       ->  do
                    putStrLn ("restoreWallet failed" :: Text)
                    putStrLn $ "Error: " <> (show err :: Text)
                    pure $ Left err
        Left err -> do
            -- FIXME: Define a better logging mechanism
            putStrLn ("restoreWallet failed" :: Text)
            putStrLn $ "Error: " <> (show err :: Text)
            pure $ Left err
        Right (WBE.ApiWallet (WBE.ApiT walletId) _ _ _ _ _ _ _ _) -> do
            putStrLn $ "Restored wallet: " <> (show walletId :: Text)
            pure $ Right walletId

-- NOTE: This was copied from Cardano-wallet/Cardano.Cli
decodeError
    :: BL.ByteString
    -> Maybe Text
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj

data WBEConfig = WBEConfig { _wbeHost :: String, _wbePort :: Int }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)

data AppConfig = AppConfig { getWbeConfig :: WBEConfig, getStaticPath :: FilePath }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON)

initializeServerContext :: FilePath -> IO AppConfig
initializeServerContext configPath = do
    mConfig <- decodeFileStrict configPath
    case mConfig of
        Just config -> pure config
        Nothing     -> ioError $ userError "Config file has invalid format"
