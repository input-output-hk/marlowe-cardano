{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Wallet.V1.CentralizedTestnet.Server
 ( handlers
 )
 where

import Cardano.Mnemonic (SomeMnemonic (..), entropyToMnemonic, genEntropy, mkSomeMnemonic, mnemonicToText)
import Cardano.Prelude hiding (Handler)
import Cardano.Wallet.Api (WalletKeys)
import qualified Cardano.Wallet.Api.Client as WBE.Api
import Cardano.Wallet.Api.Types (ApiVerificationKeyShelley (..))
import qualified Cardano.Wallet.Api.Types as WBE
import Cardano.Wallet.Mock.Types (WalletInfo (..))
import Cardano.Wallet.Primitive.AddressDerivation (Passphrase (Passphrase))
import qualified Cardano.Wallet.Primitive.AddressDerivation as WBE
import qualified Cardano.Wallet.Primitive.Types as WBE
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.String as S
import qualified Data.Text as Text
import Data.Text.Class (FromText (..))
import Ledger (PaymentPubKeyHash (..), PubKeyHash (..))
import Marlowe.Run.Wallet.V1.CentralizedTestnet.API (API)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreatePostData (..), CreateResponse (..), RestoreError (..),
                                                       RestorePostData (..))
import Marlowe.Run.Wallet.V1.Client (callWBE, decodeError)
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant (ServerT, (:<|>) ((:<|>)), (:>))
import Servant.Client (ClientEnv, ClientError (FailureResponse), ClientM, ResponseF (responseBody), client)
import Text.Regex (Regex)
import qualified Text.Regex as Regex
import qualified Wallet.Emulator.Wallet as Pab.Wallet

handlers ::
    MonadIO m =>
    MonadReader ClientEnv m =>
    ServerT API m
handlers = restoreWallet :<|> createWallet

createWallet ::
     MonadIO m =>
     MonadReader ClientEnv m =>
     CreatePostData ->
     m (Maybe CreateResponse)
createWallet postData = runMaybeT $ do
  mnemonic <- liftIO $ entropyToMnemonic <$> genEntropy @256
  let
    walletName = WBE.WalletName $ getCreateWalletName postData
    passphrase = Passphrase $ fromString $ Text.unpack $ getCreatePassphrase postData

  walletId <- MaybeT $ hush <$> createOrRestoreWallet walletName passphrase (SomeMnemonic mnemonic)

  pubKeyHash <- MaybeT $ hush <$> getPubKeyHashFromWallet walletId

  pure $ CreateResponse
    (mnemonicToText mnemonic)
    (WalletInfo{wiWallet=Pab.Wallet.Wallet (Pab.Wallet.WalletId walletId), wiPaymentPubKeyHash = PaymentPubKeyHash pubKeyHash })

-- [UC-WALLET-TESTNET-2][1] Restore a testnet wallet
restoreWallet ::
     MonadIO m =>
     MonadReader ClientEnv m =>
     RestorePostData ->
     m (Either RestoreError WalletInfo)
restoreWallet postData = runExceptT $ do
    let
        phrase = getRestoreMnemonicPhrase postData
        walletName = WBE.WalletName $ getRestoreWalletName postData
        passphrase = Passphrase $ fromString $ Text.unpack $ getRestorePassphrase postData

    -- Try to decode the passphrase into a mnemonic or fail with InvalidMnemonic
    mnemonic <- withExceptT (const InvalidMnemonic)
        ( ExceptT $
            pure (mkSomeMnemonic @'[15, 18, 21, 24] phrase)
        )

    -- Call the WBE trying to restore the wallet, and take error 409 Conflict as a success
    walletId <- withExceptT (const RestoreWalletError)
        (ExceptT $ createOrRestoreWallet walletName passphrase mnemonic)

    -- Get the pubKeyHash of the first wallet derivation
    pubKeyHash <- withExceptT (const FetchPubKeyHashError) $
        ExceptT $ getPubKeyHashFromWallet walletId

    pure $ WalletInfo{wiWallet=Pab.Wallet.Wallet (Pab.Wallet.WalletId walletId), wiPaymentPubKeyHash = PaymentPubKeyHash pubKeyHash }

getPubKeyHashFromWallet ::
    MonadIO m =>
    MonadReader ClientEnv m =>
    WBE.WalletId ->
    m (Either ClientError PubKeyHash)
getPubKeyHashFromWallet walletId = do
  let
    -- This endpoint is not exposed directly by the WBE, I took this helper from the plutus-pab code.
    getWalletKey :: WBE.ApiT WBE.WalletId -> WBE.ApiT WBE.Role -> WBE.ApiT WBE.DerivationIndex -> Maybe Bool -> ClientM WBE.ApiVerificationKeyShelley
    getWalletKey :<|> _ :<|> _ :<|> _ = client (Proxy @("v2" :> WalletKeys))

    -- makeRequest :: String
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
  transformResponse makeRequest

createOrRestoreWallet ::
    MonadIO m =>
    MonadReader ClientEnv m =>
    WBE.WalletName ->
    Passphrase "raw" ->
    SomeMnemonic ->
    m (Either ClientError WBE.WalletId)
createOrRestoreWallet walletName passphrase mnemonic = do
    let
        walletPostData = WBE.WalletOrAccountPostData $ Left $ WBE.WalletPostData
            Nothing
            (WBE.ApiMnemonicT mnemonic)
            Nothing
            (WBE.ApiT walletName)
            (WBE.ApiT passphrase)

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
                    putStrLn "restoreWallet failed"
                    putStrLn $ "Error: " <> show err
                    pure $ Left err
        Left err -> do
            -- FIXME: Define a better logging mechanism
            putStrLn "restoreWallet failed"
            putStrLn $ "Error: " <> show err
            pure $ Left err
        Right WBE.ApiWallet{WBE.id = WBE.ApiT walletId} -> do
            putStrLn $ "Restored wallet: " <> show walletId
            pure $ Right walletId
