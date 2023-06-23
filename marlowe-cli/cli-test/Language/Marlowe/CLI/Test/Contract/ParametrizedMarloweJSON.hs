{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON where

import Cardano.Api (AddressInEra, CardanoMode, LocalNodeConnectInfo)
import qualified Contrib.Data.Aeson.Traversals as A
import Contrib.Data.Time.Clock (nominalDiffTimeToMilliseconds)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, gets)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import Data.Bifunctor (Bifunctor(first))
import Data.Functor ((<&>))
import Data.Has (Has(getter))
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy)
import qualified Data.Text as Text
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Generics (Generic)
import qualified Language.Marlowe as Marlowe
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currencies(Currencies)
  , Currency(Currency, ccCurrencySymbol)
  , CurrencyNickname(CurrencyNickname)
  , Wallet(_waAddress)
  , WalletNickname(WalletNickname)
  , Wallets
  , getAllWallets
  )
import Language.Marlowe.CLI.Types (CliError(CliError))
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Marlowe
import Ledger.Address (toPlutusAddress)
import qualified Plutus.V1.Ledger.Value as PV

-- | Either a JSON of the Input or a JSON of the Contract which
-- | is parametrized by the currencies and wallets.
newtype ParametrizedMarloweJSON = ParametrizedMarloweJSON { unParametrizedMarloweJSON :: A.Value }
    deriving stock (Eq, Ord, Generic, Show)

instance FromJSON ParametrizedMarloweJSON where
  parseJSON = pure . ParametrizedMarloweJSON

instance ToJSON ParametrizedMarloweJSON where
  toJSON (ParametrizedMarloweJSON json) = json

rewriteCurrencyRefs :: Currencies -> ParametrizedMarloweJSON -> Either CurrencyNickname ParametrizedMarloweJSON
rewriteCurrencyRefs (Currencies currencies) (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    findCurrency nickname = case M.lookup nickname currencies of
      Nothing -> Left nickname
      Just currency -> pure currency
    rewrite = \case
      obj@(A.Object (KeyMap.toAscList -> props)) -> do
        case props of
          [("currency_symbol", A.String "") , ("token_name", A.String "")] -> pure obj
          [("currency_symbol", A.String currencyNickname) , ("token_name", tokenName)] -> do
              let
                nickname = Text.unpack currencyNickname
              Currency { ccCurrencySymbol=PV.CurrencySymbol cs } <- findCurrency (CurrencyNickname nickname)
              pure $ A.object
                [
                  ("currency_symbol", A.toJSON cs)
                , ("token_name", tokenName)
                ]
          _ -> pure obj
      v -> do
        pure v

data RewritePartyError era = WalletNotFound WalletNickname | InvalidWalletAddress WalletNickname (AddressInEra era)
  deriving stock (Eq, Generic, Show)

rewritePartyRefs :: Marlowe.Network -> Wallets era -> ParametrizedMarloweJSON -> Either (RewritePartyError era) ParametrizedMarloweJSON
rewritePartyRefs network wallets (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    findWallet nickname = case M.lookup nickname $ getAllWallets wallets of
      Nothing -> Left $ WalletNotFound nickname
      Just wallet -> pure wallet
    rewrite = \case
      A.Object (KeyMap.toList -> [("address", A.String walletNickname)]) -> do
        wallet <- findWallet (WalletNickname $ Text.unpack walletNickname)
        let
          address = toPlutusAddress . _waAddress $ wallet
        pure $ A.toJSON (Marlowe.Address network address)
      v -> do
        pure v

newtype Now = Now POSIXTime

rewriteTimeouts :: Monad m => Now -> ParametrizedMarloweJSON -> m ParametrizedMarloweJSON
rewriteTimeouts (Now n) (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    rewrite = \case
      A.Object (KeyMap.toList -> [("relative", A.Number difference)]) -> do
        let
          -- Diff is expressed in seconds
          diff :: NominalDiffTime
          diff = fromInteger (ceiling difference)

          t = n + diff
        pure $ A.toJSON $ nominalDiffTimeToMilliseconds t
      v -> pure v

rewriteParametrizedMarloweJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) ParametrizedMarloweJSON
rewriteParametrizedMarloweJSON network wallets currencies n json = do
  json' <- first RewritePartyFailure $ rewritePartyRefs network wallets json
  json'' <- first CurrencyNotFound $ rewriteCurrencyRefs currencies json'
  rewriteTimeouts n json''

data ParametrizedMarloweJSONDecodeError era =
    RewritePartyFailure (RewritePartyError era)
  | CurrencyNotFound CurrencyNickname
  | InvalidMarloweJSON String String
  deriving stock (Eq, Generic, Show)

decodeParametrizedContractJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) Marlowe.Contract
decodeParametrizedContractJSON network wallets currencies n json = do
  rewriteParametrizedMarloweJSON network wallets currencies n json >>= \(ParametrizedMarloweJSON contractJSON) ->
    case A.fromJSON contractJSON of
      A.Error err -> Left $ InvalidMarloweJSON err (Text.unpack $ A.renderValue contractJSON)
      A.Success contract -> pure contract

decodeParametrizedInputJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) Marlowe.Input
decodeParametrizedInputJSON network wallets currencies n json = do
  rewriteParametrizedMarloweJSON network wallets currencies n json >>= \(ParametrizedMarloweJSON contractJSON) ->
    case A.fromJSON contractJSON of
      A.Error err -> Left $ InvalidMarloweJSON err (Text.unpack $ A.renderValue contractJSON)
      A.Success input -> pure input

doRewriteParametrizedMarloweJSON
  :: forall env era st m
   . MonadIO m
  => MonadError CliError m
  => MonadReader env m
  => Has (LocalNodeConnectInfo CardanoMode) env
  => MonadState st m
  => Has (Wallets era) st
  => Has Currencies st
  => Proxy era
  -> ParametrizedMarloweJSON
  -> m ParametrizedMarloweJSON
doRewriteParametrizedMarloweJSON _ json = do
  network <- (asks getter :: m (LocalNodeConnectInfo CardanoMode)) <&> marloweNetworkFromLocalNodeConnectInfo
  (wallets :: Wallets era) <- gets getter
  (currencies :: Currencies) <- gets getter
  n <- now
  case rewriteParametrizedMarloweJSON network wallets currencies n json of
    Left err -> throwError $ CliError $ "Error rewriting parametrized Marlowe JSON: " <> show err
    Right json' -> pure json'

now :: MonadIO m => m Now
now = liftIO $ Now <$> getPOSIXTime
