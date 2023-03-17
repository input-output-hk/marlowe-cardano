{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Tooling around contracts.
--
-----------------------------------------------------------------------------

module Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
  where


import Cardano.Api
  ( AddressInEra(AddressInEra)
  , CardanoMode
  , IsShelleyBasedEra
  , LocalNodeConnectInfo(LocalNodeConnectInfo)
  , NetworkId(Mainnet)
  , ScriptDataSupportedInEra
  )
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import qualified Cardano.Ledger.BaseTypes as LC (Network(..))
import Cardano.Ledger.Shelley.API (Network)
import qualified Contrib.Data.Aeson.Traversals as A
import Control.Error (note)
import Control.Error.Util (hush)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, gets)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import Data.Bifunctor (Bifunctor(first))
import Data.Functor ((<&>))
import Data.Has (Has(getter))
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Language.Marlowe as Marlowe
import Language.Marlowe.CLI.IO (liftCli)
import Language.Marlowe.CLI.Test.CLI.Monad (runCli)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currencies(Currencies)
  , Currency(Currency, ccCurrencySymbol)
  , CurrencyNickname(CurrencyNickname)
  , Wallet(waAddress)
  , WalletNickname(WalletNickname)
  , Wallets(Wallets)
  )
import Language.Marlowe.CLI.Types (CliError(CliError))
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Marlowe
import Ledger.Tx.CardanoAPI (fromCardanoAddressInEra)
import qualified Plutus.V1.Ledger.Value as PV

-- Either an JSON of the Input or a JSON of the Contract.

newtype ParametrizedMarloweJSON = ParametrizedMarloweJSON { unParametrizedMarloweJSON :: A.Value }
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (A.FromJSON, A.ToJSON)


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

newtype OnMainnet = OnMainnet Bool

rewritePartyRefs :: Marlowe.Network -> Wallets era -> ParametrizedMarloweJSON -> Either (RewritePartyError era) ParametrizedMarloweJSON
rewritePartyRefs network (Wallets wallets) (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    findWallet nickname = case M.lookup nickname wallets of
      Nothing -> Left $ WalletNotFound nickname
      Just wallet -> pure wallet
    rewrite = \case
      A.Object (KeyMap.toList -> [("address", A.String walletNickname)]) -> do
        wallet <- findWallet (WalletNickname $ Text.unpack walletNickname)
        let
          address = waAddress wallet
        address' <- first (const $ InvalidWalletAddress (WalletNickname $ Text.unpack walletNickname) address) $ fromCardanoAddressInEra address
        pure $ A.toJSON (Marlowe.Address network address')
      v -> do
        pure v

rewriteParametrizedMarloweJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) ParametrizedMarloweJSON
rewriteParametrizedMarloweJSON network wallets currencies json = do
  json' <- first RewritePartyFailure $ rewritePartyRefs network wallets json
  first CurrencyNotFound $ rewriteCurrencyRefs currencies json'

data ParametrizedMarloweJSONDecodeError era =
    RewritePartyFailure (RewritePartyError era)
  | CurrencyNotFound CurrencyNickname
  | InvalidMarloweJSON String String
  deriving stock (Eq, Generic, Show)

decodeParametrizedContractJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) Marlowe.Contract
decodeParametrizedContractJSON network wallets currencies json = do
  rewriteParametrizedMarloweJSON network wallets currencies json >>= \(ParametrizedMarloweJSON contractJSON) ->
    case A.fromJSON contractJSON of
      A.Error err -> Left $ InvalidMarloweJSON err (Text.unpack $ A.renderValue contractJSON)
      A.Success contract -> pure contract

decodeParametrizedInputJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) Marlowe.Input
decodeParametrizedInputJSON network wallets currencies json = do
  rewriteParametrizedMarloweJSON network wallets currencies json >>= \(ParametrizedMarloweJSON contractJSON) ->
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
  case rewriteParametrizedMarloweJSON network wallets currencies json of
    Left err -> throwError $ CliError $ "Error rewriting parametrized Marlowe JSON: " <> show err
    Right json' -> pure json'

-- doDecodeParametrizedContractJSON
--   :: forall env era st m
--    . MonadIO m
--   => MonadError CliError m
--   => MonadReader env m
--   => Has (LocalNodeConnectInfo CardanoMode) env
--   => MonadState st m
--   => Has (Wallets era) st
--   => Has Currencies st
--   => Proxy era
--   -> ParametrizedMarloweJSON
--   -> m Marlowe.Contract
-- doDecodeParametrizedContractJSON p json = do
--   (doRewriteParametrizedMarloweJSON p json :: m ParametrizedMarloweJSON) >>= \(ParametrizedMarloweJSON contractJSON) ->
--     case A.fromJSON contractJSON of
--       A.Error err -> throwError $ CliError $ "Error decoding parametrized Marlowe Contract: " <> err <> ". JSON: " <> (Text.unpack $ A.renderValue contractJSON)
--       A.Success contract -> pure contract
--
--
-- doDecodeParametrizedInputJSON
--   :: forall env era st m
--    . MonadIO m
--   => MonadError CliError m
--   => MonadReader env m
--   => Has (LocalNodeConnectInfo CardanoMode) env
--   => MonadState st m
--   => Has (Wallets era) st
--   => Has Currencies st
--   => Proxy era
--   -> ParametrizedMarloweJSON
--   -> m Marlowe.Input
-- doDecodeParametrizedInputJSON p json = do
--   (doRewriteParametrizedMarloweJSON p json :: m ParametrizedMarloweJSON) >>= \(ParametrizedMarloweJSON contractJSON) ->
--     case A.fromJSON contractJSON of
--       A.Error err -> throwError $ CliError $ "Error decoding parametrized Marlowe Input: " <> err <> ". JSON: " <> (Text.unpack $ A.renderValue contractJSON)
--       A.Success contract -> pure contract
--
