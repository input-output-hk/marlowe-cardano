{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Report (
  rewriteAddress,
) where

import Cardano.Api qualified as C
import Contrib.Data.Aeson.Traversals qualified as A
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Functor ((<&>))
import Data.Functor.Identity (runIdentity)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Language.Marlowe.CLI.Test.Wallet.Types
import Language.Marlowe.CLI.Test.Wallet.Types qualified as W

-- Turns:
-- "address": BECH32
--
-- Into
-- "address": ["BECH32", WalletNickname]
-- or
-- "address": ["BECH32", null]
--  if no nickname is found
rewriteAddress :: forall era. (C.IsCardanoEra era) => Wallets era -> A.Value -> A.Value
rewriteAddress wallets = runIdentity . A.rewriteBottomUp rewrite
  where
    wallets' = Map.toList $ W.allWalletsMap wallets
    address2Nickname =
      Map.fromList $
        wallets' <&> \(WalletNickname nickname, Wallet{_waAddress}) -> do
          let b32 = C.serialiseAddress _waAddress
          (b32, nickname)

    addrKey = Key.fromString "address"
    rewrite json' = pure $ case json' of
      A.Object (Map.fromList . KeyMap.toAscList -> props) -> do
        case Map.lookup addrKey props of
          Just (A.String b32) -> do
            let props' = case Map.lookup b32 address2Nickname of
                  Just nickname -> do
                    Map.insert addrKey (A.toJSON [A.String b32, A.String $ T.pack nickname]) props
                  Nothing -> do
                    Map.insert addrKey (A.toJSON [A.String b32, A.Null]) props
            A.object $ Map.toList props'
          _ -> json'
      _ -> json'
