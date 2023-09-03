{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.CLI.Test.ExecutionMode.TrackedUTxOs (
  TrackedUTxOs,
  toUTxO,
  fromUTxO,
  unTrackedUTxOs,
  trackedUTxOs,
)
where

import Cardano.Api qualified as C
import Data.Map.Strict qualified as Map
import Language.Marlowe.CLI.Types (AnUTxO (..))

newtype TrackedUTxOs era = TrackedUTxOs [AnUTxO era]
  deriving stock (Eq, Show)

instance Semigroup (TrackedUTxOs era) where
  TrackedUTxOs a <> TrackedUTxOs b = do
    let a' = Map.fromList $ fmap unAnUTxO a
        b' = Map.fromList $ fmap unAnUTxO b
    TrackedUTxOs $ AnUTxO <$> (Map.toList $ a' <> b')

instance Monoid (TrackedUTxOs era) where
  mempty = TrackedUTxOs mempty

toUTxO :: TrackedUTxOs era -> C.UTxO era
toUTxO (TrackedUTxOs utxos) = C.UTxO $ Map.fromList $ fmap unAnUTxO utxos

fromUTxO :: C.UTxO era -> TrackedUTxOs era
fromUTxO (C.UTxO utxos) = TrackedUTxOs $ AnUTxO <$> Map.toList utxos

unTrackedUTxOs :: TrackedUTxOs era -> [AnUTxO era]
unTrackedUTxOs (TrackedUTxOs utxos) = utxos

trackedUTxOs :: [AnUTxO era] -> TrackedUTxOs era
trackedUTxOs utxos = TrackedUTxOs $ do
  let utxos' = map AnUTxO . Map.toList . Map.fromList $ fmap unAnUTxO utxos
  utxos'
