{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.CLI.Test.ExecutionMode (
  module Language.Marlowe.CLI.Test.ExecutionMode.ExecutionMode,
  queryUTxOs,
  queryByAddress,
  -- , getNetworkId
  -- , runTxSubmitterFn
  -- , TxSubmitterFn(..)
  HasInterpretEnv (..),
  InterpretMonad,
)
where

import Cardano.Api qualified as C
import Control.Lens (Lens', view)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (..))
import Language.Marlowe.CLI.Test.CLI.Monad (runCli)

import Control.Lens.Getter (Getter)
import Control.Monad.Reader.Class (MonadReader)
import Data.Set qualified as Set
import Language.Marlowe.CLI.IO qualified as CLI.IO
import Language.Marlowe.CLI.Test.ExecutionMode.ExecutionMode
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError)
import Language.Marlowe.CLI.Types (TxBuildupContext, toQueryContext)
import Language.Marlowe.CLI.Types qualified as CT

class HasInterpretEnv env era | env -> era where
  eraL :: Lens' env (C.ScriptDataSupportedInEra era)
  txBuildupContextL :: Getter env (TxBuildupContext era)

type InterpretMonad env m era =
  ( MonadReader env m
  , HasInterpretEnv env era
  , MonadError InterpreterError m
  , MonadIO m
  )

queryUTxOs
  :: (InterpretMonad env m era)
  => C.QueryUTxOFilter
  -- ^ The query.
  -> m (C.UTxO era)
  -- ^ Action for running the query.
queryUTxOs queryFilter = do
  ctx <- view txBuildupContextL
  let queryCtx = toQueryContext ctx
  era <- view eraL
  runCli era "queryUTxOs" $ CLI.IO.queryUTxOs queryCtx queryFilter

queryByAddress
  :: (InterpretMonad env m era)
  => C.AddressInEra era
  -- ^ Address to query.
  -> m (C.UTxO era)
  -- ^ Action for running the query.
queryByAddress = queryUTxOs . C.QueryUTxOByAddress . Set.singleton . CT.toAddressAny'

-- We want to wrap execution of every tx creation and submission in our execution context
-- when we are running in a simulation mode.
-- data TxSubmitterFn t m era = TxSubmitterFn
--   [C.AddressInEra era]
--   ( TxBuildupContext era
--     -> CT.SubmitMode
--     -> Maybe (C.UTxO era)
--     -> m (t (C.TxBody era))
--   )
--
---- Given a function which expects a possible
-- runTxSubmitterFn
--  :: Traversable t
--  => InterpretMonad env m era
--  => TxSubmitterFn t m era
--  -> m (t (C.TxBody era))
-- runTxSubmitterFn (TxSubmitterFn utxoProviders txSubmitterFn) = do
--  ConnectionWrapper connection <- view connectionL
--  view txBuildupContextL >>= \case
--    SimulationMode utxosVar -> do
--      let
--        utxoProviders' = map CT.toAddressAny' utxoProviders
--      submitterUTxOs <- queryUTxOs (C.QueryUTxOByAddress $ Set.fromList utxoProviders')
--      liftIO $ hPutStrLn stderr $ "Submitter utxos: " <> show (map fst $ U.toList submitterUTxOs)
--      txBodies <- txSubmitterFn connection CT.DontSubmit (Just submitterUTxOs)
--      submissionResult <- liftIO $ runExceptT $ for txBodies \txBody -> do
--        let
--          txId = C.getTxId txBody
--          C.TxBody txBodyContent = txBody
--
--          txIns = map fst . C.txIns $ txBodyContent
--          txInsRef = case C.txInsReference txBodyContent of
--            C.TxInsReferenceNone -> []
--            C.TxInsReference _ ins -> ins
--          txOuts = map C.toCtxUTxOTxOut . C.txOuts $ txBodyContent
--
--          newUTxOs = zip [0..] txOuts <&> \(txIx, txOut) -> (C.TxIn txId (C.TxIx txIx), txOut)
--
--        liftIO $ hPutStrLn stderr $ "NEW UTxOs" <> show newUTxOs
--        liftIO $ hPutStrLn stderr $ "txIns" <> show txIns
--        liftIO $ hPutStrLn stderr $ "txInsRef" <> show txInsRef
--        ExceptT $ S.atomically do
--          utxos <- S.readTVar utxosVar
--          let
--            utxosMap = C.unUTxO utxos
--          if any (flip Map.notMember utxosMap) (txIns ++ txInsRef)
--            then pure $ Left $ "UTxO missing:" <> show (filter (flip Map.notMember utxosMap) (txIns ++ txInsRef)) <> ". All UTxOs: " <> show (Map.keys utxosMap)
--            else do
--              let
--                utxosList = U.toList utxos
--                utxosList' =
--                  newUTxOs <>
--                    filter (\(txIn, _) -> txIn `notElem` txIns) utxosList
--                utxos' = U.fromList utxosList'
--              S.writeTVar utxosVar utxos'
--              pure $ Right ()
--      void $ liftEither $ first (\msg -> SimulationOperationFailed msg []) submissionResult
--      pure txBodies
--    OnChainMode t -> do
--      txSubmitterFn connection (CT.DoSubmit t) Nothing
--
-- getNetworkId
--   :: InterpretMonad env m era
--   => m C.NetworkId
-- getNetworkId = do
--   ConnectionWrapper connection <- view connectionL
--   let
--     C.LocalNodeConnectInfo{localNodeNetworkId} = connection
--   pure localNodeNetworkId
