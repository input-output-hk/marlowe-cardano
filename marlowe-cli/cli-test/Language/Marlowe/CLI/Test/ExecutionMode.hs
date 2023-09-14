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
