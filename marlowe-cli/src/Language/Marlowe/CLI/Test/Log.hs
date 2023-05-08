{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.CLI.Test.Log
  where

import Contrib.Data.Aeson.Generic (GetConName, constructorName)
import Control.Lens (Lens', (%=))
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.State.Class (MonadState)
import qualified Data.Aeson.Types as A
import GHC.Generics (Generic(Rep))
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError(ieInfo, ieMessage))
import Language.Marlowe.CLI.Types (CliError(CliError))
import System.IO (hPutStrLn)

-- We should use proper tracing or nested namespace tracking
-- but for now we just use a string label.
class Label l where
  label :: l -> String

instance {-# OVERLAPPABLE #-} (Generic l, GetConName (Rep l)) => Label l where
  label = constructorName

instance {-# OVERLAPPING #-} Label String where
  label = id

printLabeledMsg :: (Label l) => l -> String -> String
printLabeledMsg l msg = do
  let
    l' = label l
  "[" <> l' <> "]" <> " " <> msg

throwLabeledError :: Label l => MonadError InterpreterError m => l -> InterpreterError -> m a
throwLabeledError loc err = do
  let
    info' = ("label", A.toJSON $ label loc) : ieInfo err
    msg' = printLabeledMsg loc $ ieMessage err
    err' = err { ieInfo = info', ieMessage = msg' }
  throwError err'

type LogEntry = (String, String, [A.Pair])

type Logs = [LogEntry]

class HasLogStore st where
  logStoreL :: Lens' st Logs

logLabeledMsg :: Label l => MonadIO m => l -> String -> m ()
logLabeledMsg l msg = liftIO . hPutStrLn stderr $ printLabeledMsg l msg

logStoreMsgWith
  :: MonadIO m
  => HasLogStore st
  => MonadState st m
  => Label l
  => l
  -> String
  -> [A.Pair]
  -> m ()
logStoreMsgWith l msg pairs = do
  let
    l' = label l
  logStoreL %= ((l', msg, pairs) :)
  liftIO . hPutStrLn stderr $ printLabeledMsg l msg

logStoreLabeledMsg
  :: MonadIO m
  => HasLogStore st
  => MonadState st m
  => Label l
  => l
  -> String
  -> m ()
logStoreLabeledMsg l msg = logStoreMsgWith l msg []

