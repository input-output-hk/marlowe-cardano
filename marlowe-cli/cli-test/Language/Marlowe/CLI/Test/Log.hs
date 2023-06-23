{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.CLI.Test.Log where

import Contrib.Data.Aeson.Generic (GetConName, constructorName)
import Control.Lens (Lens', (%=), (^.))
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State.Class (MonadState)
import Data.Aeson.Types qualified as A
import GHC.Generics (Generic (Rep))
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError (_ieInfo, _ieMessage), ieInfo, ieMessage)
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
  let l' = label l
  "[" <> l' <> "]" <> " " <> msg

throwLabeledError :: (Label l) => (MonadError InterpreterError m) => l -> InterpreterError -> m a
throwLabeledError loc err = do
  let info' = ("label", A.toJSON $ label loc) : err ^. ieInfo
      msg' = printLabeledMsg loc $ err ^. ieMessage
      err' = err{_ieInfo = info', _ieMessage = msg'}
  throwError err'

-- | `LogEntry` consists of a "label", a message and a list of an extra info.
type LogEntry = (String, String, [A.Pair])

type Logs = [LogEntry]

class HasLogStore st where
  logStoreL :: Lens' st Logs

logLabeledMsg :: (Label l) => (MonadIO m) => l -> String -> m ()
logLabeledMsg l msg = liftIO . hPutStrLn stderr $ printLabeledMsg l msg

logStoreMsgWith
  :: (MonadIO m)
  => (HasLogStore st)
  => (MonadState st m)
  => (Label l)
  => l
  -> String
  -> [A.Pair]
  -> m ()
logStoreMsgWith l msg pairs = do
  let l' = label l
  logStoreL %= ((l', msg, pairs) :)
  liftIO . hPutStrLn stderr $ printLabeledMsg l msg

logStoreLabeledMsg
  :: (MonadIO m)
  => (HasLogStore st)
  => (MonadState st m)
  => (Label l)
  => l
  -> String
  -> m ()
logStoreLabeledMsg l msg = logStoreMsgWith l msg []
