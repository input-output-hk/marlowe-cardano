{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.CLI.Test.Log (
  module Language.Marlowe.CLI.Test.Log.Label,
  logTxBody,
  logStoreMsgWith,
  logLabeledMsg,
  logStoreLabeledMsg,
  printLabeledMsg,
  throwLabeledError,
  LogEntry,
  Logs,
  HasInterpretEnv (..),
  HasInterpretState (..),
  InterpretMonad,
)
where

import Cardano.Api qualified as C
import Contrib.Cardano.Debug as C.Debug
import Control.Lens (Lens', view, (%=), (^.))
import Control.Lens.Type (Getter)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson.OneLine qualified as A
import Data.Aeson.Types qualified as A
import Data.Text qualified as T
import GHC.IO.Handle.FD (stderr)
import Language.Marlowe.CLI.IO (getProtocolParams, txResourceUsage)
import Language.Marlowe.CLI.Test.CLI.Monad (runCli)
import Language.Marlowe.CLI.Test.InterpreterError (
  InterpreterError (..),
  ieInfo,
  ieMessage,
 )
import Language.Marlowe.CLI.Test.Log.Label (Label (..))
import Language.Marlowe.CLI.Types (QueryExecutionContext)
import System.IO (hPutStrLn)

printLabeledMsg :: (Label l) => l -> String -> String
printLabeledMsg l msg = do
  let l' = label l
  "[" <> l' <> "]" <> " " <> msg

throwLabeledError
  :: (Label l)
  => (MonadError InterpreterError m)
  => l
  -> InterpreterError
  -> m a
throwLabeledError loc err = do
  let info' = ("label", A.toJSON $ label loc) : err ^. ieInfo
      msg' = printLabeledMsg loc $ err ^. ieMessage
      err' = err{_ieInfo = info', _ieMessage = msg'}
  throwError err'

-- | `LogEntry` consists of a "label", a message and a list of an extra info.
type LogEntry = (String, String, [A.Pair])

type Logs = [LogEntry]

class HasInterpretEnv env era | env -> era where
  reportDirL :: Getter env FilePath
  queryCtxL :: Getter env (QueryExecutionContext era)
  eraL :: Lens' env (C.ScriptDataSupportedInEra era)

class HasInterpretState st where
  logStoreL :: Lens' st Logs

type InterpretMonad env st m era =
  ( MonadState st m
  , HasInterpretState st
  , MonadReader env m
  , HasInterpretEnv env era
  , MonadIO m
  , MonadError InterpreterError m
  )

logLabeledMsg
  :: (Label l)
  => (MonadIO m)
  => l
  -> String
  -> m ()
logLabeledMsg l msg = liftIO . hPutStrLn stderr $ printLabeledMsg l msg

logStoreMsgWith
  :: (InterpretMonad env st m era)
  => (Label l)
  => l
  -> String
  -> [A.Pair]
  -> m ()
logStoreMsgWith l msg pairs = do
  let l' = label l
  logStoreL %= ((l', msg, pairs) :)
  case pairs of
    [] -> liftIO . hPutStrLn stderr $ printLabeledMsg l msg
    _ -> do
      let payloadStr = do
            let jsonStr = T.unpack $ A.renderValue $ A.object pairs
            if length jsonStr > 80
              then take 80 jsonStr <> "..."
              else jsonStr
      liftIO . hPutStrLn stderr $ printLabeledMsg l $ msg <> " " <> payloadStr

logStoreLabeledMsg
  :: (InterpretMonad env st m era)
  => (Label l)
  => l
  -> String
  -> m ()
logStoreLabeledMsg l msg = logStoreMsgWith l msg []

logTxBody
  :: (InterpretMonad env st m era)
  => (C.IsCardanoEra era)
  => (Label l)
  => l
  -> String
  -> C.TxBody era
  -> (A.Value -> A.Value)
  -> m ()
logTxBody l msg txBody jsonRewrite = do
  let txId = T.unpack $ C.serialiseToRawBytesHexText $ C.getTxId txBody
      txJson =
        T.unpack $
          A.renderValue $
            jsonRewrite $
              A.object $
                C.Debug.friendlyTxBody C.cardanoEra txBody
  era <- view eraL
  queryCtx <- view queryCtxL
  pp <- runCli era (label l) $ getProtocolParams queryCtx
  view reportDirL >>= \reportDir -> do
    let jsonFile = reportDir <> "/" <> txId <> ".json"
        envelopeFile = reportDir <> "/" <> txId <> ".envelope"
        resourceUsage = txResourceUsage era pp txBody
        info =
          [ ("txId", A.toJSON txId)
          , ("json", A.toJSON jsonFile)
          , ("envelope", A.toJSON envelopeFile)
          , ("resourceUsage", A.toJSON resourceUsage)
          ]
    liftIO $ writeFile jsonFile txJson
    liftIO (C.writeFileTextEnvelope envelopeFile mempty txBody) >>= \case
      Left err -> throwLabeledError l $ TestExecutionFailed (show err) []
      Right () -> pure ()
    logStoreMsgWith l msg info
