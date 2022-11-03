{-# LANGUAGE FlexibleContexts #-}
module Language.Marlowe.Runtime.Logging.Colog
  where

import Colog
  ( LogAction(LogAction)
  , LoggerT(runLoggerT)
  , Message
  , Msg(..)
  , Severity(..)
  , cfilter
  , fmtMessage
  , formatWith
  , logTextStderr
  , withLog
  )
import Control.Exception (Exception(displayException))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT(runReaderT), runReaderT)
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)

prefixLogger :: Monad m => Text -> LoggerT Message m a -> LoggerT Message m a
prefixLogger str action = do
  let
    prefixMsg :: Message -> Message
    prefixMsg (Msg sev stack msg) = Msg sev stack $ str <> msg
  withLog (contramap prefixMsg) action

-- | Given the severity we log everything from that level.
-- `Nothing` value indicates silent mode.
mkLogger :: MonadIO m => Maybe Severity -> LogAction m Message
mkLogger Nothing = LogAction (const $ pure ())
mkLogger (Just sev) = do
  let
    messageActionStdErr = formatWith fmtMessage logTextStderr
  cfilter (\(Msg msgSev _ _) -> sev <= msgSev) messageActionStdErr

-- | It seems that `co-log` doesn't provide helpers to run logging
-- directly in the action monad which doesn't provide `Reader` and `HasLog`
-- instances.
-- The belowe set of functions together with `unliftLogAction` can help
-- in such a case.
runLoggerInM :: LogAction (LoggerT msg m) msg -> LoggerT msg m a -> m a
runLoggerInM logAction = flip runReaderT logAction . runLoggerT

logM :: HasCallStack => LogAction m (Msg sev) -> sev -> Text -> m ()
logM (LogAction doLog) msgSeverity msgText =
    withFrozenCallStack (doLog Msg{ msgStack = callStack, .. })

-- | Logs the message with the 'Debug' severity.
logDebugM :: HasCallStack => LogAction m (Msg Severity) -> Text -> m ()
logDebugM logAction = withFrozenCallStack (logM logAction Debug)

-- | Logs the message with the 'Info' severity.
logInfoM :: HasCallStack => LogAction m (Msg Severity) -> Text -> m ()
logInfoM logAction = withFrozenCallStack (logM logAction Info)

-- | Logs the message with the 'Warning' severity.
logWarningM :: HasCallStack => LogAction m (Msg Severity) -> Text -> m ()
logWarningM logAction = withFrozenCallStack (logM logAction Warning)

-- | Logs the message with the 'Error' severity.
logErrorM :: HasCallStack => LogAction m (Msg Severity) -> Text -> m ()
logErrorM logAction = withFrozenCallStack (logM logAction Error)

-- | Logs 'Exception' message with the 'Error' severity.
logExceptionM :: HasCallStack => forall e m. Exception e => LogAction m (Msg Severity) -> e -> m ()
logExceptionM logAction = withFrozenCallStack (logErrorM logAction . T.pack . displayException)

-- | You have `LoggerT` based `LogAction` to log from the underlining
-- monad by unlifting and picking one of the above `log*M` helpers.
unliftLogAction :: LogAction (LoggerT msg m) msg -> LogAction m msg
unliftLogAction logAction@(LogAction doLog) = do
  LogAction (\msg -> runReaderT (runLoggerT (doLog msg)) logAction)
