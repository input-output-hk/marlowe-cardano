module Language.Marlowe.Runtime.Logging
  where

import Colog (LogAction(LogAction), Message, Msg(Msg), Severity, cfilter, fmtMessage, formatWith, logTextStderr)
import Control.Monad.IO.Class (MonadIO)

-- | Given the severity we log everything from that level.
-- `Nothing` value indicates silent mode.
mkLogger :: MonadIO m => Maybe Severity -> LogAction m Message
mkLogger Nothing = LogAction (const $ pure ())
mkLogger (Just sev) = do
  let
    messageActionStdErr = formatWith fmtMessage logTextStderr
  cfilter (\(Msg msgSev _ _) -> sev <= msgSev) messageActionStdErr

