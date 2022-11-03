{-# LANGUAGE LiberalTypeSynonyms #-}
module Language.Marlowe.Runtime.Logging.Colog.LogIO
  where

import Colog
import Control.Applicative (Alternative(empty))
import Control.Concurrent (ThreadId, forkFinally)
import qualified Control.Concurrent as C
import Control.Concurrent.Async (Concurrently(Concurrently), race, runConcurrently)
import qualified Control.Concurrent.Async as A
import Control.Exception (Exception, bracket, bracketOnError, catch, finally, throwIO)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import GHC.Base (Alternative((<|>)))
import Language.Marlowe.Runtime.Logging.Colog (unliftLogAction)
import System.Exit (exitFailure)

-- TODO I:
-- We want to abstract away `Message` so we can define proper types for (application specific) logs and pass them as values. At the end we want to
-- interpret them on the log consumer side - turn them into `String`, `JSON` etc.).
-- Some details like `CallStack` and even `Severity` can be a part of default context beside the semantic log value part (I call it a `trace` below):
-- `data Msg trace = Msg CallStack Severity trace`.
--
-- TODO II:
-- We want to abstract away `LoggerT` here to any `ReaderT ctx` + `HasLog ctx` combination.
type LogIO = LoggerT Message IO
type LogIOAction = LogAction LogIO Message

runLogIO :: LogIOAction -> LogIO a -> IO a
runLogIO logAction = usingLoggerT (unliftLogAction logAction)

askLogIOAction :: LogIO LogIOAction
askLogIOAction = LoggerT ask

-- TODO:
-- When we migrate to more semantic representation of loger like `LogIO msg ()`
-- this and other functions should allow us to enhance the logging context like:
-- ```
-- forkLogIO :: Log msg' IO -> (msg' -> msg) -> LogIO msg ThreadId
-- ```
forkLogIO :: LogIO () -> LogIO ThreadId
forkLogIO action = do
  logAction <- askLogIOAction
  liftIO $ C.forkIO do
    runLogIO logAction action

forkFinallyLogIO :: LogIO a -> (Either SomeException a -> LogIO ()) -> LogIO ThreadId
forkFinallyLogIO action andThen = do
  logAction <- askLogIOAction
  liftIO $ forkFinally (runLogIO logAction action) (runLogIO logAction <$> andThen)

concurrentlyLogIO :: LogIO a -> LogIO b -> LogIO (a, b)
concurrentlyLogIO action1 action2 = do
  logAction <- askLogIOAction
  liftIO (A.concurrently (runLogIO logAction action1) (runLogIO logAction action2))

concurrentlyLogIO_ :: LogIO a -> LogIO b -> LogIO ()
concurrentlyLogIO_ action1 action2 = do
  logAction <- askLogIOAction
  liftIO (A.concurrently_ (runLogIO logAction action1) (runLogIO logAction action2))

newtype ConcurrentlyLogIO a = ConcurrentlyLogIO { runConcurrentlyLogIO :: LogIO a }
  deriving newtype (Functor, Applicative)

instance Alternative ConcurrentlyLogIO where
  empty = ConcurrentlyLogIO $ lift (runConcurrently empty)
  ConcurrentlyLogIO as <|> ConcurrentlyLogIO bs =
    ConcurrentlyLogIO $ do
      logAction <- askLogIOAction
      liftIO . runConcurrently $ Concurrently (runLogIO logAction as) <|> Concurrently (runLogIO logAction bs)

catchLogIO :: Exception e => LogIO a -> (e -> LogIO a) -> LogIO a
catchLogIO action handler = do
  logAction <- askLogIOAction
  liftIO $ runLogIO logAction action `catch` (runLogIO logAction <$> handler)

dieLogIO :: forall a. Text -> LogIO a
dieLogIO msg = do
  logError msg
  liftIO exitFailure

withAsyncLogIO :: LogIO a -> (A.Async a -> LogIO b) -> LogIO b
withAsyncLogIO action handle = do
  logAction <- askLogIOAction
  liftIO $ A.withAsync (runLogIO logAction action) (runLogIO logAction <$> handle)

bracketLogIO :: LogIO a -> (a -> LogIO b) -> (a -> LogIO c) -> LogIO c
bracketLogIO acquire release action = do
  logAction <- askLogIOAction
  liftIO $ bracket
    (runLogIO logAction acquire)
    (runLogIO logAction <$> release)
    (runLogIO logAction <$> action)

bracketOnErrorLogIO :: LogIO a -> (a -> LogIO b) -> (a -> LogIO c) -> LogIO c
bracketOnErrorLogIO acquire onError action = do
  logAction <- askLogIOAction
  liftIO $ bracketOnError
    (runLogIO logAction acquire)
    (runLogIO logAction <$> onError)
    (runLogIO logAction <$> action)

raceLogIO :: LogIO a -> LogIO b -> LogIO (Either a b)
raceLogIO a b = do
  logAction <- askLogIOAction
  liftIO $ race (runLogIO logAction a) (runLogIO logAction b)

throwLogIO :: Exception e => Text -> e -> LogIO a
throwLogIO msg e = do
  logError msg
  liftIO $ throwIO e

finallyLogIO :: LogIO a -> LogIO b -> LogIO a
finallyLogIO action finalize = do
  logAction <- askLogIOAction
  liftIO $ runLogIO logAction action `finally` runLogIO logAction finalize

