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
import Control.Monad.Trans.Reader (ask)
import Data.Text (Text)
import GHC.Base (Alternative((<|>)))
import Language.Marlowe.Runtime.Logging.Colog (unliftLogAction)
import System.Exit (exitFailure)

-- | Bunch of wrappers around IO and Async for `LoggerT Message IO`.
-- TODO: We should be able to generalize this to an API over newtyped ReaderT.
type LogIO = LoggerT Message IO
type LogIOAction = LogAction LogIO Message

runLogIO :: LogIOAction -> LogIO a -> IO a
runLogIO logAction = usingLoggerT (unliftLogAction logAction)

askLogIOAction :: LogIO LogIOAction
askLogIOAction = LoggerT ask

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
  empty = ConcurrentlyLogIO (LoggerT empty)
  ConcurrentlyLogIO as <|> ConcurrentlyLogIO bs =
    ConcurrentlyLogIO $ do
      logAction <- askLogIOAction
      liftIO . runConcurrently $ Concurrently (runLogIO logAction as) <|> Concurrently (runLogIO logAction bs)

catchLogIO :: Exception e => LogIO a -> (e -> LogIO a) -> LogIO a
catchLogIO action handler = do
  logAction <- askLogIOAction
  liftIO $ runLogIO logAction action `catch` (runLogIO logAction <$> handler)

dieLogIO :: Text -> LogIO ()
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

