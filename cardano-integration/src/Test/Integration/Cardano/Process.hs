module Test.Integration.Cardano.Process
  ( exec
  , exec'
  , execCli
  , execCli'
  , execCli_
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor (void)
import qualified GHC.Stack as GHC
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

execCli_ :: MonadIO m => [String] -> m ()
execCli_ = void . execCli

execCli :: MonadIO m => [String] -> m String
execCli = exec "cardano-cli"

execCli' :: MonadIO m => [String] -> m (ExitCode, String, String)
execCli' = exec' "cardano-cli"

exec :: MonadIO m => FilePath -> [String] -> m String
exec path args = do
  (exitCode, stdout, stderr) <- exec' path args
  case exitCode of
    ExitSuccess -> pure stdout
    ExitFailure code -> error
      $ "command "
      <> path
      <> " exited with code "
      <> show code
      <> ", stderr:\n"
      <> stderr

exec' :: MonadIO m => FilePath -> [String] -> m (ExitCode, String, String)
exec' path args = liftIO $ GHC.withFrozenCallStack $ readProcessWithExitCode path args mempty
