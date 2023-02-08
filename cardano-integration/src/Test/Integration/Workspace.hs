module Test.Integration.Workspace
  ( Workspace(..)
  , copyToWorkspace
  , createWorkspace
  , createWorkspaceDir
  , moveToWorkspace
  , openWorkspaceFile
  , resolveWorkspacePath
  , rewriteJSONFile
  , rewriteYAMLFile
  , writeWorkspaceFile
  , writeWorkspaceFileJSON
  , writeWorkspaceFileYAML
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (catch)
import Control.Exception.Base (SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Resource (allocate_)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encode, encodeFile)
import qualified Data.ByteString.Lazy as LBS
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import qualified Data.Yaml as YAML
import GHC.Natural (Natural)
import System.Directory (copyFile, createDirectoryIfMissing, removePathForcibly, renameFile)
import System.FilePath (takeDirectory, (</>))
import System.IO (Handle, IOMode, hClose, openFile)
import UnliftIO.Resource (MonadResource, ReleaseKey, allocate)

data Workspace = Workspace
  { workspaceId :: UUID
  , workspaceDir :: FilePath
  , workspaceName :: String
  , releaseKey :: ReleaseKey
  }

createWorkspace :: MonadResource m => FilePath -> m Workspace
createWorkspace workspaceName = do
  workspaceId <- liftIO nextRandom
  let workspaceDir = "/tmp/workspaces" </> (workspaceName <> "-" <> show workspaceId)
  releaseKey <- allocate_ (createDirectoryIfMissing True workspaceDir) do
    removePathForciblyWithRetries 10 workspaceDir
  pure Workspace{..}

removePathForciblyWithRetries :: Natural -> FilePath -> IO ()
removePathForciblyWithRetries 0 path = removePathForcibly path
removePathForciblyWithRetries n path = removePathForcibly path `catch` \(_ :: SomeException) -> do
  threadDelay 10_000
  removePathForciblyWithRetries (n - 1) path

resolveWorkspacePath :: Workspace -> FilePath -> FilePath
resolveWorkspacePath Workspace{..} = (workspaceDir </>)

openWorkspaceFile :: MonadResource m => Workspace -> FilePath -> IOMode -> m Handle
openWorkspaceFile workspace file mode = do
  let path = resolveWorkspacePath workspace file
  let dir = takeDirectory path
  liftIO $ createDirectoryIfMissing True dir
  snd <$> allocate (openFile path mode) hClose

createWorkspaceDir :: MonadIO m => Workspace -> FilePath -> m FilePath
createWorkspaceDir workspace dirName = do
  let dir = resolveWorkspacePath workspace dirName
  liftIO $ createDirectoryIfMissing True dir
  pure dir

writeWorkspaceFile :: MonadIO m => Workspace -> FilePath -> LBS.ByteString -> m FilePath
writeWorkspaceFile workspace file contents = liftIO do
  let path = resolveWorkspacePath workspace file
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  LBS.writeFile path contents
  pure path

copyToWorkspace :: MonadIO m => Workspace -> FilePath -> FilePath -> m FilePath
copyToWorkspace workspace source target = liftIO do
  let path = resolveWorkspacePath workspace target
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  copyFile source path
  pure path

moveToWorkspace :: MonadIO m => Workspace -> FilePath -> FilePath -> m FilePath
moveToWorkspace workspace source target = liftIO do
  let path = resolveWorkspacePath workspace target
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  renameFile source path
  pure path

writeWorkspaceFileJSON :: (ToJSON a, MonadIO m) => Workspace -> FilePath -> a -> m FilePath
writeWorkspaceFileJSON workspace file = writeWorkspaceFile workspace file . encode

rewriteJSONFile :: (FromJSON a, ToJSON b, MonadIO m) => FilePath -> (a -> b) -> m ()
rewriteJSONFile file rewrite = liftIO do
  json <- either error id <$> eitherDecodeFileStrict file
  let json' = rewrite json
  encodeFile file json'

rewriteYAMLFile :: (FromJSON a, ToJSON b, MonadIO m) => FilePath -> (a -> b) -> m ()
rewriteYAMLFile file rewrite = liftIO do
  json <- YAML.decodeFileThrow file
  let json' = rewrite json
  YAML.encodeFile file json'

writeWorkspaceFileYAML :: (ToJSON a, MonadIO m) => Workspace -> FilePath -> a -> m FilePath
writeWorkspaceFileYAML workspace file = writeWorkspaceFile workspace file . LBS.fromStrict . YAML.encode
