{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Object.Archive where

import Codec.Archive.Tar (FormatError)
import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Check (FileNameError)
import Codec.Compression.GZip (compress, decompress)
import Control.Exception (IOException)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import qualified Data.Aeson as A
import Data.Binary (decodeFileOrFail, encodeFile)
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.DList as DList
import Data.Foldable (asum)
import qualified Data.Text as T
import Data.Traversable (for)
import GHC.Generics (Generic)
import Language.Marlowe.Object.Types
import System.FilePath ((</>))
import UnliftIO (
  Handler (..),
  MonadIO (..),
  MonadUnliftIO,
  atomicModifyIORef,
  catches,
  newIORef,
  readIORef,
  withSystemTempDirectory,
 )
import UnliftIO.Directory (doesFileExist, listDirectory)

data BundleManifest = BundleManifest
  { mainIs :: Label
  , objects :: [FilePath]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReadArchiveError
  = ArchiveNotFound
  | ArchiveReadFailed IOException
  | ExtractSecurityError FileNameError
  | ExtractFormatError FormatError
  | MissingManifest
  | InvalidManifest String
  | MissingObjectFile FilePath
  | InvalidObjectFile FilePath ByteOffset String
  | MissingMain
  | WrongMainType SomeObjectType
  deriving (Show)

unpackArchive
  :: (MonadUnliftIO m)
  => FilePath
  -> (BundleManifest -> m a)
  -> m (Either ReadArchiveError a)
unpackArchive archivePath f = withSystemTempDirectory "marlowe.bundle.extract" \extractDir -> runExceptT do
  exists <- doesFileExist archivePath
  unless exists $ throwE ArchiveNotFound
  ExceptT $
    liftIO $
      (Right <$> extractGz archivePath extractDir)
        `catches` [ Handler $ pure . Left . ArchiveReadFailed
                  , Handler $ pure . Left . ExtractSecurityError
                  , Handler $ pure . Left . ExtractFormatError
                  ]
  manifest <- validateManifestAndResolvePaths extractDir
  mainType <- checkObjects manifest
  checkMain mainType
  lift $ f manifest

packArchive
  :: (MonadUnliftIO m)
  => FilePath
  -> Label
  -> ((LabelledObject -> m ()) -> m a)
  -> m a
packArchive archivePath mainIs f = withSystemTempDirectory "marlowe.bundle.create" \baseDir -> do
  objectsVar <- newIORef mempty
  a <- f \obj -> do
    let objectPath = T.unpack $ encodeBase16 $ unLabel $ _label obj
    liftIO $ encodeFile (baseDir </> objectPath) obj
    atomicModifyIORef objectsVar \objects -> (DList.snoc objects objectPath, ())
  (DList.toList -> objects) <- readIORef objectsVar
  liftIO $ A.encodeFile (baseDir </> "manifest.json") BundleManifest{..}
  liftIO $ createGz archivePath baseDir =<< listDirectory baseDir
  pure a

extractGz :: FilePath -> FilePath -> IO ()
extractGz archivePath extractDir =
  Tar.unpack extractDir . Tar.read . decompress =<< LBS.readFile archivePath

createGz :: FilePath -> FilePath -> [FilePath] -> IO ()
createGz archivePath baseDir paths =
  LBS.writeFile archivePath . compress . Tar.write =<< Tar.pack baseDir paths

checkMain :: (Monad m) => Maybe SomeObjectType -> ExceptT ReadArchiveError m ()
checkMain =
  except . \case
    Nothing -> Left MissingMain
    Just (SomeObjectType ContractType) -> Right ()
    Just t -> Left $ WrongMainType t

checkObjects :: (MonadIO m) => BundleManifest -> ExceptT ReadArchiveError m (Maybe SomeObjectType)
checkObjects BundleManifest{..} =
  asum <$> for objects \objectPath -> do
    exists <- doesFileExist objectPath
    unless exists $ throwE $ MissingObjectFile objectPath
    LabelledObject{..} <-
      withExceptT (uncurry (InvalidObjectFile objectPath)) $ ExceptT $ liftIO $ decodeFileOrFail objectPath
    pure
      if _label == mainIs
        then Just $ SomeObjectType _type
        else Nothing

validateManifestAndResolvePaths :: (MonadIO m) => FilePath -> ExceptT ReadArchiveError m BundleManifest
validateManifestAndResolvePaths bundlePath = do
  let manifestFile = bundlePath </> "manifest.json"
  exists <- doesFileExist manifestFile
  unless exists $ throwE MissingManifest
  BundleManifest{..} <- withExceptT InvalidManifest $ ExceptT $ liftIO $ eitherDecodeFileStrict manifestFile
  pure $ BundleManifest{objects = (bundlePath </>) <$> objects, ..}
