{-# LANGUAGE GADTs #-}

module Language.Marlowe.Object.Archive where

import Codec.Archive.Tar (FormatError, extract)
import Codec.Archive.Tar.Check (FileNameError)
import Control.Exception (IOException)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Binary (decodeFileOrFail)
import Data.Binary.Get (ByteOffset)
import Data.Foldable (asum)
import Data.Traversable (for)
import GHC.Generics (Generic)
import Language.Marlowe.Object.Types
import System.FilePath ((</>))
import UnliftIO (Handler (..), MonadIO (..), MonadUnliftIO, catches, withSystemTempDirectory)
import UnliftIO.Directory (doesFileExist)

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
      (Right <$> extract archivePath extractDir)
        `catches` [ Handler $ pure . Left . ArchiveReadFailed
                  , Handler $ pure . Left . ExtractSecurityError
                  , Handler $ pure . Left . ExtractFormatError
                  ]
  manifest <- validateManifestAndResolvePaths extractDir
  mainType <- checkObjects manifest
  checkMain mainType
  lift $ f manifest

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
