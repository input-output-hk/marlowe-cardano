{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.CLI.Command.Load (
  LoadCommand (..),
  loadCommandParser,
  runLoadCommand,
) where

import Codec.Archive.Tar (extract)
import Control.Monad (forever, unless)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Marlowe.Class (runMarloweTransferClient)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Binary (decodeFile, decodeFileOrFail)
import Data.ByteString.Base16 (encodeBase16)
import Data.Foldable (for_, traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Marlowe.Object.Types (Label (..), LabelledObject (..), ObjectBundle (..), ObjectType (..))
import Language.Marlowe.Protocol.Transfer.Types (ImportError)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Client (importIncremental)
import Options.Applicative (ParserInfo, help, info, metavar, progDesc, strArgument)
import Pipes (Pipe, Producer, await, yield, (>->))
import qualified Pipes.Prelude as P
import System.Exit (die)
import System.FilePath ((</>))
import UnliftIO (liftIO, pooledMapConcurrently, withSystemTempDirectory)
import UnliftIO.Directory (doesFileExist)

newtype LoadCommand = LoadCommand
  { bundleArchive :: FilePath
  }

loadCommandParser :: ParserInfo LoadCommand
loadCommandParser = info parser $ progDesc "Load a contract into the runtime"
  where
    parser =
      LoadCommand
        <$> bundleArchiveOption
    bundleArchiveOption =
      strArgument $
        mconcat
          [ metavar "FILE_PATH"
          , help "A Marlowe contract bundle, as a TAR archive."
          ]

runLoadCommand :: LoadCommand -> CLI ()
runLoadCommand LoadCommand{..} = do
  exists <- liftIO $ doesFileExist bundleArchive
  liftIO $ unless exists $ die "Bundle archive file does not exist"
  withSystemTempDirectory "marlowe.bundle.extract." \extractDir -> do
    liftIO $ extract extractDir bundleArchive
    manifest <- validateBundle extractDir
    rootHash <-
      P.head $
        bundles bundleArchive manifest
          >-> (lift . handleError =<< runMarloweTransferClient importIncremental)
          >-> collectResults manifest
    liftIO case rootHash of
      Nothing -> die "Error: main not linked"
      Just hash -> putStrLn $ T.unpack $ encodeBase16 $ unDatumHash hash

bundles :: FilePath -> BundleManifest -> Producer ObjectBundle CLI ()
bundles archivePath BundleManifest{..} = for_ (chunksOf 50 objects) \chunk -> do
  (ObjectBundle -> bundle) <-
    liftIO $ pooledMapConcurrently (decodeFile . (archivePath </>) . T.unpack . encodeBase16 . unLabel) chunk
  yield bundle

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i as = chunk : chunksOf i as'
  where
    (chunk, as') = splitAt i as

handleError :: Maybe ImportError -> CLI ()
handleError = maybe (pure ()) (liftIO . die . ("Failed to import bundle: " <>) . show)

collectResults :: BundleManifest -> Pipe (Map Label DatumHash) DatumHash CLI ()
collectResults BundleManifest{..} = forever $ traverse_ yield . Map.lookup mainIs =<< await

validateBundle :: FilePath -> CLI BundleManifest
validateBundle bundlePath = do
  manifest <- validateManifest bundlePath
  checkObjects bundlePath manifest
  checkMain bundlePath manifest
  pure manifest

validateManifest :: FilePath -> CLI BundleManifest
validateManifest bundlePath = liftIO do
  let manifestFile = bundlePath </> "manifest.json"
  exists <- doesFileExist manifestFile
  unless exists $ die "Bundle does not contain a manifest.json"
  result <- eitherDecodeFileStrict manifestFile
  case result of
    Left err -> die $ "Bundle contain an invalid manifest.json: " <> err
    Right manifest -> pure manifest

checkObjects :: FilePath -> BundleManifest -> CLI ()
checkObjects bundlePath BundleManifest{..} = liftIO $ for_ objects \(Label label) -> do
  let objectFile = bundlePath </> T.unpack (encodeBase16 label)
  exists <- doesFileExist objectFile
  unless exists $ die $ "Bundle does not contain object listed in manifest.json: " <> show (Label label)
  result <- decodeFileOrFail objectFile
  case result of
    Left _ -> die $ "Bundle contain invalid object: " <> show label
    Right LabelledObject{} -> pure ()

checkMain :: FilePath -> BundleManifest -> CLI ()
checkMain bundlePath BundleManifest{..} = liftIO do
  unless (mainIs `elem` objects) $ die "Bundle is missing main object"
  LabelledObject{..} <- decodeFile (bundlePath </> T.unpack (encodeBase16 $ unLabel mainIs))
  case _type of
    ContractType -> pure ()
    _ -> die $ "Main must be a contract, but it is a " <> show _type

data BundleManifest = BundleManifest
  { mainIs :: Label
  , objects :: [Label]
  }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)
