{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Language.Marlowe.Runtime.CLI.Command.Load (
  LoadCommand (..),
  loadCommandParser,
  runLoadCommand,
) where

import Control.Monad (forever, unless)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Marlowe.Class (runMarloweTransferClient)
import Data.Binary (decodeFile)
import Data.ByteString.Base16 (encodeBase16)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.Marlowe.Object.Archive (BundleManifest (..), ReadArchiveError (..), unpackArchive)
import Language.Marlowe.Object.Types (Label (..), ObjectBundle (..), SomeObjectType (..))
import Language.Marlowe.Protocol.Transfer.Types (ImportError)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Client (importIncremental)
import Options.Applicative (ParserInfo, help, info, metavar, progDesc, strArgument)
import Pipes (Pipe, Producer, await, each, yield, (>->))
import qualified Pipes.Prelude as P
import System.Exit (die)
import UnliftIO (Exception (displayException), liftIO, pooledMapConcurrently)
import UnliftIO.Directory (doesFileExist)

newtype LoadCommand = LoadCommand
  { archivePath :: FilePath
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
  exists <- liftIO $ doesFileExist archivePath
  liftIO $ unless exists $ die "Bundle archive file does not exist"
  result <- unpackArchive archivePath \manifest ->
    P.head $
      bundles manifest
        >-> (lift . handleError =<< runMarloweTransferClient importIncremental)
        >-> collectMain manifest
  liftIO case result of
    Left err -> die $ formatReadArchiveError err
    Right Nothing -> die "Error: main not linked. This is a bug, please report it with the archive you were trying to load attached."
    Right (Just mainHash) -> putStrLn $ T.unpack $ encodeBase16 $ unDatumHash mainHash

bundles :: BundleManifest -> Producer ObjectBundle CLI ()
bundles BundleManifest{..} =
  each (chunksOf 50 objects)
    >-> (await >>= liftIO . pooledMapConcurrently decodeFile >>= yield . ObjectBundle)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i as = chunk : chunksOf i as'
  where
    (chunk, as') = splitAt i as

handleError :: Maybe ImportError -> CLI ()
handleError = maybe (pure ()) (liftIO . die . ("Failed to import bundle: " <>) . show)

collectMain :: BundleManifest -> Pipe (Map Label DatumHash) DatumHash CLI ()
collectMain BundleManifest{..} = forever $ traverse_ yield . Map.lookup mainIs =<< await

formatReadArchiveError :: ReadArchiveError -> String
formatReadArchiveError = \case
  ArchiveNotFound -> "Archive file not found"
  ArchiveReadFailed e -> "Failed to read archive file: " <> displayException e
  ExtractSecurityError e -> "Failed to extract archive file: " <> displayException e
  ExtractFormatError e -> "Failed to extract archive file: " <> displayException e
  MissingManifest -> "Archive does not contain a manifest.json"
  InvalidManifest e -> "Archive contains an invalid manifest.json: " <> e
  MissingObjectFile path -> "Manifest lists a missing object file path: " <> path
  InvalidObjectFile path byteOffset err ->
    "Bundle contains an invalid object file. Path: " <> path <> "; Byte Offset: " <> show byteOffset <> "; Error:" <> err
  MissingMain -> "Manifest specifies a main object that is not listed as an object"
  WrongMainType (UnsafeSomeObjectType actual) -> "Manifest specifies a main object that is of a non-contract type: " <> show actual
