module Language.Marlowe.Runtime.Contract.Store.File
  ( ContractStoreOptions(..)
  , createContractStore
  , defaultContractStoreOptions
  ) where

import Cardano.Api (hashScriptData)
import Codec.Compression.GZip (compress)
import Control.Monad (unless)
import Control.Monad.Catch (MonadMask)
import Data.Binary (put)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LBS
import Data.Default (def)
import Data.Foldable (fold, foldl')
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.UUID.V4 (nextRandom)
import GHC.IO (mkUserError)
import Language.Marlowe (Case(..), Contract(..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(DatumHash), toDatum)
import Language.Marlowe.Runtime.Contract.Store
import Plutus.V2.Ledger.Api (fromBuiltin)
import System.FilePath (takeBaseName, (<.>), (</>))
import System.IO.LockFile (withLockFile)
import UnliftIO
import UnliftIO.Directory
  ( XdgDirectory(XdgData)
  , copyFile
  , createDirectory
  , createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , getXdgDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  )

-- | Options to configure a file-based contract store
data ContractStoreOptions = ContractStoreOptions
  { contractStoreDirectory :: FilePath -- ^ The directory to store the contract files
  , contractStoreStagingDirectory :: FilePath -- ^ The directory to create staging areas
  }

-- | Default options. uses $XDG_DATA/marlowe/runtime/marlowe-contract/store for the
-- store directory and /tmp for the staging directory
defaultContractStoreOptions :: MonadIO m => m ContractStoreOptions
defaultContractStoreOptions = ContractStoreOptions
  <$> getXdgDirectory XdgData ("marlowe" </> "runtime" </> "marlowe-contract" </> "store")
  <*> getTemporaryDirectory

-- | Create a contract store that uses the file system.
--
-- N.B. This implementation requires merkleized contract fragments to be added to the staging area
-- in a bottom-up, depth-first manner (i.e. all sub-contracts of any contract c
-- must be staged before c its self). This allows for a significant performance
-- optimization for computing closures that uses dynamic programming.
createContractStore :: forall m. (MonadUnliftIO m, MonadMask m) => ContractStoreOptions -> m (ContractStore m)
createContractStore ContractStoreOptions{..} = do
  -- Create the root directories if they do not exist.
  createDirectoryIfMissing True contractStoreDirectory
  createDirectoryIfMissing True contractStoreStagingDirectory
  -- Used to obtain exclusive write access to the store.
  let lockfile = contractStoreDirectory </> "lockfile"
  pure ContractStore
    { createContractStagingArea = createContractStagingArea lockfile
    }

  where
    createContractStagingArea storeLockfile = do
      -- Use a UUID as the staging area directory name.
      uuid <- liftIO nextRandom
      let directory = "staging-area-" <> show uuid
      -- Create the staging directory
      createDirectory directory
      -- State variable to control when the staging area can be accessed.
      open <- newMVar True
      -- State variable for buffering file writes.
      mBuffer <- newMVar mempty
      let
        -- | Runs the given action only if the store is open.
        whenOpen
          :: Bool -- ^ True to leave the store open afterwards, False to close it and remove the directory.
          -> m a  -- ^ The action to run if the store is open.
          -> m a
        whenOpen leaveOpen m = modifyMVar open \case
          False -> throwIO $ mkUserError "Staging area is no longer open"
          True -> do
            a <- m
            unless leaveOpen $ removePathForcibly directory
            pure (leaveOpen, a)

        -- | Writes the contents of the buffer to disk and empties the buffer.
        flush :: m (Set DatumHash)
        flush = modifyMVar mBuffer \(closures, buffer) -> do
          pooledMapConcurrently_ id $ flushContractRecord =<< HashMap.elems buffer
          pure ((closures, mempty), Set.fromList $ HashMap.keys buffer)

        -- | Returns a list of file-writing actions to save the contents of a
        -- contract record.
        flushContractRecord ContractRecord{..} =
          let
            basePath = directory </> read (show hash)
            writeIndex name hashes = do
              let filePath = basePath <.> name
              -- Do not compress the index files for speed.
              liftIO $ LBS.writeFile filePath $ runPut $ put $ HashSet.toList hashes
          in
            [ do
                let filePath = basePath <.> "contract"
                -- Compress the contract file for size efficiency.
                liftIO $ LBS.writeFile filePath $ compress $ runPut $ put $ toDatum contract
            , writeIndex "adjacency" adjacency
            , writeIndex "closure" closure
            ]

        -- | Move a file from the staging area to the store. Do not use rename
        -- as it is not supported across file systems.
        moveStagingFile file = do
          let oldName = directory </> file
          let newName = contractStoreDirectory </> file
          fileExists <- doesFileExist newName
          if fileExists
            then pure Nothing
            else do
              copyFile oldName newName
              removeFile oldName
              pure $ Just $ takeBaseName file

      pure ContractStagingArea
        { stageContract =
            -- only run when open, leave staging area open.
            whenOpen True . \case
              -- short-circuit on close contracts and return a static hash
              Close -> pure closeHash
              contract -> do
                -- hash the contract
                let hash = fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ toDatum contract
                -- modify the buffer atomically.
                modifyMVar mBuffer \(closures, buffer) ->
                  -- Do nothing if the contract has already been staged.
                  if HashMap.member hash closures
                    then pure ((closures, buffer), hash)
                    else do
                      -- Compute the adjacency and closure information.
                      record <- computeRecord closures hash contract
                      -- Update the closures and buffer.
                      pure ((HashMap.insert hash (closure record) closures, HashMap.insert hash record buffer), hash)
        , flush =
            -- only run when open, leave staging area open.
            whenOpen True do
              -- flush the buffer
              flush
        , commit =
            -- only run when open, close the staging area.
            whenOpen False do
              -- flush the buffer
              _ <- flush
              -- get all files in the staging area
              files <- listDirectory directory
              -- lock the store
              withLockFile def storeLockfile do
                -- concurrently move all files from the staging area to the store
                results <- pooledMapConcurrently moveStagingFile files
                -- Extract the hashes which were moved.
                pure
                  $ Set.map fromString
                  $ Set.fromList
                  $ HashSet.toList
                  $ HashSet.fromList
                  $ catMaybes results
        , discard =
            -- only run when open, close the staging area.
            whenOpen False $ pure ()
        }

-- | Computes the adjacency and closure information for a merkleized contract.
computeRecord :: MonadIO m => HashMap DatumHash (HashSet DatumHash) -> DatumHash -> Contract -> m ContractRecord
computeRecord closures hash contract = do
  let adjacency = computeAdjacency mempty contract
  closure <- computeClosure hash closures adjacency
  pure ContractRecord{..}

-- | Computes the adjacency information for a merkleized contract.
computeAdjacency :: HashSet DatumHash -> Contract -> HashSet DatumHash
computeAdjacency acc = \case
  Close -> acc
  Pay _ _ _ _ c -> computeAdjacency acc c
  If _ c1 c2 -> computeAdjacency (computeAdjacency acc c1) c2
  When cases _ c -> computeAdjacency (foldl' computeCasesAdjacency acc cases) c
  Let _ _ c -> computeAdjacency acc c
  Assert _ c -> computeAdjacency acc c

-- | Computes the adjacency information for a case in a when contract.
computeCasesAdjacency
  :: HashSet DatumHash
  -> Case Contract
  -> HashSet DatumHash
computeCasesAdjacency acc  = \case
  Case _ c -> computeAdjacency acc c
  MerkleizedCase _ hash -> HashSet.insert (DatumHash $ fromBuiltin hash) acc

-- | Expands the adjacency information of a contract into a closure.
computeClosure
  :: MonadIO m
  => DatumHash -- ^ The hash of the contract.
  -> HashMap DatumHash (HashSet DatumHash) -- ^ Lookup for existing closures.
  -> HashSet DatumHash                     -- ^ Set contract hashes to compute the closure for.
                                           -- Must all be members of the closure map argument.
  -> m (HashSet DatumHash)
computeClosure rootHash closures = fmap (HashSet.insert rootHash . fold) . traverse expand . HashSet.toList
  where
    expand hash
      | hash == closeHash = pure $ HashSet.singleton hash
      | otherwise = case HashMap.lookup hash closures of
          Nothing -> throwIO $ mkUserError $ "No closure found for" <> show hash
          Just transitiveClosure -> pure transitiveClosure

-- | Static hash for the close contract.
closeHash :: DatumHash
closeHash = fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ toDatum Close

-- | A contract with its adjacency and closure information.
data ContractRecord = ContractRecord
  { contract :: Contract -- ^ The contract.
  , hash :: DatumHash  -- ^ The hash of the contract (script datum hash)
  , adjacency :: HashSet DatumHash -- ^ The set of continuation hashes explicitly contained in the contract.
  , closure :: HashSet DatumHash -- ^ The set of hashes contained in the contract and all recursive continuations of the contract.
                                 -- includes the hash of the contract its self.
                                 -- Does not contain the hash of the close contract.
  }
