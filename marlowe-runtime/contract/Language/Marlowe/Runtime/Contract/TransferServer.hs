{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.TransferServer (
  TransferServerDependencies (..),
  transferServer,
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS (RWST, evalRWST, get, modify, tell)
import Control.Monad.Trans.Resource (ReleaseKey, ResourceT, unprotect)
import Data.Coerce (coerce)
import qualified Data.DList as DList
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import Language.Marlowe.Object.Link (LinkedObject (..), SymbolTable, linkBundle')
import Language.Marlowe.Object.Types
import Language.Marlowe.Protocol.Transfer.Server
import Language.Marlowe.Protocol.Transfer.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Runtime.Contract.Api (ContractWithAdjacency (..))
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea (..), ContractStore (..))
import Network.Protocol.Connection (ServerSource (..), resourceTServerSource)
import Plutus.V1.Ledger.Api (fromBuiltin)
import qualified Plutus.V2.Ledger.Api as PV2
import UnliftIO (MonadUnliftIO, pooledMapConcurrently)
import UnliftIO.Resource (allocateU)

newtype TransferServerDependencies m = TransferServerDependencies
  { contractStore :: ContractStore m
  }

transferServer
  :: forall m. (MonadUnliftIO m, MonadFail m) => TransferServerDependencies m -> ServerSource MarloweTransferServer m ()
transferServer TransferServerDependencies{..} =
  resourceTServerSource hoistMarloweTransferServer $ pure server
  where
    server :: MarloweTransferServer (ResourceT m) ()
    server = MarloweTransferServer $ pure idle

    idle :: ServerStIdle (ResourceT m) ()
    idle =
      ServerStIdle
        { recvMsgStartImport = do
            (releaseKey, stage) <-
              allocateU
                (lift $ createContractStagingArea contractStore)
                (lift . discard)
            pure $ uploadServer mempty releaseKey stage
        , recvMsgRequestExport = \rootHash -> lift do
            hashes <- getClosureInExportOrder contractStore rootHash
            pure $ maybe (SendMsgContractNotFound idle) (SendMsgStartExport . downloadServer) hashes
        , recvMsgDone = pure ()
        }

    uploadServer :: SymbolTable -> ReleaseKey -> ContractStagingArea m -> ServerStCanUpload (ResourceT m) ()
    uploadServer objects releaseKey stage@ContractStagingArea{..} =
      ServerStCanUpload
        { recvMsgImported = lift do
            void $ unprotect releaseKey
            void commit
            pure idle
        , recvMsgUpload = \bundle -> lift do
            result <- runExceptT $ linkBundle' bundle (merkleizeAndStoreContracts stage) objects
            case result of
              Left err -> pure $ SendMsgUploadFailed err idle
              Right (Left linkError) -> pure $ SendMsgUploadFailed (LinkError linkError) idle
              Right (Right (hashes, objects')) -> do
                void flush
                pure $
                  SendMsgUploaded (Map.fromList $ mapMaybe sequence hashes) $
                    uploadServer objects' releaseKey stage
        }

    downloadServer :: [DatumHash] -> ServerStCanDownload (ResourceT m) ()
    downloadServer hashes =
      ServerStCanDownload
        { recvMsgCancel = pure idle
        , recvMsgDownload = \i -> lift do
            let (batchHashes, hashes') = splitAt (fromIntegral i) hashes
            case batchHashes of
              [] -> pure $ SendMsgExported idle
              _ ->
                flip SendMsgDownloaded (downloadServer hashes') . ObjectBundle
                  <$> pooledMapConcurrently (loadContract contractStore) batchHashes
        }

getClosureInExportOrder :: forall m. (Monad m) => ContractStore m -> DatumHash -> m (Maybe [DatumHash])
getClosureInExportOrder store rootHash = runMaybeT $ DList.toList . snd <$> evalRWST (writeClosureInExportOrder rootHash) () mempty
  where
    writeClosureInExportOrder :: DatumHash -> RWST () (DList.DList DatumHash) (HashSet DatumHash) (MaybeT m) ()
    writeClosureInExportOrder hash = do
      visited <- get
      if HashSet.member hash visited
        then pure ()
        else do
          ContractWithAdjacency{..} <- lift $ MaybeT $ getContract store hash
          traverse_ writeClosureInExportOrder adjacency
          tell $ pure hash
          modify $ HashSet.insert hash

loadContract :: (MonadFail m) => ContractStore m -> DatumHash -> m LabelledObject
loadContract store hash = do
  Just ContractWithAdjacency{..} <- getContract store hash
  pure $ LabelledObject (coerce hash) ContractType $ fromCoreContract contract

merkleizeAndStoreContracts
  :: (Monad m) => ContractStagingArea m -> LinkedObject -> ExceptT ImportError m (LinkedObject, Maybe DatumHash)
merkleizeAndStoreContracts stage = \case
  LinkedContract contract -> do
    merkleizedContract <- merkleizeAndStore stage contract
    hash <- lift $ stageContract stage merkleizedContract
    pure (LinkedContract merkleizedContract, Just hash)
  obj -> pure (obj, Nothing)

merkleizeAndStore :: (Monad m) => ContractStagingArea m -> Core.Contract -> ExceptT ImportError m Core.Contract
merkleizeAndStore stage = \case
  Core.Close -> pure Core.Close
  Core.Pay account payee token value contract -> Core.Pay account payee token value <$> merkleizeAndStore stage contract
  Core.If obs c1 c2 -> Core.If obs <$> merkleizeAndStore stage c1 <*> merkleizeAndStore stage c2
  Core.When cases timeout contract ->
    Core.When <$> traverse (merkleizeAndStoreCase stage) cases <*> pure timeout <*> merkleizeAndStore stage contract
  Core.Let valueId value contract -> Core.Let valueId value <$> merkleizeAndStore stage contract
  Core.Assert obs contract -> Core.Assert obs <$> merkleizeAndStore stage contract

merkleizeAndStoreCase
  :: (Monad m) => ContractStagingArea m -> Core.Case Core.Contract -> ExceptT ImportError m (Core.Case Core.Contract)
merkleizeAndStoreCase stage@ContractStagingArea{..} = \case
  Core.Case action Core.Close -> pure $ Core.Case action Core.Close
  Core.Case action contract -> do
    contract' <- merkleizeAndStore stage contract
    DatumHash hash <- lift $ stageContract contract'
    pure $ Core.MerkleizedCase action $ PV2.toBuiltin hash
  Core.MerkleizedCase action hash -> do
    exists <- lift $ doesContractExist $ DatumHash $ fromBuiltin hash
    if exists
      then pure $ Core.MerkleizedCase action hash
      else throwE $ ContinuationNotInStore $ fromCoreContractHash hash
