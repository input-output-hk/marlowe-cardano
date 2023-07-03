{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Contract.ImportServer (
  ImportServerDependencies (..),
  importServer,
) where

import Cardano.Api (hashScriptData)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Resource (ReleaseKey, unprotect)
import Control.Monad.Trans.State (StateT (..), runStateT)
import Data.Bifunctor (first)
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import Language.Marlowe.Object.Link (LinkedObject (..), SymbolTable, linkObject)
import Language.Marlowe.Object.Types
import Language.Marlowe.Protocol.Transfer.Server
import Language.Marlowe.Protocol.Transfer.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..), toDatum)
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea (..), ContractStore (..))
import Network.Protocol.Connection (ServerSource (..))
import Plutus.V1.Ledger.Api (fromBuiltin)
import qualified Plutus.V2.Ledger.Api as PV2
import UnliftIO (MonadIO, MonadUnliftIO)
import UnliftIO.Resource (allocateU)

newtype ImportServerDependencies m = ImportServerDependencies
  { contractStore :: ContractStore m
  }

importServer :: (MonadUnliftIO m) => ImportServerDependencies m -> ServerSource MarloweTransferServer m ()
importServer ImportServerDependencies{..} = ServerSource do
  (releaseKey, stagingArea) <-
    allocateU
      (lift $ createContractStagingArea contractStore)
      (lift . discard)
  pure $ server releaseKey stagingArea

server :: forall m. (MonadIO m) => ReleaseKey -> ContractStagingArea m -> MarloweTransferServer m ()
server releaseKey stagingArea@ContractStagingArea{..} = MarloweTransferServer $ pure $ idle mempty
  where
    idle :: SymbolTable -> ServerStIdle m ()
    idle symbols =
      ServerStIdle
        { recvMsgDone = do
            void $ unprotect releaseKey
            void commit
        , recvMsgTransfer = \bundle -> do
            mHashes <- runExceptT $ runStateT (traverse (ingestObject stagingArea) $ getObjects bundle) symbols
            case mHashes of
              Left err -> pure $ SendMsgTransferFailed err ()
              Right (hashes, symbols') -> do
                void flush
                pure $ SendMsgTransferred (Map.fromList hashes) $ idle symbols'
        }

ingestObject
  :: (Monad m)
  => ContractStagingArea m
  -> LabelledObject
  -> StateT SymbolTable (ExceptT TransferError m) (Label, DatumHash)
ingestObject stagingArea obj@LabelledObject{..} = do
  linkedObj <-
    StateT $
      mergeExceptT LinkError . linkObject obj \case
        LinkedContract contract -> LinkedContract <$> merkleizeAndStore stagingArea contract
        linkedObj -> pure linkedObj
  pure (label, hashLinked linkedObj)

mergeExceptT :: (Functor m) => (e' -> e) -> ExceptT e m (Either e' a) -> ExceptT e m a
mergeExceptT injectError = ExceptT . fmap (first injectError =<<) . runExceptT

hashLinked :: LinkedObject -> DatumHash
hashLinked =
  fromCardanoDatumHash . hashScriptData . toCardanoScriptData . \case
    LinkedValue value -> toDatum value
    LinkedObservation obs -> toDatum obs
    LinkedContract contract -> toDatum contract
    LinkedParty party -> toDatum party
    LinkedToken token -> toDatum token
    LinkedAction action -> toDatum action

merkleizeAndStore :: (Monad m) => ContractStagingArea m -> Core.Contract -> ExceptT TransferError m Core.Contract
merkleizeAndStore stage = \case
  Core.Close -> pure Core.Close
  Core.Pay account payee token value contract -> Core.Pay account payee token value <$> merkleizeAndStore stage contract
  Core.If obs c1 c2 -> Core.If obs <$> merkleizeAndStore stage c1 <*> merkleizeAndStore stage c2
  Core.When cases timeout contract ->
    Core.When <$> traverse (merkleizeAndStoreCase stage) cases <*> pure timeout <*> merkleizeAndStore stage contract
  Core.Let valueId value contract -> Core.Let valueId value <$> merkleizeAndStore stage contract
  Core.Assert obs contract -> Core.Assert obs <$> merkleizeAndStore stage contract

merkleizeAndStoreCase
  :: (Monad m) => ContractStagingArea m -> Core.Case Core.Contract -> ExceptT TransferError m (Core.Case Core.Contract)
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
