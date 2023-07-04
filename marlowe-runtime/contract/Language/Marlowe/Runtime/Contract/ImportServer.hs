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
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Language.Marlowe.Core.V1.Semantics.Types as Core
import Language.Marlowe.Object.Link (LinkedObject (..), SymbolTable, linkBundle)
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
  (releaseKey, stage) <-
    allocateU
      (lift $ createContractStagingArea contractStore)
      (lift . discard)
  pure $ server releaseKey stage

server :: forall m. (MonadIO m) => ReleaseKey -> ContractStagingArea m -> MarloweTransferServer m ()
server releaseKey stage@ContractStagingArea{..} = MarloweTransferServer $ pure $ idle mempty
  where
    idle :: SymbolTable -> ServerStIdle m ()
    idle objects =
      ServerStIdle
        { recvMsgDone = do
            void $ unprotect releaseKey
            void commit
        , recvMsgTransfer = \bundle -> do
            result <- runExceptT $ linkBundle bundle (merkleizeAndStoreContracts stage) objects
            case result of
              Left err -> pure $ SendMsgTransferFailed err ()
              Right (Left linkError) -> pure $ SendMsgTransferFailed (LinkError linkError) ()
              Right (Right (hashes, symbols')) -> do
                void flush
                pure $ SendMsgTransferred (hashLinked <$> Map.fromList hashes) $ idle symbols'
        }

merkleizeAndStoreContracts :: (Monad m) => ContractStagingArea m -> LinkedObject -> ExceptT TransferError m LinkedObject
merkleizeAndStoreContracts stage = \case
  LinkedContract contract -> LinkedContract <$> merkleizeAndStore stage contract
  obj -> pure obj

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

hashLinked :: LinkedObject -> DatumHash
hashLinked =
  fromCardanoDatumHash . hashScriptData . toCardanoScriptData . \case
    LinkedValue value -> toDatum value
    LinkedObservation obs -> toDatum obs
    LinkedContract contract -> toDatum contract
    LinkedParty party -> toDatum party
    LinkedToken token -> toDatum token
    LinkedAction action -> toDatum action
