{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Contract.Store
  where

import Control.Monad.Event.Class
import Data.Set (Set)
import Data.Void (Void)
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Observe.Event (InjectSelector, addField, injectSelector, reference)
import Observe.Event.Backend (setInitialCauseEventBackend)

data ContractStoreSelector f where
  CreateContractStagingArea :: ContractStoreSelector Void
  ContractStagingAreaSelector :: ContractStagingAreaSelector f -> ContractStoreSelector f

newtype ContractStore m = ContractStore
  { createContractStagingArea :: m (ContractStagingArea m)
  }

hoistContractStore
  :: Functor m
  => (forall x. m x -> n x)
  -> ContractStore m
  -> ContractStore n
hoistContractStore f ContractStore{..} = ContractStore
  { createContractStagingArea = f $ hoistContractStagingArea f <$> createContractStagingArea
  }

traceContractStore
  :: (MonadEvent r s m)
  => InjectSelector ContractStoreSelector s
  -> ContractStore m
  -> ContractStore m
traceContractStore inj ContractStore{..} = ContractStore
  { createContractStagingArea = withInjectEvent inj CreateContractStagingArea \ev ->
      hoistContractStagingArea (localBackend $ setInitialCauseEventBackend [reference ev])
        . traceContractStagingArea (composeInjectSelector inj $ injectSelector ContractStagingAreaSelector)
        <$> createContractStagingArea
  }

data ContractStagingAreaSelector f where
  StageContract :: ContractStagingAreaSelector StageContractField
  Flush :: ContractStagingAreaSelector (Set DatumHash)
  Commit :: ContractStagingAreaSelector (Set DatumHash)
  Discard :: ContractStagingAreaSelector Void

data StageContractField
  = StageContractContract Contract
  | StageContractHash DatumHash

data ContractStagingArea m = ContractStagingArea
  { stageContract :: Contract -> m DatumHash
  , flush :: m (Set DatumHash)
  , commit :: m (Set DatumHash)
  , discard :: m ()
  }

hoistContractStagingArea
  :: (forall x. m x -> n x)
  -> ContractStagingArea m
  -> ContractStagingArea n
hoistContractStagingArea f ContractStagingArea{..} = ContractStagingArea
  { stageContract = f . stageContract
  , flush = f flush
  , commit = f commit
  , discard = f discard
  }

traceContractStagingArea
  :: MonadEvent r s m
  => InjectSelector ContractStagingAreaSelector s
  -> ContractStagingArea m
  -> ContractStagingArea m
traceContractStagingArea inj ContractStagingArea{..} = ContractStagingArea
  { stageContract = \contract ->
      withInjectEventFields inj StageContract [StageContractContract contract] \ev -> do
        hash <- stageContract contract
        addField ev $ StageContractHash hash
        pure hash
  , flush = withInjectEvent inj Flush \ev -> do
      flushedHashes <- flush
      addField ev flushedHashes
      pure flushedHashes
  , commit = withInjectEvent inj Commit \ev -> do
      committedHashes <- commit
      addField ev committedHashes
      pure committedHashes
  , discard = withInjectEvent inj Discard $ const discard
  }
