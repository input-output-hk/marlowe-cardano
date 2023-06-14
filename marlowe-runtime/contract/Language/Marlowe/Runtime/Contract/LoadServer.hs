{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.LoadServer where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (unprotect)
import Data.Functor (void)
import Language.Marlowe.Protocol.Load.Server
import Language.Marlowe.Runtime.Contract.Store (ContractStagingArea(..), ContractStore(..))
import Network.Protocol.Connection
import Network.TypedProtocol
import UnliftIO (MonadUnliftIO)
import UnliftIO.Resource (allocateU)

data LoadServerDependencies m = forall n. LoadServerDependencies
  { batchSize :: Nat ('S n)
  , contractStore :: ContractStore m
  }

loadServer :: MonadUnliftIO m => LoadServerDependencies m -> ServerSource MarloweLoadServer m ()
loadServer LoadServerDependencies{..} = ServerSource do
  (releaseKey, ContractStagingArea{..}) <- allocateU
    (lift $ createContractStagingArea contractStore)
    (lift . discard)
  pure $ void $ pullContract batchSize stageContract (void flush) do
    void $ unprotect releaseKey
    void commit
