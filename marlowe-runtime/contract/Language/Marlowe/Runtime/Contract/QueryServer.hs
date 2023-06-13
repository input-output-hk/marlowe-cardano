{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Contract.QueryServer where

import Language.Marlowe.Runtime.Contract.Api hiding (getContract, merkleizeInputs)
import Language.Marlowe.Runtime.Contract.Store (ContractStore(..))
import Network.Protocol.Connection
import Network.Protocol.Query.Server
import UnliftIO (MonadUnliftIO, concurrently)

newtype QueryServerDependencies m = QueryServerDependencies
  { contractStore :: ContractStore m
  }

queryServer :: MonadUnliftIO m => QueryServerDependencies m -> Socket (QueryServer ContractRequest) m ()
queryServer QueryServerDependencies{..} = Socket $ pure $ respond concurrently \case
  GetContract hash -> getContract contractStore hash
  MerkleizeInputs hash state input -> merkleizeInputs contractStore hash state input
