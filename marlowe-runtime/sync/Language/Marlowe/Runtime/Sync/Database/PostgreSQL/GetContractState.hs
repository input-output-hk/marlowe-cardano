{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState
  where

import Control.Foldl (Fold)
import qualified Control.Foldl as Fold
import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16, Int64)
import Data.Maybe (fromJust)
import Hasql.TH (foldStatement, singletonStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , Credential(..)
  , PolicyId(..)
  , ScriptHash(..)
  , TxId(..)
  , TxOutRef(..)
  , paymentCredential
  )
import Language.Marlowe.Runtime.Core.Api
  (ContractId(..), MarloweVersion(MarloweV1), SomeMarloweVersion(SomeMarloweVersion))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Prelude hiding (init)

getContractState :: ContractId -> T.Transaction (Maybe SomeContractState)
getContractState _contractId = error "not implemented"
