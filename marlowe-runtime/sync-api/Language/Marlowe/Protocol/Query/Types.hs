{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Query.Types
  where

import Data.Aeson (ToJSON)
import Data.Binary (Binary)
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol

data MarloweQuery where
  StInit :: MarloweQuery
  StRequest :: err -> result -> MarloweQuery
  StDone :: MarloweQuery

instance Protocol MarloweQuery where
  data Message MarloweQuery st st' where
    MsgGetContractHeaders :: Range ContractId -> Message MarloweQuery
      'StInit
      ('StRequest Void (Page ContractId ContractHeader))

    MsgResolve :: result -> Message MarloweQuery
      ('StRequest err result)
      'StDone

    MsgReject :: err -> Message MarloweQuery
      ('StRequest err result)
      'StDone

  data ClientHasAgency ps where
    TokInit :: ClientHasAgency 'StInit

  data ServerHasAgency ps where
    TokGetContractHeaders :: ServerHasAgency ('StRequest Void (Page ContractId ContractHeader))

  data NobodyHasAgency ps where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data Range a = Range
  { rangeStart :: Maybe a
  , rangeOffset :: Int
  , rangeLimit :: Int
  , rangeDirection :: Order
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary)

data Page a b = Page
  { items :: [b]
  , nextRange :: Maybe (Range a)
  , totalCount :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary)

data Order = Ascending | Descending
  deriving stock (Eq, Show, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, Binary)
