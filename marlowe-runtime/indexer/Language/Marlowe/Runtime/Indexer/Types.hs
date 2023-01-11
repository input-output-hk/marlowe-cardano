module Language.Marlowe.Runtime.Indexer.Types
  where

import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)

data MarloweBlock = MarloweBlock
  { blockHeader :: BlockHeader
  , transactions :: [MarloweTransaction]
  } deriving (Eq, Show, Generic)

data MarloweTransaction
  = CreateTransaction MarloweCreateTransaction
  | ApplyInputsTransaction MarloweApplyInputsTransaction
  | WithdrawTransaction MarloweWithdrawTransaction
  deriving (Eq, Show, Generic)

data MarloweCreateTransaction = MarloweCreateTransaction
  {
  }
  deriving (Eq, Show, Generic)

data MarloweApplyInputsTransaction = MarloweApplyInputsTransaction
  {
  }
  deriving (Eq, Show, Generic)

data MarloweWithdrawTransaction = MarloweWithdrawTransaction
  {
  }
  deriving (Eq, Show, Generic)
