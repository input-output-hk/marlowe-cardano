{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Response where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionWarning)
import Language.Marlowe.Core.V1.Semantics.Token (Token)

data Result = Valid
            | CounterExample
                { initialSlot        :: Integer
                , transactionList    :: [TransactionInput Token]
                , transactionWarning :: [TransactionWarning Token]
                }
            | Error String
  deriving (Generic)

instance FromJSON Result
instance ToJSON Result

data Response = Response { result     :: Result
                         , durationMs :: Integer
                         }
  deriving (Generic)

instance FromJSON Response
instance ToJSON Response


