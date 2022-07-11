{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Request where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Marlowe (Contract, PubKeyHash, State, Token)

data Request = Request
  { onlyAssertions :: Bool
  , contract       :: Contract PubKeyHash Token
  , state          :: State PubKeyHash Token
  } deriving (Generic)
instance FromJSON Request
instance ToJSON Request

