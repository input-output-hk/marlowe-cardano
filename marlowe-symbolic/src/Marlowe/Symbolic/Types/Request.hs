{-# LANGUAGE DeriveGeneric #-}
module Marlowe.Symbolic.Types.Request where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Marlowe (Contract, State, Token)

data Request = Request
  { onlyAssertions :: Bool
  , contract       :: Contract Token
  , state          :: State Token
  } deriving (Generic)
instance FromJSON Request
instance ToJSON Request

