module Contrib.Foreign where

import Prelude

import Data.Argonaut (Json)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Foreign (Foreign)

foreign import foreignDecodeJsonImpl :: Foreign -> Nullable Json

foreignDecodeJson :: Foreign -> Maybe Json
foreignDecodeJson = toMaybe <<< foreignDecodeJsonImpl
