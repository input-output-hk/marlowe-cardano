module Test.Data.Argonaut.Extra where

import Data.Argonaut (Json)

-- This function is a modification of Argonaut.Core.stringifyWithIndent in which
-- the keys are ordered when we stringify.
foreign import stableStringifyWithIndent :: Int -> Json -> String
