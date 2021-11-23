module Text.Extra where

import Prologue
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix, stripSuffix, take, trim)
import Data.String as String

stripParens :: String -> String
stripParens s =
  if take 1 s == "(" then
    fromMaybe s
      $ do
          withoutPrefix <- stripPrefix (Pattern "(") $ trim s
          withoutSuffix <- stripSuffix (Pattern ")") withoutPrefix
          pure withoutSuffix
  else
    s

lines :: String -> Array String
lines = String.split (Pattern "\n")

unlines :: Array String -> String
unlines = String.joinWith "\n"
