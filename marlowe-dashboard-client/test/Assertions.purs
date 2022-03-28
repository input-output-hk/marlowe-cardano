module Test.Assertions where

import Prologue

import Ansi.Codes (Color(..))
import Ansi.Output (foreground, withGraphics)
import Control.Monad.Error.Class (class MonadThrow)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll)
import Effect.Aff (Error)
import Test.Data.Argonaut.Extra (bigIntNeq)
import Test.Spec.Assertions (fail)

foreign import jsonDiffString :: Json -> Json -> String

shouldEqualJson
  :: forall m t
   . MonadThrow Error m
  => EncodeJson t
  => t
  -> t
  -> m Unit
shouldEqualJson a b =
  when (jsonA `bigIntNeq` jsonB) do
    fail $
      joinWith "\n  "
        [ withGraphics (foreground Red) "- Actual"
        , withGraphics (foreground Green) "+ Expected\n\n  "
        ] <>
        replaceAll
          (Pattern "\n")
          (Replacement "\n  ")
          (jsonDiffString jsonA jsonB)
  where
  jsonA = encodeJson a
  jsonB = encodeJson b
