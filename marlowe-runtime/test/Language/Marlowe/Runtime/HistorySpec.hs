module Language.Marlowe.Runtime.HistorySpec
  ( spec
  ) where

import Language.Marlowe.Runtime.History.Script (foldHistoryScript)
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = do
  describe "HistoryScript" historyScriptSpec

historyScriptSpec :: Spec
historyScriptSpec = prop "It generates only valid scripts" \script ->
  either Just (const Nothing) (foldHistoryScript script) === Nothing
