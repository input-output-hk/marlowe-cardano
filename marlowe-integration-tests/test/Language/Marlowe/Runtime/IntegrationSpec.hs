module Language.Marlowe.Runtime.IntegrationSpec
  where

import qualified Language.Marlowe.Runtime.Integration.ApplyInputs as Apply
import qualified Language.Marlowe.Runtime.Integration.Basic as Basic
import qualified Language.Marlowe.Runtime.Integration.Create as Create
import qualified Language.Marlowe.Runtime.Integration.Intersections as Integrations
import qualified Language.Marlowe.Runtime.Integration.MarloweQuery as MarloweQuery
import Test.Hspec (Spec, describe)


spec :: Spec
spec = describe "Marlowe runtime API" do
  Basic.spec
  Integrations.spec
  MarloweQuery.spec
  describe "MarloweTxCommand" do
    Create.spec
    Apply.spec
