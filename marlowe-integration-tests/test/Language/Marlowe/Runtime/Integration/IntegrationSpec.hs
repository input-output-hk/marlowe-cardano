module Language.Marlowe.Runtime.Integration.IntegrationSpec (spec) where

import qualified Language.Marlowe.Runtime.Integration.ApplyInputs as Apply
import qualified Language.Marlowe.Runtime.Integration.Contract as Contract
import qualified Language.Marlowe.Runtime.Integration.Create as Create
import qualified Language.Marlowe.Runtime.Integration.Intersections as Integrations
import qualified Language.Marlowe.Runtime.Integration.MarloweQuery as MarloweQuery
import qualified Language.Marlowe.Runtime.Integration.OpenRoles as OpenRoles
import qualified Language.Marlowe.Runtime.Integration.Scenario as Basic
import qualified Language.Marlowe.Runtime.Integration.Withdraw as Withdraw
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Marlowe runtime API" do
  Basic.spec
  Integrations.spec
  MarloweQuery.spec
  describe "MarloweTxCommand" do
    Create.spec
    Apply.spec
    Withdraw.spec
  Contract.spec
  OpenRoles.spec
