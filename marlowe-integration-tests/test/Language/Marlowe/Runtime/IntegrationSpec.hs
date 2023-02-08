module Language.Marlowe.Runtime.IntegrationSpec
  where

import qualified Language.Marlowe.Runtime.Integration.Basic as Basic
import qualified Language.Marlowe.Runtime.Integration.Intersections as Integrations
import Test.Hspec (Spec, describe)


spec :: Spec
spec = describe "Marlowe runtime API" do
  Basic.spec
  Integrations.spec
