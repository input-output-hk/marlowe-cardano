module Language.Marlowe.Runtime.WebSpec
  where

import qualified Language.Marlowe.Runtime.Web.GetContracts as GetContracts
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Marlowe runtime Web API" do
  GetContracts.spec
