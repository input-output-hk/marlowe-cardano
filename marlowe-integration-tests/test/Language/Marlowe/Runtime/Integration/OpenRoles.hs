module Language.Marlowe.Runtime.Integration.OpenRoles where

import Language.Marlowe.Runtime.ChainSync.Api (Quantity (..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (OpenRoleScript))
import Language.Marlowe.Runtime.Integration.Basic (basicScenarioWithCreator)
import Language.Marlowe.Runtime.Integration.StandardContract (createStandardContractWithRolesConfig)
import Language.Marlowe.Runtime.Transaction.Api (
  Destination (..),
  RoleTokensConfig (..),
  mkMint,
 )
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Open roles" do
  describe "single token" do
    basicScenarioWithCreator
      . createStandardContractWithRolesConfig (Just "Thread")
      . RoleTokensMint
      . mkMint
      $ pure ("Party A", Nothing, ToScript OpenRoleScript, Quantity 1)
  describe "multiple tokens" do
    basicScenarioWithCreator
      . createStandardContractWithRolesConfig (Just "Thread")
      . RoleTokensMint
      . mkMint
      $ pure ("Party A", Nothing, ToScript OpenRoleScript, Quantity 2)
