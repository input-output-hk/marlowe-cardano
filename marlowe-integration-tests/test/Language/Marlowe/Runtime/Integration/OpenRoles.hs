module Language.Marlowe.Runtime.Integration.OpenRoles where

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
  basicScenarioWithCreator
    . createStandardContractWithRolesConfig
    . RoleTokensMint
    . mkMint
    $ pure ("Thread", (ToSelf, Nothing))
      <> pure ("Party A", (ToScript OpenRoleScript, Nothing))
