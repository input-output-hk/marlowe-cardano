module Language.Marlowe.Runtime.Integration.OpenRoles where

import qualified Data.Map.NonEmpty as NEMap
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (OpenRoleScript))
import Language.Marlowe.Runtime.Integration.Basic (basicScenarioWithCreator)
import Language.Marlowe.Runtime.Integration.StandardContract (createStandardContractWithRolesConfig)
import Language.Marlowe.Runtime.Transaction.Api (
  Destination (..),
  MintRole (..),
  RoleTokensConfig (..),
  mkMint,
 )
import Test.Hspec (Spec, describe)

spec :: Spec
spec = describe "Open roles" do
  basicScenarioWithCreator
    . createStandardContractWithRolesConfig (Just "Thread")
    . RoleTokensMint
    . mkMint
    $ pure ("Party A", MintRole Nothing $ NEMap.singleton (ToScript OpenRoleScript) 1)
