module Language.Marlowe.Runtime.Core.ScriptRegistrySpec (
  spec,
) where

import Cardano.Api (hashScript)
import qualified Cardano.Api as C
import Control.Monad (unless)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (fromCardanoScriptHash)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion, withSomeMarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry
import Language.Marlowe.Scripts (marloweValidator, openRolesValidator, payoutValidator)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

spec :: Spec
spec = traverse_ (withSomeMarloweVersion scriptSetSpec) [minBound .. maxBound]

scriptSetSpec :: MarloweVersion v -> Spec
scriptSetSpec marloweVersion = do
  describe (show marloweVersion) do
    let currentScripts =
          (getCurrentScripts marloweVersion)
            { marloweScriptUTxOs = mempty
            , payoutScriptUTxOs = mempty
            , helperScriptUTxOs = mempty
            }
    let scripts = getScripts marloweVersion
    it "Contains the current scripts in its script set." do
      unless (Set.member (getCurrentScripts marloweVersion) scripts) do
        expectationFailure $
          unwords
            [ "Expected the script set to contain"
            , show currentScripts
            , "but it does not."
            ]
    it "Should specify the correct current scripts" do
      let payoutScript = fromCardanoScriptHash $ hashScript $ C.PlutusScript C.plutusScriptVersion payoutValidator
      let marloweScript = fromCardanoScriptHash $ hashScript $ C.PlutusScript C.plutusScriptVersion marloweValidator
      let helperScripts =
            Map.singleton OpenRoleScript . fromCardanoScriptHash $
              hashScript $
                C.PlutusScript C.plutusScriptVersion openRolesValidator
      let marloweScriptUTxOs = mempty
      let payoutScriptUTxOs = mempty
      let helperScriptUTxOs = mempty
      let currentScripts' = MarloweScripts{..}
      currentScripts `shouldBe` currentScripts'
