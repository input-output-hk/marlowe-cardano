module Language.Marlowe.Runtime.Core.ScriptRegistrySpec (
  spec,
) where

import Cardano.Api (AsType (..), File (..), hashScript, readFileTextEnvelope)
import Control.Monad (unless)
import Data.Foldable (traverse_)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Api (fromCardanoScriptHash)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion, withSomeMarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry
import Paths_marlowe_cardano (getDataFileName)
import System.FilePath ((</>))
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
      payoutScriptPath <- getDataFileName $ "scripts" </> "marlowe-rolepayout.plutus"
      marloweScriptPath <- getDataFileName $ "scripts" </> "marlowe-semantics.plutus"
      openRoleScriptPath <- getDataFileName $ "scripts" </> "open-role.plutus"
      Right payoutScriptBytes <- readFileTextEnvelope (AsScript AsPlutusScriptV2) $ File payoutScriptPath
      Right marloweScriptBytes <- readFileTextEnvelope (AsScript AsPlutusScriptV2) $ File marloweScriptPath
      Right openRoleScriptBytes <- readFileTextEnvelope (AsScript AsPlutusScriptV2) $ File openRoleScriptPath
      let payoutScript = fromCardanoScriptHash $ hashScript payoutScriptBytes
      let marloweScript = fromCardanoScriptHash $ hashScript marloweScriptBytes
      let helperScripts = Map.singleton OpenRoleScript . fromCardanoScriptHash $ hashScript openRoleScriptBytes
      let marloweScriptUTxOs = mempty
      let payoutScriptUTxOs = mempty
      let helperScriptUTxOs = mempty
      let currentScripts' = MarloweScripts{..}
      currentScripts `shouldBe` currentScripts'
