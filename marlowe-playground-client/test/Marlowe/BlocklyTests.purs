module Marlowe.BlocklyTests where

import Prologue
import Blockly.Dom (explainError, getDom)
import Blockly.Headless as Headless
import Blockly.Internal (getInputWithName, inputList, getBlockById)
import Blockly.Internal as Blockly
import Blockly.Types (BlocklyState)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Data.Bifunctor (lmap, rmap)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Marlowe.Blockly
  ( blockDefinitions
  , blockToContract
  , rootBlockName
  , toBlockly
  )
import Marlowe.Gen (genContract, genTerm, GenerationOptions(..))
import Marlowe.GenWithHoles (GenWithHoles, contractQuickCheck)
import Marlowe.Holes (Contract, Term)
import Marlowe.Parser as Parser
import Test.QuickCheck (Result, (===))
import Test.Spec (Spec, describe, it)
import Text.Extra (stripParens)

all :: Spec Unit
all =
  describe "Marlowe.Blockly" do
    it "codeToBlocklyToCode" $ contractQuickCheck
      (GenerationOptions { withHoles: true, withExtendedConstructs: true })
      codeToBlocklyToCode

-- Here we keep using `show` because the Term range is intentionally incorrect when converting from blockly
-- It uses zero to create a dummy range. By using `show` we can reasonably compare contracts
codeToBlocklyToCode :: GenWithHoles Result
codeToBlocklyToCode = do
  contract <- genTerm "contract" genContract
  let
    positionedContract =
      rmap show
        $ lmap show
        $ Parser.parseContract
        $ stripParens
        $ show contract

    -- Unfortunately quickcheck runs the concrete Gen monad and it would need to be re-written to use MonadGen
    -- https://github.com/purescript/purescript-quickcheck/blob/v5.0.0/src/Test/QuickCheck.purs#L97
    -- I made the executive decision that it's not worth my time to do it in this specific case hence unsafePerformEffect
    -- I have created https://github.com/purescript/purescript-quickcheck/issues/102
    result = rmap show $ unsafePerformEffect $ runContract contract
  pure (result === positionedContract)

runContract :: Term Contract -> Effect (Either String (Term Contract))
runContract contract =
  liftEffect do
    blocklyState <- mkTestState
    buildBlocks blocklyState contract
    runExceptT do
      block <- withExceptT explainError (getDom blocklyState)
      ExceptT $ pure $ blockToContract block

mkTestState :: forall m. MonadEffect m => m BlocklyState
mkTestState = do
  blocklyState <- liftEffect $ Headless.createBlocklyInstance rootBlockName
  liftEffect do
    Blockly.addBlockTypes blocklyState.blockly blockDefinitions
    Headless.initializeWorkspace blocklyState
  pure blocklyState

buildBlocks :: BlocklyState -> Term Contract -> Effect Unit
buildBlocks bs contract = do
  mContract <- getBlockById bs.workspace rootBlockName
  rootBlock <- case mContract of
    Nothing -> Headless.newBlock bs.workspace "BaseContractType"
    Just block -> pure block
  for_ (getInputWithName "BaseContractType" $ inputList rootBlock) \input ->
    toBlockly Headless.newBlock bs.workspace input contract
