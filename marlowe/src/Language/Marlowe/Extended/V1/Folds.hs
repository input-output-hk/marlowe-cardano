{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Extended.V1.Folds
  where

import Control.Monad.State (State, execState, modify)
import Data.Maybe (fromMaybe)
import Language.Marlowe.Extended.V1 (Case(..), Contract(..), Observation(..), Value(..))
import Language.Marlowe.Extended.V1.Traversals
  (Rewrite(Rewrite), Visitor(..), rewriteContractBottomUp, rewriteContractTopDown)

-- To avoid confusion with the order of arguments in the step
-- function let's provide a bit more "order agnostic" wrapper.
data StepArgs a node = StepArgs
  { node :: node
  , accum :: a
  }

data Step a = Step
  { stepCase :: Maybe (StepArgs a Case -> a)
  , stepContract :: Maybe (StepArgs a Contract -> a)
  , stepObservation :: Maybe (StepArgs a Observation -> a)
  , stepValue :: Maybe (StepArgs a Value -> a)
  }

defaultStep :: Step a
defaultStep = Step Nothing Nothing Nothing Nothing

foldingVisitor :: forall a. Step a -> Visitor (Rewrite (State a))
foldingVisitor step = do
  let
    stepCase' :: StepArgs a Case -> a
    stepCase' = fromMaybe accum (stepCase step)
    stepContract' :: StepArgs a Contract -> a
    stepContract' = fromMaybe accum (stepContract step)
    stepObservation' :: StepArgs a Observation -> a
    stepObservation' = fromMaybe accum (stepObservation step)
    stepValue' :: StepArgs a Value -> a
    stepValue' = fromMaybe accum (stepValue step)

  Visitor
    { onCase = Rewrite \c -> do
        modify (stepCase' . StepArgs c)
        pure c
    , onContract = Rewrite \c -> do
        modify (stepContract' . StepArgs c)
        pure c
    , onObservation = Rewrite \c -> do
        modify (stepObservation' . StepArgs c)
        pure c
    , onValue = Rewrite \c -> do
        modify (stepValue' . StepArgs c)
        pure c
    }

-- Top down folding.
foldlContract :: Step a -> a -> Contract -> a
foldlContract step a cntr = do
  let
    visitor = foldingVisitor step
  execState (rewriteContractTopDown visitor cntr) a

-- Bottom up folding.
foldrContract :: Step a -> a -> Contract -> a
foldrContract step a cntr = do
  let
    visitor = foldingVisitor step
  execState (rewriteContractBottomUp visitor cntr) a

data MapStep a = MapStep
  { mapCase :: Case -> a
  , mapContract :: Contract -> a
  , mapObservation :: Observation -> a
  , mapValue :: Value -> a
  }

defaultMapStep :: Monoid a => MapStep a
defaultMapStep = MapStep (const mempty) (const mempty) (const mempty) (const mempty)

foldMapContract :: forall a. Monoid a => MapStep a -> Contract -> a
foldMapContract MapStep{..} = do
  let
    appendAccum :: (node -> a) -> StepArgs a node -> a
    appendAccum f (StepArgs node acc) = f node <> acc

    step = Step
      { stepCase = Just $ appendAccum mapCase
      , stepContract = Just $ appendAccum mapContract
      , stepObservation = Just $ appendAccum mapObservation
      , stepValue = Just $ appendAccum mapValue
      }
  foldlContract step mempty

