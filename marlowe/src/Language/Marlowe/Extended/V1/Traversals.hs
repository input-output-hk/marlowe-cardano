{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Extended.V1.Traversals
  where

import Control.Monad ((>=>))
import Control.Monad.Identity (runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Language.Marlowe.Extended.V1 (Case(..), Contract(..), Module(..), Observation(..), Timeout(TimeParam), Value(..))

newtype Rewrite f node = Rewrite { runRewrite :: node -> f node }

data Visitor f = Visitor
  { onCase :: f Case
  , onContract :: f Contract
  , onObservation :: f Observation
  , onValue :: f Value
  }

defaultVisitor :: Applicative f => Visitor (Rewrite f)
defaultVisitor =
  Visitor rewrite rewrite rewrite rewrite
  where
    rewrite = Rewrite pure

-- | All the below `traverse*` functions perform
-- a non recursive rewrite pass.
traverseModule :: forall f. Functor f => Visitor (Rewrite f) -> Rewrite f Module
traverseModule Visitor { onContract } = Rewrite $ \Module {..} ->
  Module metadata <$> runRewrite onContract contract

traverseContract :: forall f. Applicative f => Visitor (Rewrite f) -> Rewrite f Contract
traverseContract Visitor {..} = do
  let
    rewriteContract = runRewrite onContract
    rewriteObservation = runRewrite onObservation
    rewriteValue = runRewrite onValue
  Rewrite \case
    Close -> rewriteContract Close
    Pay accId payee token value contract -> Pay accId payee token
      <$> rewriteValue value
      <*> rewriteContract contract
    If obs contract1 contract2 -> If
      <$> rewriteObservation obs
      <*> rewriteContract contract1
      <*> rewriteContract contract2
    When cases timeout contract -> When
      <$> for cases (runRewrite onCase)
      <*> pure timeout
      <*> rewriteContract contract
    Let valId value contract -> Let valId
      <$> rewriteValue value
      <*> rewriteContract contract
    Assert obs contract -> Assert
      <$> rewriteObservation obs
      <*> rewriteContract contract

traverseCase :: forall f. Functor f => Visitor (Rewrite f) -> Rewrite f Case
traverseCase Visitor {..} = Rewrite \(Case action contract) ->
  Case action <$> runRewrite onContract contract

traverseObservation :: forall f. Applicative f => Visitor (Rewrite f) -> Rewrite f Observation
traverseObservation Visitor {..} = Rewrite \case
  AndObs obs1 obs2 -> AndObs
    <$> runRewrite onObservation obs1
    <*> runRewrite onObservation obs2
  OrObs obs1 obs2 -> OrObs
    <$> runRewrite onObservation obs1
    <*> runRewrite onObservation obs2
  NotObs obs -> NotObs <$> runRewrite onObservation obs
  ValueGE val1 val2 -> ValueGE
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  ValueGT val1 val2 -> ValueGT
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  ValueLE val1 val2 -> ValueLE
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  ValueLT val1 val2 -> ValueLT
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  ValueEQ val1 val2 -> ValueEQ
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  TrueObs -> pure TrueObs
  FalseObs -> pure FalseObs
  chose@ChoseSomething {} -> pure chose

traverseValue :: forall f. Applicative f => Visitor (Rewrite f) -> Rewrite f Value
traverseValue Visitor {..} = Rewrite \case
  a@(AvailableMoney _ _) -> pure a
  c@(Constant _) -> pure c
  c@(ConstantParam _) -> pure c
  NegValue value -> NegValue <$> runRewrite onValue value
  AddValue val1 val2 -> AddValue
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  SubValue val1 val2 -> SubValue
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  MulValue val1 val2 -> MulValue
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  DivValue val1 val2 -> DivValue
    <$> runRewrite onValue val1
    <*> runRewrite onValue val2
  c@(ChoiceValue _) -> pure c
  t@TimeIntervalStart -> pure t
  t@TimeIntervalEnd -> pure t
  u@(UseValue _) -> pure u
  Cond obs val1 val2 -> Cond
    <$> runRewrite onObservation obs
    <*> runRewrite onValue val1
    <*> runRewrite onValue val2

-- | Given non recursive vistor create a recursive top to bottom one.
topDownVisitor :: Monad f => Visitor (Rewrite f) -> Visitor (Rewrite f)
topDownVisitor Visitor{..} = do
  let
    visitor' = Visitor
      { onContract = Rewrite $ runRewrite onContract >=> runRewrite (traverseContract visitor')
      , onValue = Rewrite $ runRewrite onValue >=> runRewrite (traverseValue visitor')
      , onCase = Rewrite $ runRewrite onCase >=> runRewrite (traverseCase visitor')
      , onObservation = Rewrite $ runRewrite onObservation >=> runRewrite (traverseObservation visitor')
      }
  visitor'

rewriteContractTopDown :: Monad f => Visitor (Rewrite f) -> Contract -> f Contract
rewriteContractTopDown visitor contract = do
  let
    Visitor {..} = topDownVisitor visitor
  runRewrite onContract contract

-- | Flipped version which allows traversing using `for` like syntax.
forContractTopDown :: Monad f => Contract -> Visitor (Rewrite f) -> f Contract
forContractTopDown = flip rewriteContractTopDown

-- | Given non recursive vistor create a recursive bottom to top one.
bottomUpVisitor :: Monad f => Visitor (Rewrite f) -> Visitor (Rewrite f)
bottomUpVisitor Visitor{..} = do
  let
    visitor' = Visitor
      { onContract = Rewrite $ runRewrite onContract >=> runRewrite (traverseContract visitor')
      , onValue = Rewrite $ runRewrite onValue >=> runRewrite (traverseValue visitor')
      , onCase = Rewrite $ runRewrite onCase >=> runRewrite (traverseCase visitor')
      , onObservation = Rewrite $ runRewrite onObservation >=> runRewrite (traverseObservation visitor')
      }
  visitor'

rewriteContractBottomUp :: Monad f => Visitor (Rewrite f) -> Contract -> f Contract
rewriteContractBottomUp visitor contract = do
  let
    Visitor {..} = bottomUpVisitor visitor
  runRewrite onContract contract

-- | Flipped version which allows traversing using `for` like syntax.
forContractBottomUp :: Monad f => Contract -> Visitor (Rewrite f) -> f Contract
forContractBottomUp = flip rewriteContractBottomUp

fillContract :: Map String Value -> Map String Timeout -> Contract -> Contract
fillContract paramEnv timeoutEnv = do
  let
    timeParamName (TimeParam name) = Just name
    timeParamName _ = Nothing
    visitor = defaultVisitor
      { onContract = Rewrite \case
          (When cs (timeParamName >=> flip Map.lookup timeoutEnv -> Just timeout) contract) ->
            pure $ When cs timeout contract
          contract -> pure contract
      , onValue = Rewrite \case
          ConstantParam (flip Map.lookup paramEnv -> Just value) -> pure value
          contract -> pure contract
      }
  runIdentity . rewriteContractBottomUp visitor


