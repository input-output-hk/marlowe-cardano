{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Extended.V1.Traversals
  where

import Control.Monad ((>=>))
import Control.Monad.Identity (Identity, runIdentity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Traversable (for)
import Language.Marlowe.Extended.V1
  (Action(Deposit), Case(..), Contract(..), Module(..), Observation(..), Timeout(TimeParam), Value(..))

data Visitor f = Visitor
  { onCase :: Case -> f Case
  , onContract :: Contract -> f Contract
  , onObservation :: Observation -> f Observation
  , onValue :: Value -> f Value
  }

defaultVisitor :: Applicative f => Visitor f
defaultVisitor = Visitor pure pure pure pure

-- | All the below `traverse*` functions perform
-- a non recursive rewrite pass over the *attributes*
-- of a given constructor.
traverseModule :: forall f. Functor f => Visitor f -> Module -> f Module
traverseModule Visitor { onContract } Module {..} =
  Module metadata <$> onContract contract

traverseContract :: forall f. Applicative f => Visitor f -> Contract -> f Contract
traverseContract Visitor {..} = do
  \case
    Close -> pure Close
    Pay accId payee token value contract -> Pay accId payee token
      <$> onValue value
      <*> onContract contract
    If obs contract1 contract2 -> If
      <$> onObservation obs
      <*> onContract contract1
      <*> onContract contract2
    When cases timeout contract -> When
      <$> for cases onCase
      <*> pure timeout
      <*> onContract contract
    Let valId value contract -> Let valId
      <$> onValue value
      <*> onContract contract
    Assert obs contract -> Assert
      <$> onObservation obs
      <*> onContract contract

traverseCase :: forall f. Applicative f => Visitor f -> Case -> f Case
traverseCase
  Visitor { onContract, onValue }
  (Case (Deposit accountId party token value) contract) =
  let
    deposit = Deposit accountId party token <$> onValue value
  in
    Case <$> deposit <*> onContract contract
traverseCase Visitor {..} (Case action contract) =
  Case action <$> onContract contract

traverseObservation :: forall f. Applicative f => Visitor f -> Observation -> f Observation
traverseObservation Visitor {..} = \case
  AndObs obs1 obs2 -> AndObs
    <$> onObservation obs1
    <*> onObservation obs2
  OrObs obs1 obs2 -> OrObs
    <$> onObservation obs1
    <*> onObservation obs2
  NotObs obs -> NotObs <$> onObservation obs
  ValueGE val1 val2 -> ValueGE
    <$> onValue val1
    <*> onValue val2
  ValueGT val1 val2 -> ValueGT
    <$> onValue val1
    <*> onValue val2
  ValueLE val1 val2 -> ValueLE
    <$> onValue val1
    <*> onValue val2
  ValueLT val1 val2 -> ValueLT
    <$> onValue val1
    <*> onValue val2
  ValueEQ val1 val2 -> ValueEQ
    <$> onValue val1
    <*> onValue val2
  TrueObs -> pure TrueObs
  FalseObs -> pure FalseObs
  chose@ChoseSomething {} -> pure chose

traverseValue :: forall f. Applicative f => Visitor f -> Value -> f Value
traverseValue Visitor {..} = \case
  a@(AvailableMoney _ _) -> pure a
  c@(Constant _) -> pure c
  c@(ConstantParam _) -> pure c
  NegValue value -> NegValue <$> onValue value
  AddValue val1 val2 -> AddValue
    <$> onValue val1
    <*> onValue val2
  SubValue val1 val2 -> SubValue
    <$> onValue val1
    <*> onValue val2
  MulValue val1 val2 -> MulValue
    <$> onValue val1
    <*> onValue val2
  DivValue val1 val2 -> DivValue
    <$> onValue val1
    <*> onValue val2
  c@(ChoiceValue _) -> pure c
  t@TimeIntervalStart -> pure t
  t@TimeIntervalEnd -> pure t
  u@(UseValue _) -> pure u
  Cond obs val1 val2 -> Cond
    <$> onObservation obs
    <*> onValue val1
    <*> onValue val2

-- | Given non recursive vistor create a recursive top to bottom one.
topDownVisitor :: Monad f => Visitor f -> Visitor f
topDownVisitor Visitor {..} = do
  let
    visitor = Visitor
      { onContract = onContract >=> traverseContract visitor
      , onValue = onValue >=> traverseValue visitor
      , onCase = onCase >=> traverseCase visitor
      , onObservation = onObservation >=> traverseObservation visitor
      }
  visitor

rewriteContractTopDown :: Monad f => Visitor f -> Contract -> f Contract
rewriteContractTopDown visitor contract = do
  let
    Visitor {..} = topDownVisitor visitor
  onContract contract

-- | Flipped version which allows traversing using `for` like infix syntax.
forContractTopDown :: Monad f => Contract -> Visitor f -> f Contract
forContractTopDown = flip rewriteContractTopDown

-- | Given non recursive vistor create a recursive bottom to top one.
bottomUpVisitor :: Monad f => Visitor f -> Visitor f
bottomUpVisitor Visitor{..} = do
  let
    visitor = Visitor
      { onContract = traverseContract visitor >=> onContract
      , onValue = traverseValue visitor >=> onValue
      , onCase = traverseCase visitor >=> onCase
      , onObservation = traverseObservation visitor >=> onObservation
      }
  visitor

rewriteContractBottomUp :: Monad f => Visitor f -> Contract -> f Contract
rewriteContractBottomUp visitor contract = do
  let
    Visitor {..} = bottomUpVisitor visitor
  onContract contract

-- | Flipped version which allows traversing using `for` like or infix syntax.
forContractBottomUp :: Monad f => Contract -> Visitor f -> f Contract
forContractBottomUp = flip rewriteContractBottomUp

substitute :: Map String Value -> Map String Timeout -> Contract -> Contract
substitute paramEnv timeoutEnv = do
  let
    timeParamName (TimeParam name) = Just name
    timeParamName _ = Nothing

    visitor :: Visitor Identity
    visitor = defaultVisitor
      { onContract = \case
          (When cs (timeParamName >=> flip Map.lookup timeoutEnv -> Just timeout) contract) ->
            pure $ When cs timeout contract
          contract -> pure contract
      , onValue = \case
          ConstantParam (flip Map.lookup paramEnv -> Just value) -> pure value
          contract -> pure contract
      }
  runIdentity . rewriteContractBottomUp visitor

