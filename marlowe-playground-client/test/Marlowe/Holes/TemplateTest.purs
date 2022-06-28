module Marlowe.Holes.TemplateTest where

import Prologue

import Data.Map as Map
import Data.Maybe (isNothing)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1 (toCore)
import Language.Marlowe.Extended.V1 as EM
import Marlowe.Gen (GenerationOptions(..), genBigInt, genContract, genInstant)
import Marlowe.GenWithHoles (GenWithHoles, contractQuickCheck)
import Marlowe.Holes (fromTerm)
import Marlowe.Template
  ( Placeholders(..)
  , TemplateContent(..)
  , fillTemplate
  , getPlaceholderIds
  )
import Test.QuickCheck (Result(..), (===))
import Test.Spec (Spec, describe, it)

all :: Spec Unit
all =
  describe "Marlowe.Holes.Template" do
    let
      genOpts = GenerationOptions
        { withHoles: false, withExtendedConstructs: true }
    it "Term and Extended contract has the same getPlaceholderIds" $
      contractQuickCheck genOpts sameGetPlaceholderIds
    it "Term and Extended contract has the same fillTemplate" $
      contractQuickCheck genOpts sameFillTemplate

sameGetPlaceholderIds :: GenWithHoles Result
sameGetPlaceholderIds = do
  termContract <- genContract
  let
    (emContract :: Maybe EM.Contract) = fromTerm termContract

    (termPlaceholders :: Placeholders) = getPlaceholderIds termContract

    (emPlaceHolders :: Maybe Placeholders) = getPlaceholderIds <$> emContract
  pure (Just termPlaceholders === emPlaceHolders)

-- This property test checks that for all contracts, if we fill the template values
-- on a term and on an extended contract, the result is the same
sameFillTemplate :: GenWithHoles Result
sameFillTemplate = do
  -- We start by generating a random contract and getting the placeholder of the values
  -- to fill
  termContract <- genContract
  let
    (emContract :: Maybe EM.Contract) = fromTerm termContract

    Placeholders { timeoutPlaceholderIds, valuePlaceholderIds } =
      getPlaceholderIds
        termContract

    timeoutPlaceholderArray :: Array String
    timeoutPlaceholderArray = Set.toUnfoldable timeoutPlaceholderIds

    valuePlaceholderArray :: Array String
    valuePlaceholderArray = Set.toUnfoldable valuePlaceholderIds
  -- We generate random values for the template variables
  timeContent <-
    Map.fromFoldable
      <$>
        ( for timeoutPlaceholderArray \timeoutId -> do
            timeoutValue <- genInstant
            pure (timeoutId /\ timeoutValue)
        )
  valueContent <-
    Map.fromFoldable
      <$>
        ( for valuePlaceholderArray \valueId -> do
            timeoutValue <- genBigInt
            pure (valueId /\ timeoutValue)
        )
  -- And check that once we fill the contract, the semantic version for both of them
  -- are the same.
  -- In other tests we do the comparison of the string representation, but there appears to be
  -- some problem with parentesis, so I decided to convert both of them to the semantic version
  -- and test that those are equal.
  let
    templateContent = TemplateContent { timeContent, valueContent }

    filledTerm = fillTemplate templateContent termContract

    filledExtended = fillTemplate templateContent <$> emContract

    semantic1 :: Maybe S.Contract
    semantic1 = toCore =<< filledExtended

    semantic2 :: Maybe S.Contract
    semantic2 = fromTerm filledTerm
  if (isNothing semantic1) then
    -- If we reach this state then the filledTerm has holes or the filledExtended
    -- has template variables that weren't filled
    pure $ Failed "could not create semantic contract"
  else
    pure $ semantic1 === semantic2
