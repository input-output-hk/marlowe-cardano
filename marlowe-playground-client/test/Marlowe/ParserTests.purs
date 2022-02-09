module Marlowe.ParserTests where

import Prologue

import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), stripPrefix, stripSuffix, trim)
import Marlowe.Gen (GenerationOptions(..), genContract)
import Marlowe.GenWithHoles (GenWithHoles, contractQuickCheck)
import Marlowe.Holes (Contract)
import Marlowe.Parser (parseContract)
import Test.QuickCheck (Result, (===))
import Test.Spec (Spec, describe, it)
import Text.Pretty (genericPretty)

all :: Spec Unit
all =
  describe "Marlowe.Parser" do
    let
      genOpts = GenerationOptions
        { withHoles: false, withExtendedConstructs: true }
    it "Contract Parser" $ contractQuickCheck genOpts contractParser
    it "Pretty Contract Parser" $ contractQuickCheck genOpts
      prettyContractParser

contractParser :: GenWithHoles Result
contractParser = do
  v <- genContract
  let
    contractWithNoParens = fromMaybe (show v)
      (stripPrefix (Pattern "(") (show v) >>= stripSuffix (Pattern ")"))

    result = parseContract contractWithNoParens

    (expected :: Either String Contract) = Right v
  pure (show result === show expected)

prettyContractParser :: GenWithHoles Result
prettyContractParser = do
  v <- genContract
  let
    prettyContract = trim <<< show <<< genericPretty $ v

    contractWithNoParens = fromMaybe prettyContract
      (stripPrefix (Pattern "(") prettyContract >>= stripSuffix (Pattern ")"))

    result = parseContract contractWithNoParens

    (expected :: Either String Contract) = Right v
  pure (show result === show expected)
