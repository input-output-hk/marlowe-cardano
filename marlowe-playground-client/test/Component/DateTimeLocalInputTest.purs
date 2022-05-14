module Test.Component.DateTimeLocalInputTest where

import Prologue

import Component.DateTimeLocalInput.State (parseInput)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

all :: Spec Unit
all =
  describe "Component.DateTimeLocalInput" do
    parseInputTest

parseInputTest :: Spec Unit
parseInputTest = describe "parseInput" do
  let
    fromEpoch :: Number -> Maybe DateTime
    fromEpoch = map toDateTime <<< instant <<< Milliseconds
  it "Should parse Date + Hour + min " do
    parseInput "2022-03-18T20:13" `shouldEqual` fromEpoch 1647634380000.0
  it "Should parse Date + Hour + min + sec" do
    parseInput "2022-03-18T20:13:25" `shouldEqual` fromEpoch 1647634405000.0
  it "Should parse Date + Hour + min + sec + ms but discard the ms" do
    parseInput "2022-03-18T20:13:25.125Z" `shouldEqual` fromEpoch
      1647634405000.0

