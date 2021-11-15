module Test.Data.Map.Row where

import Prelude

import Data.Map.Row (RowMap, get, mapRecord, set, toRecord)
import Data.Map.Row.Gen (genRowMap)
import Data.Maybe (Maybe(..))
import Record as Record
import Test.QuickCheck (class Arbitrary, arbitrary, (===))
import Test.Spec (Spec, describe, it)
import Test.Spec.QuickCheck (quickCheck)
import Type.Proxy (Proxy(..))

type Operational = RowMap
  ( foo :: Int
  , bar :: String
  )

type Denotational =
  { foo :: Maybe Int
  , bar :: Maybe String
  }

newtype ArbitraryOperational = ArbitraryOperational Operational

instance arbitraryArbitraryOperational :: Arbitrary ArbitraryOperational where
  arbitrary = ArbitraryOperational <$> genRowMap arbitrary

_foo :: Proxy "foo"
_foo = Proxy

_bar :: Proxy "bar"
_bar = Proxy

spec :: Spec Unit
spec = do
  describe "get@_foo" $ elimSpec (get _foo) (Record.get _foo)
  describe "get@_bar" $ elimSpec (get _bar) (Record.get _bar)
  describe "set@_foo" $ termSpec2 (set _foo) (Record.set _foo)
  describe "set@_bar" $ termSpec2 (set _bar) (Record.set _bar)

elimSpec
  :: forall a
   . Eq a
  => Show a
  => (Operational -> a)
  -> (Denotational -> a)
  -> Spec Unit
elimSpec op den = do
  it "matches the denotational implementation" do
    quickCheck \(ArbitraryOperational operational) ->
      (op operational) === (den (toRecord operational))

termSpec
  :: (Operational -> Operational)
  -> (Denotational -> Denotational)
  -> Spec Unit
termSpec op den = do
  it "matches the denotational implementation" do
    quickCheck \(ArbitraryOperational operational) ->
      op operational === mapRecord den operational

termSpec2
  :: forall a
   . Arbitrary a
  => (a -> Operational -> Operational)
  -> (Maybe a -> Denotational -> Denotational)
  -> Spec Unit
termSpec2 op den = do
  it "matches the denotational implementation" do
    quickCheck \(ArbitraryOperational operational) a ->
      op a operational === mapRecord (den (Just a)) operational
