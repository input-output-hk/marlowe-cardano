module Test.Data.Map.Row where

import Prelude

import Data.Map.Row (RowMap, get, toRecord)
import Data.Map.Row.Gen (genRowMap)
import Data.Maybe (Maybe)
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
  describe "get@_foo" $ denotationalSpec1 (get _foo) (Record.get _foo)
  describe "get@_bar" $ denotationalSpec1 (get _bar) (Record.get _bar)

denotationalSpec1
  :: forall a
   . Eq a
  => Show a
  => (Operational -> a)
  -> (Denotational -> a)
  -> Spec Unit
denotationalSpec1 op den = do
  it "matches the denotational implementation" do
    quickCheck \(ArbitraryOperational operational) ->
      (op operational) === (den (toRecord operational))
