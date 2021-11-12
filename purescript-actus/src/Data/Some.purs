module Data.Some
  ( Some
  , fromRecord
  , get
  , modify
  , prop
  , set
  , toRecord
  , unset
  ) where

import Prelude

import Data.Array (any)
import Data.Lens (Traversal, _Just, iso)
import Data.Lens.Record as Lens
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (class IsSymbol)
import Foreign (Foreign)
import Prim.Row (class Cons)
import Record as Record
import Type.Proxy (Proxy)

type Some' f = Record (f Maybe)

newtype Some :: ((Type -> Type) -> Row Type) -> Type
newtype Some f = Some (Some' f)

derive instance eqSome :: Eq (Some' f) => Eq (Some f)

instance showSome :: Show (Some' f) => Show (Some f) where
  show (Some r) = "(Some " <> show r <> ")"

fromRecord
  :: forall (f :: (Type -> Type) -> Row Type). Some' f -> Maybe (Some f)
fromRecord r
  | any isJust $ unsafeValues r = Nothing
  | otherwise = Just (Some r)

foreign import unsafeValues
  :: forall (f :: (Type -> Type) -> Row Type)
   . Some' f
  -> Array (Maybe Foreign)

toRecord :: forall (f :: (Type -> Type) -> Row Type). Some f -> Some' f
toRecord (Some r) = r

prop
  :: forall l f1 f2 r a b
   . IsSymbol l
  => Cons l (Maybe a) r (f1 Maybe)
  => Cons l (Maybe b) r (f2 Maybe)
  => Proxy l
  -> Traversal (Some f1) (Some f2) a b
prop p = iso (toRecord) (Some) <<< Lens.prop p <<< _Just

get
  :: forall l r f a
   . IsSymbol l
  => Cons l a r (f Maybe)
  => Proxy l
  -> Some f
  -> a
get p = Record.get p <<< toRecord

set
  :: forall l f1 f2 r a b
   . IsSymbol l
  => Cons l (Maybe a) r (f1 Maybe)
  => Cons l (Maybe b) r (f2 Maybe)
  => Proxy l
  -> b
  -> Some f1
  -> Some f2
set p b = Some <<< Record.set p (Just b) <<< toRecord

unset
  :: forall l f1 f2 r a b
   . IsSymbol l
  => Cons l (Maybe a) r (f1 Maybe)
  => Cons l (Maybe b) r (f2 Maybe)
  => Proxy l
  -> Some f1
  -> Maybe (Some f2)
unset p = fromRecord <<< Record.set p Nothing <<< toRecord

modify
  :: forall l f1 f2 r a b
   . IsSymbol l
  => Cons l (Maybe a) r (f1 Maybe)
  => Cons l (Maybe b) r (f2 Maybe)
  => Proxy l
  -> (a -> b)
  -> Some f1
  -> Some f2
modify p f = Some <<< Record.modify p (map f) <<< toRecord
