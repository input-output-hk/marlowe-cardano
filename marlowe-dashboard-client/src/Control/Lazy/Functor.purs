module Control.Lazy.Functor where

import Prelude

import Control.Comonad.Cofree (Cofree)
import Control.Lazy (defer)
import Control.Monad.RWS (RWST)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Const (Const(..))
import Data.Functor.App (App(..))
import Data.Lazy (Lazy)
import Data.List.Lazy (List)
import Data.Newtype (unwrap)
import Effect.Aff (Aff)
import Test.QuickCheck.Gen (Gen)
import Text.Parsing.Parser (ParserT)

class Functor f <= Lazy1 f where
  defer1 :: forall a. (Unit -> f a) -> f a

instance Functor m => Lazy1 (RWST r w s m) where
  defer1 = defer

instance Lazy1 (Function a) where
  defer1 = defer

instance Lazy1 (Const Unit) where
  defer1 _ = Const unit

instance Lazy1 List where
  defer1 = defer

instance Lazy1 Aff where
  defer1 = defer

instance Functor f => Lazy1 (Cofree f) where
  defer1 = defer

instance Lazy1 Lazy where
  defer1 = defer

instance Lazy1 f => Lazy1 (App f) where
  defer1 = App <<< defer1 <<< map unwrap

instance Functor m => Lazy1 (ParserT s m) where
  defer1 = defer

instance Lazy1 Gen where
  defer1 = defer

mfix :: forall m a. MonadRec m => Lazy1 m => (a -> m a) -> m a
mfix f = go
  where
  go = defer1 \_ -> f =<< go
