{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A monad for conveniently defining Marlowe object bundles. It allows one to define a Marlowe contract
--   with named abstractions for commonly-used sub-contracts, and under the hood it converts it to an
--   object definition and replaces usages with a reference.
module Language.Marlowe.Object.Bundler where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Cont (ContT)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..), IdentityT)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Writer (WriterT)
import Control.Monad.Writer (MonadWriter (..), censor)
import Control.Monad.Zip (MonadZip (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Functor (($>))
import GHC.Show (showSpace)
import Language.Marlowe.Object.Types
import Pipes (Producer, yield)
import Pipes.Core (Proxy)
import Pipes.Prelude (toListM')

-- | A monad transformer for building Marlowe object bundles.
data BundlerT m a
  = Pure a
  | M (m (BundlerT m a))
  | Define LabelledObject (BundlerT m a)
  deriving (Functor)

type Bundler = BundlerT Identity

instance (forall x. (Eq x) => Eq (m x), Eq a) => Eq (BundlerT m a) where
  b1 == b2 = case b1 of
    Pure a -> case b2 of
      Pure b -> a == b
      _ -> False
    M m1 -> case b2 of
      M m2 -> m1 == m2
      _ -> False
    Define obj1 b1' -> case b2 of
      Define obj2 b2' -> obj1 == obj2 && b1' == b2'
      _ -> False

instance (forall x. (Show x) => Show (m x), Show a) => Show (BundlerT m a) where
  showsPrec p =
    showParen (p > 10) . \case
      Pure a -> showString "Pure" . showSpace . showsPrec 11 a
      M m -> showString "M" . showsPrec 11 m
      Define obj b -> showString "Define" . showSpace . showsPrec 11 obj . showSpace . showsPrec 11 b

-- | A class of monads able to append marlowe objects to a bundle.
class (Monad m) => MonadBundler m where
  define :: LabelledObject -> m ()

instance (MonadBundler m) => MonadBundler (IdentityT m) where
  define = lift . define

instance (MonadBundler m) => MonadBundler (ReaderT r m) where
  define = lift . define

instance (MonadBundler m) => MonadBundler (StateT s m) where
  define = lift . define

instance (MonadBundler m, Monoid w) => MonadBundler (WriterT w m) where
  define = lift . define

instance (MonadBundler m) => MonadBundler (MaybeT m) where
  define = lift . define

instance (MonadBundler m) => MonadBundler (ExceptT e m) where
  define = lift . define

instance (MonadBundler m) => MonadBundler (ContT r m) where
  define = lift . define

instance (MonadBundler m) => MonadBundler (Proxy a' a b' b m) where
  define = lift . define

instance (Functor m) => MonadBundler (BundlerT m) where
  define obj = Define obj $ Pure ()

instance (Functor m) => Applicative (BundlerT m) where
  pure = Pure
  bf <*> ba = go bf
    where
      go = \case
        Pure f -> f <$> ba
        M m -> M (go <$> m)
        Define obj bf' -> Define obj $ go bf'

instance (Functor m) => Monad (BundlerT m) where
  b >>= f = go b
    where
      go = \case
        Pure a -> f a
        M m -> M (go <$> m)
        Define obj b' -> Define obj $ go b'

instance (Functor m) => MonadZip (BundlerT m) where
  mzip = liftA2 (,)

instance (Functor m, Semigroup a) => Semigroup (BundlerT m a) where
  (<>) = liftA2 (<>)

instance (Functor m, Monoid a) => Monoid (BundlerT m a) where
  mempty = pure mempty

instance MonadTrans BundlerT where
  lift = M . fmap pure

instance (MonadFail m) => MonadFail (BundlerT m) where
  fail = lift . fail

instance (MonadIO m) => MonadIO (BundlerT m) where
  liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (BundlerT m) where
  ask = lift ask
  local f = go
    where
      go = \case
        Pure a -> Pure a
        M m -> M $ go <$> local f m
        Define obj b -> Define obj $ go b

instance (MonadState s m) => MonadState s (BundlerT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadWriter w m) => MonadWriter w (BundlerT m) where
  writer = lift . writer
  tell = lift . tell
  listen = go mempty
    where
      go w = \case
        Pure a -> Pure (a, w)
        M m -> M do
          (b, w') <- listen m
          let !w'' = w <> w'
          pure $ go w'' b
        Define obj b -> Define obj $ go w b
  pass = go mempty
    where
      go w = \case
        Pure (a, f) -> M $ pass $ pure (Pure a, const $ f w)
        M m -> M do
          (b, w') <- censor (const mempty) $ listen m
          let !w'' = w <> w'
          pure $ go w'' b
        Define obj b -> Define obj $ go w b

instance (MonadError e m) => MonadError e (BundlerT m) where
  throwError = lift . throwError
  catchError b f = go b
    where
      go = \case
        Pure a -> Pure a
        M m -> M $ (go <$> m) `catchError` (pure . f)
        Define obj b' -> Define obj $ go b'

-- | Run a bundler action, returning the resulting object bundle and the result of the action.
runBundlerT :: (Monad m) => BundlerT m a -> m (ObjectBundle, a)
runBundlerT = (fmap . first) ObjectBundle . toListM' . runBundlerTIncremental

-- | Run a bundler action, returning the resulting object bundle and the result of the action.
runBundlerT_ :: (Monad m) => BundlerT m () -> m ObjectBundle
runBundlerT_ = fmap fst . runBundlerT

-- | Run a bundler action, returning the resulting object bundle and the result of the action.
runBundler :: Bundler a -> (ObjectBundle, a)
runBundler = runIdentity . runBundlerT

-- | Run a bundler action, returning the resulting object bundle and the result of the action.
runBundler_ :: Bundler () -> ObjectBundle
runBundler_ = runIdentity . runBundlerT_

-- | Run a bundler action, streaming the defined objects via a producer.
runBundlerTIncremental :: (Monad m) => BundlerT m a -> Producer LabelledObject m a
runBundlerTIncremental = \case
  Pure a -> pure a
  M m -> runBundlerTIncremental =<< lift m
  Define obj b' -> yield obj *> runBundlerTIncremental b'

-- | Define an object with a given label, returning a reference to the defined object.
defineObject :: (MonadBundler m) => Label -> ObjectType a -> a -> m a
defineObject _label _type _value =
  define LabelledObject{..} $> case _type of
    ValueType -> ValueRef _label
    ObservationType -> ObservationRef _label
    ContractType -> ContractRef _label
    PartyType -> PartyRef _label
    TokenType -> TokenRef _label
    ActionType -> ActionRef _label

-- | Define a value with a given label, returning a reference to the defined value.
defineValue :: (MonadBundler m) => Label -> Value -> m Value
defineValue l = defineObject l ValueType

-- | Define an observation with a given label, returning a reference to the defined observation.
defineObservation :: (MonadBundler m) => Label -> Observation -> m Observation
defineObservation l = defineObject l ObservationType

-- | Define a contract with a given label, returning a reference to the defined contract.
defineContract :: (MonadBundler m) => Label -> Contract -> m Contract
defineContract l = defineObject l ContractType

-- | Define a party with a given label, returning a reference to the defined party.
defineParty :: (MonadBundler m) => Label -> Party -> m Party
defineParty l = defineObject l PartyType

-- | Define a token with a given label, returning a reference to the defined token.
defineToken :: (MonadBundler m) => Label -> Token -> m Token
defineToken l = defineObject l TokenType

-- | Define an action with a given label, returning a reference to the defined action.
defineAction :: (MonadBundler m) => Label -> Action -> m Action
defineAction l = defineObject l ActionType
