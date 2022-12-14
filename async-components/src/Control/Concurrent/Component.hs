{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Concurrent.Component
  where

import Control.Applicative (liftA2)
import Control.Arrow
import Control.Category
import Control.Concurrent.Async.Lifted (Async, Concurrently(..), waitCatchSTM, withAsync)
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.STM
import Control.Exception.Base (SomeException)
import Control.Exception.Lifted (try)
import Control.Monad (forever, join)
import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Control (MonadBaseControl(StM))
import qualified Data.Bifunctor as B
import Data.Functor (void)
import Prelude hiding ((.))

newtype Component m a b = Component { unComponent :: a -> STM (Concurrently m (), b) }
  deriving Functor

instance MonadBaseControl IO m => Applicative (Component m a) where
  pure = Component . pure . pure . pure
  cf <*> cx = Component $ (liftA2 . liftA2) (<*>) (unComponent cf) (unComponent cx)

instance MonadBaseControl IO m => Monad (Component m a) where
  c >>= k = Component \a -> do
    (run1, x) <- unComponent c a
    (run2, b) <- unComponent (k x) a
    pure (run1 <> run2, b)

instance MonadBaseControl IO m => Category (Component m) where
  id = Component \a -> pure (pure (), a)
  cbc . cab = Component \a -> do
    (run1, b) <- unComponent cab a
    (run2, c) <- unComponent cbc b
    pure (run1 <> run2, c)

instance MonadBaseControl IO m => Arrow (Component m) where
  arr f = Component \a -> pure (pure (), f a)
  c1 *** c2 = Component \(a, b) -> do
    (run1, c) <- unComponent c1 a
    (run2, d) <- unComponent c2 b
    pure (run1 <> run2, (c, d))
  c1 &&& c2 = Component \a -> do
    (run1, b) <- unComponent c1 a
    (run2, c) <- unComponent c2 a
    pure (run1 <> run2, (b, c))
  first c = Component \(a, d) -> fmap (,d) <$> unComponent c a
  second c = Component \(d, a) -> fmap (d,) <$> unComponent c a

instance MonadBaseControl IO m => ArrowChoice (Component m) where
  c1 +++ c2 = Component $ either
    ((fmap . fmap) Left . unComponent c1)
    ((fmap . fmap) Right . unComponent c2)
  c1 ||| c2 = Component $ either (unComponent c1) (unComponent c2)
  left c = Component $ either
    ((fmap . fmap) Left . unComponent c)
    (pure . pure . Right)
  right c = Component $ either
    (pure . pure . Left)
    ((fmap . fmap) Right . unComponent c)

instance MonadBaseControl IO m => ArrowApply (Component m) where
  app = Component $ uncurry unComponent

instance MonadBaseControl IO m => ArrowLoop (Component m) where
  loop c = Component \a -> mdo
    (run, (b, d)) <- unComponent c (a, d)
    pure (run, b)

supervisor :: (MonadBaseControl IO m, MonadIO m) => Component m a b -> Component m a (STM b)
supervisor c = Component \a -> do
  (Concurrently run, b) <- unComponent c a
  outputVar <- newTVar b
  let
    runWithRetry run' = withAsync run' \async -> join $ liftIO $ atomically do
      result <- waitCatchSTM async
      case result of
        Left _ -> do
          (Concurrently run'', b') <- unComponent c a
          writeTVar outputVar b'
          pure $ runWithRetry run''
        Right _ -> pure $ pure ()
  pure (Concurrently $ runWithRetry run, readTVar outputVar)

supervisor_ :: (MonadBaseControl IO m, MonadIO m) => Component m a () -> Component m a ()
supervisor_ c = Component \a -> do
  (Concurrently run, _) <- unComponent c a
  let
    runWithRetry run' = withAsync run' \async -> join $ liftIO $ atomically do
      result <- waitCatchSTM async
      case result of
        Left _ -> do
          (Concurrently run'', _) <- unComponent c a
          pure $ runWithRetry run''
        Right _ -> pure $ pure ()
  pure (Concurrently $ runWithRetry run, ())

suppressErrors :: MonadBaseControl IO m => Component m a b -> Component m a b
suppressErrors c = Component \a -> do
  (Concurrently run, b) <- unComponent c a
  pure (Concurrently $ void $ try @_ @SomeException run, b)

runComponent :: Component m a b -> a -> STM (m (), b)
runComponent c a = do
  (run, b) <- unComponent c a
  pure (runConcurrently run, b)

runComponent_ :: MonadBaseControl IO m => Component m a () -> a -> m ()
runComponent_ c a = fst =<< liftBase (atomically $ runComponent c a)

withComponent :: MonadBaseControl IO m => Component m a b -> a -> (b -> Async (StM m ()) -> m c) -> m c
withComponent c a f = do
  (Concurrently run, b) <- liftBase $ atomically $ unComponent c a
  withAsync run $ f b

withComponent_ :: MonadBaseControl IO m => Component m a () -> a -> (Async (StM m ()) -> m c) -> m c
withComponent_ c a = withComponent c a . const

component :: (a -> STM (m (), b)) -> Component m a b
component run = Component $ (fmap . B.first) Concurrently . run

component_ :: (a -> m ()) -> Component m a ()
component_ = component . fmap (pure . (,()))

serverComponent :: forall m a b . MonadBaseControl IO m => Component m b () -> (a -> m b) -> Component m a ()
serverComponent worker = serverComponentWithSetup worker . (pure .)

serverComponentWithSetup
  :: forall m a b
   . MonadBaseControl IO m
  => Component m b ()
  -> (a -> STM (m b))
  -> Component m a ()
serverComponentWithSetup worker mkAccept = component \a -> do
  accept <- mkAccept a
  let
    run = forever do
      b <- accept
      void $ fork $ runComponent_ (suppressErrors worker) b
  pure (run, ())
