{-# LANGUAGE LambdaCase #-}

module Language.Marlowe.CLI.Test.Contrib.Monad.Loops where

import Control.Monad.Loops (whileM)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as List

-- | During first pass the counter equals to 0 - first pass is not a retry
newtype RetryCounter = RetryCounter Int

newtype MaxRetries = MaxRetries Int

retryTillJust :: (Monad m) => MaxRetries -> (RetryCounter -> m (Maybe a)) -> m (Maybe a)
retryTillJust (MaxRetries maxRetries) action = go 0
  where
    go cnt
      | maxRetries <= cnt = pure Nothing
      | otherwise = do
          (action $ RetryCounter cnt) >>= \case
            Nothing -> go (cnt + 1)
            res -> pure res

newtype PrevResult e a = PrevResult (Maybe (Either e a))

retryTillRight :: (Monad m) => MaxRetries -> (RetryCounter -> PrevResult e a -> m (Either e a)) -> m (Either e a)
retryTillRight (MaxRetries maxRetries) action = do
  res <- action (RetryCounter 0) (PrevResult Nothing)
  case res of
    Right _ -> pure res
    Left _ -> go 1 res
  where
    go cnt res
      | maxRetries <= cnt = pure res
      | otherwise = do
          res' <- action (RetryCounter cnt) (PrevResult $ Just res)
          case res' of
            Left _ -> go (cnt + 1) res'
            Right _ -> pure res'

untilMNonEmpty :: (Monad m) => m a -> m Bool -> m (List.NonEmpty a)
untilMNonEmpty action condition = do
  x <- action
  xs <- whileM condition action
  pure $ x :| xs
