{-# LANGUAGE LambdaCase #-}

module Contrib.Monad.Loops
  where

-- | During first pass the counter equals to 0 - first pass is not a retry
newtype RetryCounter = RetryCounter Int
newtype MaxRetries = MaxRetries Int

retryTillJust :: Monad m => MaxRetries -> (RetryCounter -> m (Maybe a)) -> m (Maybe a)
retryTillJust (MaxRetries maxRetries) action = go 0
  where
    go cnt
      | maxRetries <= cnt = pure Nothing
      | otherwise = do
          (action $ RetryCounter cnt) >>= \case
            Nothing -> go (cnt + 1)
            res     -> pure res

