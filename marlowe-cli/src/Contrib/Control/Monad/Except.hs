module Contrib.Control.Monad.Except
  where

import Control.Monad.Except (MonadError(throwError))

exceptMaybe :: MonadError e m => e -> Maybe a -> m a
exceptMaybe e = maybe (throwError e) pure
