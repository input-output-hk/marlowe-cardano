module Contrib.Control.Monad.Except where

import Control.Monad.Except (MonadError (throwError))

-- MTL generalized version of `Control.Error.note`
note :: (MonadError e m) => e -> Maybe a -> m a
note e = maybe (throwError e) pure
