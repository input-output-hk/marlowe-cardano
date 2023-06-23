module Contrib.Control.Exception where

import Control.Exception (Exception, throwIO)

noteIO :: (Exception e) => e -> Maybe a -> IO a
noteIO e = maybe (throwIO e) pure

liftEitherIO :: (Exception e) => Either e a -> IO a
liftEitherIO = either throwIO pure
