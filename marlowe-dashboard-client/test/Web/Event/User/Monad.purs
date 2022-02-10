module Test.Web.Event.User.Monad where

import Prelude

import Test.Web.Event.User.Api (UserApi)
import Test.Web.Event.User.Options (UserOptions)

class Monad m <= MonadUser m where
  api :: m UserApi
  setup :: forall a. UserOptions -> m a -> m a
