{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Contrib.Data.Aeson.Traversals
  where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Traversable (for)


traverseJSON :: Applicative f => (A.Value -> f A.Value) -> A.Value -> f A.Value
traverseJSON f = \case
  A.Object props ->
    A.Object . KeyMap.fromList <$> for (KeyMap.toList props) \(k, v) -> (k,) <$> f v
  A.Array elems -> do
    A.Array <$> for elems \e -> f e
  val -> pure val


rewriteBottomUp :: Monad m => (A.Value -> m A.Value) -> A.Value -> m A.Value
rewriteBottomUp f = do
  let
    visitor json = do
      json' <- traverseJSON visitor json
      f json'
  visitor


rewriteTopDown :: Monad m => (A.Value -> m A.Value) -> A.Value -> m A.Value
rewriteTopDown f = do
  let
    visitor json = do
      json' <- f json
      traverseJSON visitor json'
  visitor
