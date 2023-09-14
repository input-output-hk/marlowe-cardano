{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.CLI.Test.Log.Label where

import Contrib.Data.Aeson.Generic (GetConName, constructorName)
import GHC.Generics (Generic (Rep))

-- We should use proper tracing or nested namespace tracking
-- but for now we just use a string label.
class Label l where
  label :: l -> String

instance {-# OVERLAPPABLE #-} (Generic l, GetConName (Rep l)) => Label l where
  label = constructorName

instance {-# OVERLAPPING #-} Label String where
  label = id
