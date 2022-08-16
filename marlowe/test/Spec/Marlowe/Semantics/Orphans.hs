{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Semantics.Orphans (
) where


import Language.Marlowe.Core.V1.Semantics


deriving instance Eq Payment


deriving instance Eq ApplyWarning


deriving instance Eq ApplyResult


deriving instance Eq ReduceWarning


deriving instance Eq ReduceResult
