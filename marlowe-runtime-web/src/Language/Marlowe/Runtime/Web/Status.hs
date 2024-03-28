{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Status (RuntimeStatus (..)) where

import Data.Version (Version)

import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()

import Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId)
import Language.Marlowe.Runtime.Web.Core.Tip (ChainTip)

data RuntimeStatus = RuntimeStatus
  { nodeTip :: ChainTip
  , runtimeChainTip :: ChainTip
  , runtimeTip :: ChainTip
  , networkId :: NetworkId
  , runtimeVersion :: Version
  }
  deriving (Show, Eq, Ord)
