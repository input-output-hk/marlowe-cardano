{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Server.Flags
  where

data Enabled
data Disabled

data Flag a where
  Enabled :: Flag Enabled
  Disabled :: Flag Disabled

data SomeFlag = forall a. SomeFlag (Flag a)
