{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.CLI.Test.Runtime.Thread
  where

-- Curretly we don't need any extra information for the runtime thread.
type RuntimeTxInfo = ()

type RuntimeMarloweThread lang era = MarloweThread RuntimeTxInfo lang era

type AnyRuntimeMarloweThread lang era = AnyMarloweThread RuntimeTxInfo lang era

anyRuntimeMarloweThread :: Maybe C.TxIn
                        -> [M.Input]
                        -> AnyRuntimeMarloweThread lang era
                        -> Maybe (AnyRuntimeMarloweThread lang era)
anyRuntimeMarloweThread = anyMarloweThread ()

