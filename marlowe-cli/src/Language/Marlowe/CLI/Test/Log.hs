{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

module Language.Marlowe.CLI.Test.Log
  where

import Contrib.Data.Aeson.Generic (GetConName, constructorName)
import Control.Monad.Error.Class (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Generics (Generic(Rep))
import Language.Marlowe.CLI.Types (CliError(CliError))

printTraceMsg :: String -> String -> String
printTraceMsg loc msg = "[" <> loc <> "]" <> " " <> msg

logTraceMsg :: MonadIO m => String -> String -> m ()
logTraceMsg loc msg = liftIO . putStrLn $ printTraceMsg loc msg

throwTraceError :: MonadError CliError m => String -> String -> m a
throwTraceError loc msg = throwError
    $ CliError
    $ printTraceMsg loc msg

data LabelFormat = LabelShow | LabelConstructorName

type Label l = (Show l, Generic l, GetConName (Rep l))

printLabel :: Label l => LabelFormat -> l -> String
printLabel LabelShow l = show l
printLabel LabelConstructorName l = constructorName l

printLabeledMsg :: (Label l) => LabelFormat -> l -> String -> String
printLabeledMsg format l = printTraceMsg (printLabel format l)

throwLabeledError :: Label l => MonadError CliError m => l -> String -> m a
throwLabeledError loc msg = throwError
    $ CliError
    $ printLabeledMsg LabelConstructorName loc msg

exceptLabeledMaybe :: Label l => MonadError CliError m => l -> String -> Maybe a -> m a
exceptLabeledMaybe _ _ (Just a) = pure a
exceptLabeledMaybe label msg _ = throwLabeledError label msg

logLabeledMsg :: Label l => MonadIO m => l -> String -> m ()
logLabeledMsg l msg = liftIO . putStrLn $ printLabeledMsg LabelConstructorName l msg
