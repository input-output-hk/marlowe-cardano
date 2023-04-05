{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.CLI.Test.Operation.Aeson
  where

import Data.Aeson as A
import Data.Char as Char
import Data.List as List
import Data.Maybe (fromMaybe)

genericParseJSONOptions :: String -> A.Options
genericParseJSONOptions prefix = do
  let
    lowerCaseFirstChar :: String -> String
    lowerCaseFirstChar (c:cs) = Char.toLower c : cs
    lowerCaseFirstChar [] = []

  A.defaultOptions
    { A.fieldLabelModifier = \label ->
        lowerCaseFirstChar $
        fromMaybe label (List.stripPrefix prefix label)
    }

