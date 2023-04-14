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

genericJSONOptions :: String -> A.Options
genericJSONOptions prefix = do
  let
    lowerCaseFirstChar :: String -> String
    lowerCaseFirstChar (c:cs) = Char.toLower c : cs
    lowerCaseFirstChar [] = []

  A.defaultOptions
    { A.fieldLabelModifier = \label ->
        lowerCaseFirstChar $
        fromMaybe label (List.stripPrefix prefix label)
    , rejectUnknownFields = True
    }

