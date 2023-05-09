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
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Operation.Aeson
  where

import qualified Data.Aeson as A
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import qualified Data.Aeson.Types as A
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import GHC.Generics (Generic(Rep))

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
    , A.rejectUnknownFields = True
    }

parseConstructorBasedJSON :: forall a. A.GFromJSON A.Zero (Rep a) => Generic a => String -> A.Value -> A.Parser a
parseConstructorBasedJSON prefix = \case
  (A.Object (KeyMap.toList -> [(k, A.Object o)])) -> do
    let
      kv = KeyMap.toList o
      k' = Key.toText k
      o' = A.object $ ("tag", A.String k') : kv
    A.genericParseJSON (genericJSONOptions prefix) o'
  (A.String s) -> do
    let
      o = A.object [("tag", A.String s)]
    A.genericParseJSON (genericJSONOptions prefix) o
  json -> fail $ "Expecting an object when parsing constructor based JSON, got: " <> Text.unpack (A.renderValue json)

toConstructorBasedJSON :: forall a. A.GToJSON A.Zero (Rep a) => Generic a => String -> a -> A.Value
toConstructorBasedJSON prefix a = do
  let
    json = A.genericToJSON (genericJSONOptions prefix) a
  fromMaybe json $ case json of
    A.Object (Map.fromList . KeyMap.toList -> km) -> do
      constructorName <- Map.lookup "tag" km
      constructorName' <- case constructorName of
        A.String s -> pure s
        _ -> Nothing
      let
        km' = Map.delete "tag" km
      pure $ if km' == Map.empty
        then A.String constructorName'
        else A.Object $ KeyMap.fromList [(Key.fromText constructorName', A.object $ Map.toList km')]
    _ -> Nothing

parseSingleFieldConstructorBasedJSON :: forall a. A.GFromJSON A.Zero (Rep a) => Generic a => A.Value -> A.Parser a
parseSingleFieldConstructorBasedJSON = \case
  (A.Object (KeyMap.toList -> [(k, A.Object o)])) -> do
    let
      kv = KeyMap.toList o
      k' = Key.toText k
      o' = A.object $ ("tag", A.String k') : kv
    A.genericParseJSON A.defaultOptions o'
  (A.String s) -> do
    let
      o = A.object [("tag", A.String s)]
    A.genericParseJSON A.defaultOptions o
  _ -> fail "Expecting an object"

toSingleFieldConstructorBasedJSON :: forall a. A.GToJSON A.Zero (Rep a) => Generic a => a -> A.Value
toSingleFieldConstructorBasedJSON a = do
  let
    json = A.genericToJSON A.defaultOptions a
  fromMaybe json $ case json of
    A.Object (Map.fromList . KeyMap.toList -> km) -> do
      constructorName <- Map.lookup "tag" km
      constructorName' <- case constructorName of
        A.String s -> pure s
        _ -> Nothing
      let
        km' = Map.delete "tag" km
      pure $ if km' == Map.empty
        then A.String constructorName'
        else A.Object $ KeyMap.fromList [(Key.fromText constructorName', A.object $ Map.toList km')]
    _ -> Nothing
