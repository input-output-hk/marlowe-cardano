{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Operation.Aeson where

import Data.Aeson qualified as A
import Data.Aeson.Key qualified as A.Key
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as A.KeyMap
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text qualified as A
import Data.Aeson.Types qualified as A
import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import GHC.Base (Alternative ((<|>)))
import GHC.Generics (Generic (Rep))

genericJSONOptions :: String -> A.Options
genericJSONOptions prefix = do
  let lowerCaseFirstChar :: String -> String
      lowerCaseFirstChar (c : cs) = Char.toLower c : cs
      lowerCaseFirstChar [] = []

  A.defaultOptions
    { A.fieldLabelModifier = \label ->
        lowerCaseFirstChar $
          fromMaybe label (List.stripPrefix prefix label)
    , A.rejectUnknownFields = True
    }

newtype ConstructorName = ConstructorName String
  deriving (Eq, Ord, Show)

-- Constructor based rewrites.
newtype PreprocessConstructorJSON = PreprocessConstructorJSON (ConstructorName -> Maybe A.Value -> Maybe A.Value)

instance Semigroup PreprocessConstructorJSON where
  (<>) (PreprocessConstructorJSON f) (PreprocessConstructorJSON g) = PreprocessConstructorJSON \cn v -> do
    let v' = f cn v
    g cn v'

instance Monoid PreprocessConstructorJSON where
  mempty = PreprocessConstructorJSON \_ v -> v

-- We want to chain transformations so you should return back the original value if you don't want to change it.
-- This smart constructor guards this invariant.
preprocessConstructorJSON :: (ConstructorName -> Maybe A.Value -> Maybe A.Value) -> PreprocessConstructorJSON
preprocessConstructorJSON f = PreprocessConstructorJSON \cn v -> f cn v <|> v

runPreprocessConstructorJSON :: PreprocessConstructorJSON -> ConstructorName -> Maybe A.Value -> Maybe A.Value
runPreprocessConstructorJSON (PreprocessConstructorJSON f) = f

newtype OldPropName = OldPropName String

newtype NewPropName = NewPropName String

rewritePropWith :: ConstructorName -> OldPropName -> NewPropName -> (A.Value -> A.Value) -> PreprocessConstructorJSON
rewritePropWith expectedConstructor (OldPropName oldPropName) (NewPropName newPropName) f = do
  let preprocess givenConstructor (Just (A.Object (A.KeyMap.toList -> km))) =
        if expectedConstructor == givenConstructor
          then do
            let km' = Map.fromList km
            case Map.lookup (Key.fromString oldPropName) km' of
              Nothing -> Nothing
              Just v -> do
                let km'' = Map.delete (Key.fromString oldPropName) km'
                Just $ A.object $ Map.toList $ Map.insert (Key.fromString newPropName) (f v) km''
          else Nothing
      preprocess _ _ = Nothing
  preprocessConstructorJSON preprocess

rewriteProp :: ConstructorName -> OldPropName -> NewPropName -> PreprocessConstructorJSON
rewriteProp expectedConstructor oldPropName newPropName = rewritePropWith expectedConstructor oldPropName newPropName id

newtype PropName = PropName String

-- Given a non object value like `"Buyer"` in `{ Withdraw: Buyer }`
-- we can transform it to `{ Withdraw: { wallet: Buyer }}` with
-- `rewriteToSingleton (ConstructorName "Withdraw") (PropName "wallet")`.
-- If a given value is an object we don't change it.
rewriteToSingletonObject :: ConstructorName -> PropName -> PreprocessConstructorJSON
rewriteToSingletonObject expectedConstructor (PropName propName) = do
  let preprocess _ (Just (A.Object _)) = Nothing
      preprocess givenConstructor (Just v)
        | expectedConstructor == givenConstructor =
            Just $
              A.object
                [ (Key.fromString propName, v)
                ]
      preprocess _ _ = Nothing
  preprocessConstructorJSON preprocess

rewriteToSingletonObjectConstrutorWith :: ConstructorName -> (A.Value -> A.Value) -> PreprocessConstructorJSON
rewriteToSingletonObjectConstrutorWith constructorName@(ConstructorName cn) f = do
  let preprocess givenConstructor (Just v) =
        if givenConstructor == constructorName
          then
            Just $
              A.object
                [ (Key.fromString "tag", A.String $ Text.pack cn)
                , (Key.fromString "contents", f v)
                ]
          else Nothing
      preprocess _ _ = Nothing
  preprocessConstructorJSON preprocess

rewriteToSingleFieldConstructor :: ConstructorName -> PreprocessConstructorJSON
rewriteToSingleFieldConstructor constructorName = rewriteToSingletonObjectConstrutorWith constructorName id

-- Given a string value like `"Withdraw"` we can transform
-- it to `{ Withdraw: {} }` with
-- `rewriteToEmptyObject (ConstructorName "Withdraw")`.
rewriteToEmptyObject :: ConstructorName -> PreprocessConstructorJSON
rewriteToEmptyObject constructorName = do
  let rewrite givenConstructorName Nothing | givenConstructorName == constructorName = Just $ A.object []
      rewrite _ _ = Nothing
  preprocessConstructorJSON rewrite

parseConstructorBasedJSON
  :: forall a. (A.GFromJSON A.Zero (Rep a)) => (Generic a) => String -> PreprocessConstructorJSON -> A.Value -> A.Parser a
parseConstructorBasedJSON prefix preprocess orig = do
  let json = fromMaybe orig $ case orig of
        (A.Object (KeyMap.toList -> [(k, v)])) -> do
          v' <- runPreprocessConstructorJSON preprocess (ConstructorName $ A.Key.toString k) (Just v)
          pure $ A.Object $ KeyMap.fromList [(k, v')]
        (A.String s) -> do
          case runPreprocessConstructorJSON preprocess (ConstructorName $ Text.unpack s) Nothing of
            Nothing -> Nothing
            Just v' ->
              pure $ A.Object $ KeyMap.fromList [(A.Key.fromText s, v')]
        _ -> Nothing

  case json of
    (A.Object (KeyMap.toList -> [(k, A.Object o)])) -> do
      let kv = KeyMap.toList o
          k' = Key.toText k
          o' = A.object $ ("tag", A.String k') : kv
      A.genericParseJSON (genericJSONOptions prefix) o'
    (A.Object (KeyMap.toList -> [(k, v)])) -> do
      let k' = Key.toText k
          o' =
            A.object
              [ ("tag", A.String k')
              , ("contents", v)
              ]
      A.genericParseJSON (genericJSONOptions prefix) o'
    (A.String s) -> do
      let o = A.object [("tag", A.String s)]
      A.genericParseJSON (genericJSONOptions prefix) o
    _ ->
      fail $
        "Expecting an object when parsing constructor based JSON, got: "
          <> TL.unpack (A.encodeToLazyText json)
          <> ". The original json was:"
          <> TL.unpack (A.encodeToLazyText orig)

parseConstructorBasedJSON' :: forall a. (A.GFromJSON A.Zero (Rep a)) => (Generic a) => String -> A.Value -> A.Parser a
parseConstructorBasedJSON' prefix = parseConstructorBasedJSON prefix mempty

toConstructorBasedJSON :: forall a. (A.GToJSON A.Zero (Rep a)) => (Generic a) => String -> a -> A.Value
toConstructorBasedJSON prefix a = do
  let json = A.genericToJSON (genericJSONOptions prefix) a
  fromMaybe json $ case json of
    A.Object (Map.fromList . KeyMap.toList -> km) -> case List.sort (Map.toList km) of
      [("contents", value), ("tag", A.String constructorName)] ->
        pure $ A.Object $ KeyMap.fromList [(Key.fromText constructorName, value)]
      _ -> do
        constructorName <- Map.lookup "tag" km
        constructorName' <- case constructorName of
          A.String s -> pure s
          _ -> Nothing
        let km' = Map.delete "tag" km
        pure $
          if km' == Map.empty
            then A.String constructorName'
            else A.Object $ KeyMap.fromList [(Key.fromText constructorName', A.object $ Map.toList km')]
    _ -> Nothing

-- For types with single param constructors like:
-- data TestResult = Failure FailureReport | Success SuccessReport
parseSingleFieldConstructorBasedJSON :: forall a. (A.GFromJSON A.Zero (Rep a)) => (Generic a) => A.Value -> A.Parser a
parseSingleFieldConstructorBasedJSON = \case
  (A.Object (KeyMap.toList -> [(k, A.Object o)])) -> do
    let kv = KeyMap.toList o
        k' = Key.toText k
        o' = A.object $ ("tag", A.String k') : kv
    A.genericParseJSON A.defaultOptions o'
  (A.String s) -> do
    let o = A.object [("tag", A.String s)]
    A.genericParseJSON A.defaultOptions o
  _ -> fail "Expecting an object"

toSingleFieldConstructorBasedJSON :: forall a. (A.GToJSON A.Zero (Rep a)) => (Generic a) => a -> A.Value
toSingleFieldConstructorBasedJSON a = do
  let json = A.genericToJSON A.defaultOptions a
  fromMaybe json $ case json of
    A.Object (Map.fromList . KeyMap.toList -> km) -> do
      constructorName <- Map.lookup "tag" km
      constructorName' <- case constructorName of
        A.String s -> pure s
        _ -> Nothing
      let km' = Map.delete "tag" km
      pure $
        if km' == Map.empty
          then A.String constructorName'
          else A.Object $ KeyMap.fromList [(Key.fromText constructorName', A.object $ Map.toList km')]
    _ -> Nothing

-- Plain object JSON is rewritten to a tagged version for a given constructor.
rewriteToSingleConstructorJSON :: ConstructorName -> A.Value -> A.Value
rewriteToSingleConstructorJSON (ConstructorName constructorName) json = do
  A.object [("tag", A.String $ Text.pack constructorName), ("contents", json)]
