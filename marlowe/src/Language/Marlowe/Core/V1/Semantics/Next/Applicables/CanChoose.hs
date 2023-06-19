
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass #-}

{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Core.V1.Semantics.Next.Applicables.CanChoose
  ( CanChoose(..)
  , compactAdjoinedBounds
  , difference
  , overlaps
  ) where


import Control.Monad ((<=<))
import Data.Aeson.Types (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.List (tails)
import GHC.Generics
import qualified Language.Marlowe.Core.V1.Semantics.Next.Applicables.Bound as Bound
import Language.Marlowe.Core.V1.Semantics.Next.Indexed
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types (Bound(..), ChoiceId)
import Language.Marlowe.Pretty (Pretty)
import Prelude


data CanChoose
    = CanChoose { choiceId :: ChoiceId
                , bounds :: [Bound]
                , isMerkleizedContinuation :: IsMerkleizedContinuation}
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)

overlaps :: [Indexed CanChoose]  -> Bool
overlaps xs = overlaps' $ getIndexedValue <$> xs

overlaps' :: [CanChoose]  -> Bool
overlaps' l
    = let combinations = [(x,y) | (x:ys) <- tails l, y <- ys]
      in any (uncurry overlapWith) combinations

overlapWith :: CanChoose -> CanChoose -> Bool
overlapWith a b | choiceId a /= choiceId b
    = let combinations = [(x,y) | x <- bounds a, y <- bounds b]
      in any (uncurry Bound.overlapWith) combinations
overlapWith _ _ = False


boundsByChoiceId :: ChoiceId -> [Indexed CanChoose] -> [Bound]
boundsByChoiceId choiceId' = bounds <=< (filter(\x -> choiceId'  == choiceId x) . fmap getIndexedValue)

compactAdjoinedBounds :: [Indexed CanChoose]  -> [Bound]
compactAdjoinedBounds xs = Bound.compactAdjoinedBounds ((bounds . getIndexedValue) =<< xs)


difference :: Indexed CanChoose -> [Indexed CanChoose] -> Maybe (Indexed CanChoose)
difference (Indexed i CanChoose {..})
    = (\case
        [] -> Nothing
        newBounds -> Just $ Indexed i (CanChoose choiceId newBounds isMerkleizedContinuation))
    . Bound.difference bounds
    . boundsByChoiceId choiceId


instance FromJSON (Indexed CanChoose) where
  parseJSON (Object v)
    =  Indexed
        <$> (CaseIndex <$> v .: "case_index")
        <*> (CanChoose
              <$>  v .: "for_choice"
              <*>  v .: "can_choose_between"
              <*>  v .: "is_merkleized_continuation")
  parseJSON _ = fail "CanChoose must be an object CanChoose "

instance ToJSON (Indexed CanChoose) where
  toJSON (Indexed caseIndex (CanChoose choiceId bounds isMerkleizedContinuation)) = object
      [ "for_choice" .= choiceId
      , "can_choose_between" .= bounds
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]
