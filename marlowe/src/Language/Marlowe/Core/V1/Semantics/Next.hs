-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for Marlowe semantics
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Core.V1.Semantics.Next
  ( AmbiguousIntervalProvided(..)
  , ApplicableGeneralizedInputs(..)
  , CanChoose(..)
  , CanDeposit(..)
  , CanNotify(..)
  , CanReduce(..)
  , CaseIndex(..)
  , Indexed(..)
  , IsMerkleizedContinuation(..)
  , Next(..)
  , next
  , toCaseIndex
  ) where

import Control.Applicative (Alternative((<|>)), Applicative((<*>)), (<$>))
import Data.Aeson (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.Aeson.Types ()
import Data.Bifunctor (Bifunctor(first))
import Data.Coerce (coerce)
import Data.Foldable (Foldable(fold))
import Data.List.Index (indexed)
import Data.Maybe (catMaybes)
import Data.Monoid as Haskell (First(First, getFirst), Monoid(mempty), (<>))

import Deriving.Aeson (Generic)
import Language.Marlowe.Core.V1.Semantics
  ( ReduceResult(ContractQuiescent, RRAmbiguousTimeIntervalError)
  , evalObservation
  , evalValue
  , reduceContractUntilQuiescent
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( AccountId
  , Action(Choice, Deposit, Notify)
  , Bound
  , Case(..)
  , ChoiceId
  , Contract(Close, When)
  , Environment
  , Party
  , State
  , Token
  )
import Language.Marlowe.Pretty (Pretty(..))
import PlutusTx.Prelude (Either(..), Integer, Maybe(..), uncurry, ($), (.))
import qualified Prelude as Haskell

data Next
  = Next
      { canReduce :: CanReduce
      , applicableGeneralizedInputs :: ApplicableGeneralizedInputs}
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving anyclass (Pretty)

data AmbiguousIntervalProvided = AmbiguousIntervalProvided
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving anyclass (Pretty)

-- | Index of an applicable input matching its derived action index in a When [Case Action]
newtype CaseIndex = CaseIndex {unIndex ::Integer}
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving anyclass (Pretty)
    deriving newtype (FromJSON,ToJSON)

data ApplicableGeneralizedInputs
    = ApplicableGeneralizedInputs
     { canNotifyMaybe :: Maybe (Indexed CanNotify)
     , deposits       :: [Indexed CanDeposit]
     , choices        :: [Indexed CanChoose]}
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
  deriving anyclass (Pretty)

data Indexed a
  = Indexed CaseIndex a
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving anyclass (Pretty)

toCaseIndex :: Indexed a -> CaseIndex
toCaseIndex (Indexed c _) = c

instance Haskell.Semigroup ApplicableGeneralizedInputs where
  a <> b  = ApplicableGeneralizedInputs
              (getFirst $ (coerce . canNotifyMaybe $ a) <> (coerce . canNotifyMaybe $ b)) -- First Monoid
              (deposits a   <> deposits b) -- (++) monoid
              (choices a    <> choices b ) -- (++) monoid

instance Haskell.Monoid ApplicableGeneralizedInputs where
  mempty = ApplicableGeneralizedInputs Nothing [] []


newtype IsMerkleizedContinuation = IsMerkleizedContinuation { unIsMerkleizedContinuation :: Haskell.Bool}
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving anyclass (Pretty)
    deriving newtype (FromJSON,ToJSON)

newtype CanNotify = CanNotify IsMerkleizedContinuation
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
  deriving anyclass (Pretty)

data CanDeposit = CanDeposit Party AccountId Token Integer IsMerkleizedContinuation
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
  deriving anyclass (Pretty)

data CanChoose  = CanChoose ChoiceId [Bound] IsMerkleizedContinuation
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
  deriving anyclass (Pretty)

newtype CanReduce = CanReduce { unCanReduce :: Haskell.Bool}
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving newtype (FromJSON,ToJSON,Pretty)

-- | Describe for a given contract which inputs can be applied it can be reduced or not
next :: Environment ->  State  ->  Contract -> Either AmbiguousIntervalProvided Next
next environment state contract
  = do
    (canReduce,reducedState,reducedContract) <- reduceContract environment state contract
    Right
      $ Next
        canReduce
        (applicables environment reducedState reducedContract)

-- | Describe for a given contract which inputs can be applied
applicables :: Environment  -> State  ->  Contract -> ApplicableGeneralizedInputs
applicables  _  _ Close = mempty
applicables environment state (When xs _ _) = applicableGeneralizedInputs' environment state xs
applicables _  _  _  = mempty

applicableGeneralizedInputs' :: Environment -> State -> [Case Contract] -> ApplicableGeneralizedInputs
applicableGeneralizedInputs' environment state
  = fold -- keep only the first Notify evaluated to True (via ApplicableGeneralizedInputs Monoid)
  . catMaybes
  . (uncurry (applicableGeneralizedInputMaybe environment state)  <$>)
  . caseIndexed

applicableGeneralizedInputMaybe :: Environment -> State -> CaseIndex -> Case Contract -> Maybe ApplicableGeneralizedInputs
applicableGeneralizedInputMaybe environment state caseIndex
  = \case
      (merkleizedContinuation,Deposit accountId party token value)
        -> Just
            $ ApplicableGeneralizedInputs
                Nothing
                [Indexed caseIndex $ CanDeposit accountId party token (evalValue environment state value ) merkleizedContinuation]
                []
      (merkleizedContinuation,Choice choiceId bounds)
        -> Just
            $ ApplicableGeneralizedInputs
                Nothing
                []
                [Indexed  caseIndex $ CanChoose choiceId bounds merkleizedContinuation]
      (merkleizedContinuation,Notify observation) | evalObservation environment state observation
        -> Just
            $ ApplicableGeneralizedInputs
                (Just . Indexed  caseIndex $ CanNotify merkleizedContinuation)
                []
                []
      (_,Notify _) -> Nothing
  . \case
      (Case action _)           -> (IsMerkleizedContinuation Haskell.False,action)
      (MerkleizedCase action _) -> (IsMerkleizedContinuation Haskell.True,action)


caseIndexed :: [a] -> [(CaseIndex,a)]
caseIndexed xs = first (CaseIndex . Haskell.fromIntegral) <$> indexed  xs

reduceContract :: Environment -> State -> Contract -> Either AmbiguousIntervalProvided (CanReduce,State,Contract)
reduceContract environment state
  = (\case
      ContractQuiescent _ _ _ newState Close -> Right (CanReduce Haskell.True ,newState,Close)  -- Todo : Add an extra notion of Terminate (N.H)
      ContractQuiescent isReduced _ _ newState newContract -> Right (CanReduce isReduced ,newState,newContract)
      RRAmbiguousTimeIntervalError -> Left AmbiguousIntervalProvided)
    . reduceContractUntilQuiescent environment state


instance FromJSON Next where
  parseJSON (Object v)
    = Next
        <$> v .: "can_reduce"
        <*> v .: "applicable_generalized_inputs"
  parseJSON _ = Haskell.fail "Next must be an object with 2 fields \"can_reduce\" and \"applicable_generalized_inputs\""

instance ToJSON Next where
  toJSON Next {..}
    = object [ "can_reduce" .= canReduce
             , "applicable_generalized_inputs" .= applicableGeneralizedInputs]


instance FromJSON ApplicableGeneralizedInputs where
  parseJSON (Object v)
    =   ApplicableGeneralizedInputs
          <$> v .: "notify"
          <*> v .: "deposits"
          <*> v .: "choices"
    <|> ApplicableGeneralizedInputs Nothing <$> (v .: "deposits")
          <*> v .: "choices"
  parseJSON _ = Haskell.fail "NextGeneralizedInput must be either an object "

instance ToJSON ApplicableGeneralizedInputs where
  toJSON ApplicableGeneralizedInputs {canNotifyMaybe = Nothing,..} = object
      [ "deposits" .= deposits
      , "choices"  .= choices
      ]
  toJSON ApplicableGeneralizedInputs {canNotifyMaybe = Just notify,..} = object
      [ "notify"   .= notify
      , "deposits" .= deposits
      , "choices"  .= choices
      ]


instance FromJSON (Indexed CanNotify) where
  parseJSON (Object v) =  Indexed <$> (CaseIndex <$> v .: "case_index")
                                  <*> (CanNotify <$> v .: "is_merkleized_continuation")
  parseJSON _ = Haskell.fail "CanDeposit must be an object with 1 field \"is_merkleized_continuation\""

instance ToJSON (Indexed CanNotify) where
  toJSON (Indexed caseindex (CanNotify isMerkleizedContinuation)) = object
      [ "case_index" .= caseindex
      , "is_merkleized_continuation" .= isMerkleizedContinuation]

instance FromJSON (Indexed CanDeposit) where
  parseJSON (Object v)
    =  Indexed
         <$>  (CaseIndex <$> v .: "case_index")
         <*>  (CanDeposit
                <$> v .: "party"
                <*> v .: "into_account"
                <*> v .: "of_token"
                <*> v .: "can_deposit"
                <*> v .: "is_merkleized_continuation")
  parseJSON _ = Haskell.fail "CanDeposit must be either an object"

instance ToJSON (Indexed CanDeposit) where
  toJSON (Indexed caseIndex (CanDeposit party accountId token quantity isMerkleizedContinuation)) = object
      [ "party" .= party
      , "can_deposit" .= quantity
      , "of_token" .= token
      , "into_account" .= accountId
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]

instance FromJSON (Indexed CanChoose) where
  parseJSON (Object v)
    =  Indexed
         <$>  (CaseIndex <$> v .: "case_index")
         <*>  (CanChoose
                <$>  v .: "for_choice"
                <*>  v .: "can_choose_between"
                <*>  v .: "is_merkleized_continuation")
  parseJSON _ = Haskell.fail "CanChoose must be an object CanChoose "

instance ToJSON (Indexed CanChoose) where
  toJSON (Indexed caseIndex (CanChoose choiceId bounds isMerkleizedContinuation)) = object
      [ "for_choice" .= choiceId
      , "can_choose_between" .= bounds
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]

