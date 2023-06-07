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

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Language.Marlowe.Core.V1.Semantics.Next
  ( AmbiguousIntervalProvided(..)
  , CaseIndex(..)
  , Next(..)
  , NextGeneralizedInput(..)
  , next
  ) where

import Control.Applicative
import Control.Monad.Cont ((=<<))
import Data.Aeson
import Data.Aeson.Types ()
import Data.Bifunctor (Bifunctor(first))
import Data.List.Index (indexed)
import Data.List.NonEmpty as NEL (NonEmpty(..), nonEmpty, toList)
import Data.Maybe (catMaybes)
import Data.Text (pack)
import Deriving.Aeson (Generic)
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types hiding (environment)
import Language.Marlowe.Pretty (Pretty(..))
import PlutusTx.Prelude (Bool(..), Either(..), Integer, Maybe(..), maybe, uncurry, ($), (.))
import qualified Prelude as Haskell




data Next = CanAdvance [NextGeneralizedInput]
          | CanNotAdvance
    deriving stock (Haskell.Show)

data AmbiguousIntervalProvided = AmbiguousIntervalProvided

newtype CaseIndex = CaseIndex {unIndex ::Integer}
    deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
    deriving anyclass (Pretty)
    deriving (FromJSON,ToJSON)

-- | Next Ranges of Inputs that can be applied to a given contract
data NextGeneralizedInput
  = CanDeposit CaseIndex Party AccountId Token Integer -- 1 possibility > 1 action
  | CanChoose  CaseIndex ChoiceId [Bound] -- 1 possibility > many different actions
  | CanNotify  CaseIndex -- 1 possibility > 1 action
  deriving stock (Haskell.Show,Haskell.Eq,Haskell.Ord,Generic)
  deriving anyclass (Pretty)


next :: Environment ->  State  ->  Contract -> Either AmbiguousIntervalProvided Next
next  _ _ Close         = Right . CanAdvance $ []
next  _ _ (When [] _ _) = Right CanNotAdvance   -- Contract Suspended in a time out
next environment state (When (x:xs) _ _)
  = Right
    . maybe
          CanNotAdvance -- Contract Suspended (only falsified Notifies evaluated in the When)
        ( CanAdvance . keepFirstNotifyTrueAndRemoveFollowings  )
    . nextGeneralizedInputsMaybe environment state
    $ x :| xs
next environment state  contract  = uncurry (next environment) =<< reduceContract environment state contract


keepFirstNotifyTrueAndRemoveFollowings ::  NonEmpty NextGeneralizedInput -> [NextGeneralizedInput]
keepFirstNotifyTrueAndRemoveFollowings
  = keepFirstNotifyTrueAndRemoveFollowings' False . NEL.toList
 where
  keepFirstNotifyTrueAndRemoveFollowings' :: Bool -> [NextGeneralizedInput] -> [NextGeneralizedInput]
  keepFirstNotifyTrueAndRemoveFollowings' _ [] =  []
  keepFirstNotifyTrueAndRemoveFollowings' False (x@CanNotify {} : xs) =  keepFirstNotifyTrueAndRemoveFollowings' True (x:xs)
  keepFirstNotifyTrueAndRemoveFollowings' True (CanNotify {} : xs) =  keepFirstNotifyTrueAndRemoveFollowings' True xs
  keepFirstNotifyTrueAndRemoveFollowings' _ xs =  keepFirstNotifyTrueAndRemoveFollowings' True xs


nextGeneralizedInputsMaybe :: Environment -> State -> NonEmpty (Case Contract) -> Maybe (NonEmpty NextGeneralizedInput)
nextGeneralizedInputsMaybe environment state
  = nonEmpty
  . catMaybes
  . (uncurry (nextGeneralizedInputMaybe environment state)  <$>)
  . caseIndexed

nextGeneralizedInputMaybe :: Environment -> State -> CaseIndex -> Case Contract -> Maybe NextGeneralizedInput
nextGeneralizedInputMaybe environment state caseIndex
  = \case
      (Deposit accountId party token value)
        -> Just $ CanDeposit
                caseIndex
                accountId
                party
                token
                (evalValue environment state value )
      (Choice choiceId bounds) -> Just $ CanChoose caseIndex choiceId bounds
      (Notify observation) | evalObservation environment state observation -> Just $ CanNotify caseIndex
      (Notify _) -> Nothing
  . \case
      (Case action _)           -> action
      (MerkleizedCase action _) -> action

caseIndexed :: NonEmpty a -> [(CaseIndex,a)]
caseIndexed xs = first (CaseIndex . Haskell.fromIntegral) <$> ( indexed . NEL.toList $ xs)


reduceContract :: Environment -> State -> Contract -> Either AmbiguousIntervalProvided (State,Contract)
reduceContract environment state
  = (\case
      ContractQuiescent _ _ _ newState newContract -> Right (newState,newContract)
      RRAmbiguousTimeIntervalError -> Left AmbiguousIntervalProvided)
    . reduceContractUntilQuiescent environment state

instance FromJSON Next where
  parseJSON (String _) = pure CanNotAdvance
  parseJSON (Object v) = CanAdvance <$> v .: "can_advance_with"
  parseJSON _ = Haskell.fail "Next must be an object with 2 fiels \"canAdvance\" and \"can_advance_with\""

instance ToJSON Next where
  toJSON (CanAdvance nextGeneralizedInputs) = object [ "can_advance_with" .= nextGeneralizedInputs]
  toJSON CanNotAdvance = String . pack $ "can_not_advance"

instance FromJSON NextGeneralizedInput where
  parseJSON (Object v)
    =   CanChoose
          <$> v .: "for_choice"
          <*> v .: "can_choose_between"
          <*> v .: "case_index"
    <|> CanDeposit
          <$> v .: "party"
          <*> v .: "into_account"
          <*> v .: "of_token"
          <*> v .: "can_deposit"
          <*> v .: "case_index"
    <|> CanNotify <$> v .: "can_notify_case_index"
  parseJSON _ = Haskell.fail "NextGeneralizedInput must be either an object CanChoose | CanDeposit | CanNotify "

instance ToJSON NextGeneralizedInput where
  toJSON (CanDeposit party accountId token quantity caseIndex) = object
      [ "party" .= party
      , "can_deposit" .= quantity
      , "of_token" .= token
      , "into_account" .= accountId
      , "case_index" .= caseIndex
      ]
  toJSON (CanChoose choiceId bounds caseIndex) = object
      [ "for_choice" .= choiceId
      , "can_choose_between" .= bounds
      , "case_index" .= caseIndex
      ]
  toJSON (CanNotify caseIndex)  = object ["can_notify_case_index" .= caseIndex]

