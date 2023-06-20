{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

module Language.Marlowe.Runtime.Web.Next.Schema
  (
  ) where

import Control.Lens ((&), (.~), (?~))
import Data.OpenApi
  ( HasDescription(description)
  , HasProperties(properties)
  , HasRequired(required)
  , HasType(type_)
  , NamedSchema(NamedSchema)
  , OpenApiType(OpenApiBoolean, OpenApiInteger, OpenApiObject)
  , ToSchema(..)
  , declareSchemaRef
  )
import Data.Proxy (Proxy(Proxy))

import Language.Marlowe.Runtime.Web.Orphans ()

import Language.Marlowe.Core.V1.Next (Next)
import Language.Marlowe.Core.V1.Next.Applicables (ApplicableInputs)
import Language.Marlowe.Core.V1.Next.Applicables.CanChoose (CanChoose)
import Language.Marlowe.Core.V1.Next.Applicables.CanDeposit (CanDeposit)
import Language.Marlowe.Core.V1.Next.Applicables.CanNotify (CanNotify)
import Language.Marlowe.Core.V1.Next.CanReduce (CanReduce)
import Language.Marlowe.Core.V1.Next.Indexed (CaseIndex, Indexed)
import Language.Marlowe.Core.V1.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Bound, ChoiceId, Party, Token)

instance ToSchema Next where
  declareNamedSchema _ = do
    applicableGeneralizedInputsSchema <- declareSchemaRef $ Proxy @ApplicableInputs
    canReduceSchema <- declareSchemaRef $ Proxy @CanReduce
    let
      can_reduce = ("can_reduce", canReduceSchema)
      applicable_generalized_inputs = ("applicable_inputs",applicableGeneralizedInputsSchema)

    pure $ NamedSchema (Just "Next") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Describe the reducibility (Can be Reduced ?) and the applicability (Can Inputs be Applied ?) for a given contract."
        & required .~ fmap fst [can_reduce, applicable_generalized_inputs]
        & properties        .~ [can_reduce, applicable_generalized_inputs]


instance ToSchema ApplicableInputs where
  declareNamedSchema _ = do
    notifySchema <- declareSchemaRef $ Proxy @(Indexed CanNotify)
    depositsSchema <- declareSchemaRef $ Proxy @[Indexed CanDeposit]
    choicesSchema <- declareSchemaRef $ Proxy @[Indexed CanChoose]
    let
      notify    = ("notify", notifySchema)
      deposits  = ("deposits", depositsSchema)
      choices   = ("choices", choicesSchema)
    pure $ NamedSchema (Just "ApplicableInputs") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Applicable Inputs for a given contract"
        & required .~ fmap fst [deposits, choices]
        & properties        .~ [notify, deposits, choices]


instance ToSchema (Indexed CanDeposit) where
  declareNamedSchema _ = do
    partySchema <- declareSchemaRef $ Proxy @Party
    accountIdSchema <- declareSchemaRef $ Proxy @AccountId
    tokenSchema <- declareSchemaRef $ Proxy @Token
    quantitySchema <- declareSchemaRef $ Proxy @Integer
    caseIndexSchema <- declareSchemaRef $ Proxy @CaseIndex
    isMrkleizedContinuationSchema <- declareSchemaRef $ Proxy @IsMerkleizedContinuation
    let
      case_index   = ("case_index", caseIndexSchema)
      party        = ("party", partySchema)
      can_deposit  = ("can_deposit", quantitySchema)
      of_token     = ("of_token", tokenSchema)
      into_account = ("into_account", accountIdSchema)
      is_merkleized_continuation = ("is_merkleized_continuation", isMrkleizedContinuationSchema)
    pure $ NamedSchema (Just "CanDeposit") $ mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Deposit Input that can be applied for a given contract"
        & required .~ fmap fst [party, can_deposit, of_token, into_account,case_index,is_merkleized_continuation]
        & properties        .~ [party, can_deposit, of_token, into_account,case_index,is_merkleized_continuation]

instance ToSchema (Indexed CanNotify) where
  declareNamedSchema _ = do
    isMrkleizedContinuationSchema <- declareSchemaRef $ Proxy @IsMerkleizedContinuation
    caseIndexSchema <- declareSchemaRef $ Proxy @CaseIndex
    let
      is_merkleized_continuation = ("is_merkleized_continuation", isMrkleizedContinuationSchema)
      case_index          = ("case_index", caseIndexSchema)
    pure $ NamedSchema (Just "CanNotify") $ mempty
        & type_ ?~ OpenApiObject
         & description ?~ "Notify Input tha can be applied for a given contract"
        & required .~ fmap fst [is_merkleized_continuation,case_index]
        & properties        .~ [is_merkleized_continuation, case_index]

instance ToSchema (Indexed CanChoose) where
  declareNamedSchema _ = do
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    boundSchema <- declareSchemaRef $ Proxy @[Bound]
    caseIndexSchema <- declareSchemaRef $ Proxy @CaseIndex
    isMrkleizedContinuationSchema <- declareSchemaRef $ Proxy @IsMerkleizedContinuation
    let
      case_index          = ("case_index", caseIndexSchema)
      for_choice          = ("for_choice", choiceIdSchema)
      can_choose_between  = ("can_choose_between", boundSchema)
      is_merkleized_continuation = ("is_merkleized_continuation", isMrkleizedContinuationSchema)
    pure $ NamedSchema (Just "CanChoose") $ mempty
        & type_ ?~ OpenApiObject
         & description ?~ "Choice Inputs that can be applied for a given contract"
        & required .~ fmap fst [for_choice, can_choose_between,case_index,is_merkleized_continuation]
        & properties        .~ [for_choice, can_choose_between,case_index,is_merkleized_continuation]


instance ToSchema CaseIndex where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "CaseIndex") $ mempty
      & type_ ?~ OpenApiInteger
      & description ?~ "Index of a \"Case Action\" in a \"When\""

instance ToSchema CanReduce where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "CanReduce") $ mempty
      & type_ ?~ OpenApiBoolean
      & description ?~ "Indicates if a given contract can be reduced (apply []) or not."

instance ToSchema IsMerkleizedContinuation where
  declareNamedSchema _ = do
    pure $ NamedSchema (Just "IsMerkleizedContinuation") $ mempty
      & type_ ?~ OpenApiBoolean
      & description ?~ "Indicates if a given contract continuation is merkleized"
