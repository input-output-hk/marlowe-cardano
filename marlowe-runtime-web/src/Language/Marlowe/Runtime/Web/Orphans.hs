{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Orphans
  where

import Control.Lens hiding (both, from, to)
import Data.OpenApi hiding (value)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import GHC.Exts (IsList(fromList))
import Language.Marlowe.Core.V1.Semantics.Types

data Address

instance ToSchema Address where
  declareNamedSchema _ = pure $ NamedSchema (Just "Address") $ mempty
    & type_ ?~ OpenApiString
    & description ?~ "A cardano address"

instance ToSchema ChoiceId where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    partySchema <- declareSchemaRef $ Proxy @Party
    let choice_name = ("choice_name", stringSchema)
    let choice_owner = ("choice_owner", partySchema)
    pure $ NamedSchema (Just "ChoiceId") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "Refers to a party by role name."
      & required .~ fmap fst [choice_name, choice_owner]
      & properties .~ [choice_name, choice_owner]

instance ToSchema Party where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    addressSchema <- declareSchemaRef $ Proxy @Address
    let
      rolePartySchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Refers to a party by role name."
        & required .~ fmap fst [role_token]
        & properties .~ [role_token]
        where
          role_token = ("role_token", stringSchema)
      addressPartySchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Refers to a party by Cardano address."
        & required .~ fmap fst [address]
        & properties .~ [address]
        where
          address = ("address", addressSchema)
    pure $ NamedSchema (Just "Party") $ mempty
      & description ?~ "A participant in a contract"
      & oneOf ?~ fmap Inline [rolePartySchema, addressPartySchema]

instance ToSchema Payee where
  declareNamedSchema _ = do
    partySchema <- declareSchemaRef $ Proxy @Party
    let
      accountPayeeSchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Pays funds into a party's account in the contract."
        & required .~ fmap fst [account]
        & properties .~ [account]
        where
          account = ("account", partySchema)
      partyPayeeSchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Pays funds to a party."
        & required .~ fmap fst [party]
        & properties .~ [party]
        where
          party = ("party", partySchema)
    pure $ NamedSchema (Just "Payee") $ mempty
      & description ?~ "A recipient of a payment"
      & oneOf ?~ fmap Inline [accountPayeeSchema, partyPayeeSchema]

instance ToSchema Token where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    let currency_symbol = ("currency_symbol", stringSchema)
    let token_name = ("token_name", stringSchema)
    pure $ NamedSchema (Just "Token") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "A token with a currency symbol (minting policy ID) and token name."
      & required .~ fmap fst [currency_symbol, token_name]
      & properties .~ [currency_symbol, token_name]

instance ToSchema a => ToSchema (Value a) where
  declareNamedSchema _ = do
    accountIdSchema <- declareSchemaRef $ Proxy @AccountId
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    tokenSchema <- declareSchemaRef $ Proxy @Token
    valueSchema <- declareSchemaRef $ Proxy @(Value a)
    observationSchema <- declareSchemaRef $ Proxy @a
    stringSchema <- declareSchemaRef $ Proxy @String
    let
      timeIntervalSchema = mempty
        & type_ ?~ OpenApiString
        & enum_ ?~ ["time_interval_start", "time_interval_end"]
      constantSchema = mempty
        & type_ ?~ OpenApiInteger
        & format ?~ "int64"
      availableMoneySchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [amount_of_token, in_account]
        & properties .~ [amount_of_token, in_account]
        where
          amount_of_token = ("amount_of_token", tokenSchema)
          in_account = ("in_account", accountIdSchema)
      negValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [negate_]
        & properties .~ [negate_]
        where
          negate_ = ("negate", valueSchema)
      addValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [val1, val2]
        & properties .~ [val1, val2]
        where
          val1 = ("add", valueSchema)
          val2 = ("and", valueSchema)
      subValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [val1, val2]
        & properties .~ [val1, val2]
        where
          val1 = ("value", valueSchema)
          val2 = ("minus", valueSchema)
      mulValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [val1, val2]
        & properties .~ [val1, val2]
        where
          val1 = ("multiply", valueSchema)
          val2 = ("times", valueSchema)
      divValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [val1, val2]
        & properties .~ [val1, val2]
        where
          val1 = ("divide", valueSchema)
          val2 = ("by", valueSchema)
      choiceValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [value_of_choice]
        & properties .~ [value_of_choice]
        where
          value_of_choice = ("value_of_choice", choiceIdSchema)
      useValueSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [use_value]
        & properties .~ [use_value]
        where
          use_value = ("use_value", stringSchema)
      condSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [if_, then_, else_]
        & properties .~ [if_, then_, else_]
        where
          if_ = ("if", observationSchema)
          then_ = ("then", valueSchema)
          else_ = ("else", valueSchema)
    pure $ NamedSchema (Just "Value") $ mempty
      & description ?~ "A time-varying expression that evaluates to a boolean"
      & oneOf ?~ fmap Inline
          [ availableMoneySchema
          , constantSchema
          , negValueSchema
          , addValueSchema
          , subValueSchema
          , mulValueSchema
          , divValueSchema
          , choiceValueSchema
          , timeIntervalSchema
          , useValueSchema
          , condSchema
          ]

instance ToSchema Observation where
  declareNamedSchema _ = do
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    valueSchema <- declareSchemaRef $ Proxy @(Value Observation)
    observationSchema <- declareSchemaRef $ Proxy @Observation
    let
      constantSchema = mempty & type_ ?~ OpenApiBoolean
      andObsSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [both, and_]
        & properties .~ [both, and_]
        where
          both = ("both", observationSchema)
          and_ = ("and", observationSchema)
      orObsSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [either_, or_]
        & properties .~ [either_, or_]
        where
          either_ = ("either", observationSchema)
          or_ = ("or", observationSchema)
      notObsSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [p]
        & properties .~ [p]
        where
          p = ("not", observationSchema)
      choseSomethingSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [chose_something_for]
        & properties .~ [chose_something_for]
        where
          chose_something_for = ("chose_something_for", choiceIdSchema)
      valueGESchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [value, ge_than]
        & properties .~ [value, ge_than]
        where
          value = ("value", valueSchema)
          ge_than = ("ge_than", valueSchema)
      valueGTSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [value, gt]
        & properties .~ [value, gt]
        where
          value = ("value", valueSchema)
          gt = ("gt", valueSchema)
      valueLESchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [value, le_than]
        & properties .~ [value, le_than]
        where
          value = ("value", valueSchema)
          le_than = ("le_than", valueSchema)
      valueLTSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [value, lt]
        & properties .~ [value, lt]
        where
          value = ("value", valueSchema)
          lt = ("lt", valueSchema)
      valueEQSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [value, equal_to]
        & properties .~ [value, equal_to]
        where
          value = ("value", valueSchema)
          equal_to = ("equal_to", valueSchema)
    pure $ NamedSchema (Just "Observation") $ mempty
      & description ?~ "A time-varying expression that evaluates to an integer"
      & oneOf ?~ fmap Inline
          [ constantSchema
          , andObsSchema
          , orObsSchema
          , notObsSchema
          , choseSomethingSchema
          , valueGESchema
          , valueGTSchema
          , valueLTSchema
          , valueLESchema
          , valueEQSchema
          ]

instance ToSchema Bound where
  declareNamedSchema _ = do
    integerSchema <- declareSchemaRef $ Proxy @Integer
    let from = ("from", integerSchema)
    let to = ("to", integerSchema)
    pure $ NamedSchema (Just "Bound") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "An inclusive range of values for a choice."
      & required .~ fmap fst [from, to]
      & properties .~ [from, to]

instance ToSchema Action where
  declareNamedSchema _ = do
    partySchema <- declareSchemaRef $ Proxy @Party
    tokenSchema <- declareSchemaRef $ Proxy @Token
    valueSchema <- declareSchemaRef $ Proxy @(Value Observation)
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    boundSchema <- declareSchemaRef $ Proxy @[Bound]
    observationSchema <- declareSchemaRef $ Proxy @Observation
    let
      depositSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [party, deposits, of_token, into_account]
        & properties .~ [party, deposits, of_token, into_account]
        where
          party = ("party", partySchema)
          deposits = ("deposits", valueSchema)
          of_token = ("of_token", tokenSchema)
          into_account = ("into_account", partySchema)
      choiceSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [for_choice, choose_between]
        & properties .~ [for_choice, choose_between]
        where
          for_choice = ("for_choice", choiceIdSchema)
          choose_between = ("choose_between", boundSchema)
      notifySchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [notify_if]
        & properties .~ [notify_if]
        where
          notify_if = ("notify_if", observationSchema)
    pure $ NamedSchema (Just "Action") $ mempty
      & description ?~ "A contract which becomes active when an action occurs."
      & oneOf ?~ fmap Inline [depositSchema, choiceSchema, notifySchema]


instance ToSchema a => ToSchema (Case a) where
  declareNamedSchema _ = do
    actionSchema <- declareSchemaRef $ Proxy @Action
    contractSchema <- declareSchemaRef $ Proxy @a
    stringSchema <- declareSchemaRef $ Proxy @String
    let
      caseSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [case_, then_]
        & properties .~ [case_, then_]
        where
          case_ = ("case", actionSchema)
          then_ = ("then", contractSchema)
      merkleizedCaseSchema = mempty
        & type_ ?~ OpenApiObject
        & required .~ fmap fst [case_, merkleized_then]
        & properties .~ [case_, merkleized_then]
        where
          case_ = ("case", actionSchema)
          merkleized_then = ("merkleized_then", stringSchema)
    pure $ NamedSchema (Just "Case") $ mempty
      & description ?~ "A contract which becomes active when an action occurs."
      & oneOf ?~ fmap Inline [caseSchema, merkleizedCaseSchema]


instance ToSchema Contract where
  declareNamedSchema _ = do
    accountIdSchema <- declareSchemaRef $ Proxy @AccountId
    payeeSchema <- declareSchemaRef $ Proxy @Payee
    tokenSchema <- declareSchemaRef $ Proxy @Token
    valueSchema <- declareSchemaRef $ Proxy @(Value Observation)
    observationSchema <- declareSchemaRef $ Proxy @Observation
    contractSchema <- declareSchemaRef $ Proxy @Contract
    casesSchema <- declareSchemaRef $ Proxy @[Case Contract]
    timeoutSchema <- declareSchemaRef $ Proxy @Integer
    stringSchema <- declareSchemaRef $ Proxy @String
    let
      closeSchema = mempty
        & type_ ?~ OpenApiString
        & description ?~ "No more payments will be sent and the balance of the contract is 0."
        & enum_ ?~ ["close"]
      paySchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "A payment will be sent from an account to a payee."
        & required .~ fmap fst [from_account, to, token, pay, then_]
        & properties .~ [from_account, to, token, pay, then_]
        where
          from_account = ("from_account", accountIdSchema)
          to = ("to", payeeSchema)
          token = ("token", tokenSchema)
          pay = ("pay", valueSchema)
          then_ = ("then", contractSchema)
      ifSchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "If an observation is true, the first contract applies, otherwise the second contract applies."
        & required .~ fmap fst [if_, then_, else_]
        & properties .~ [if_, then_, else_]
        where
          if_ = ("if", observationSchema)
          then_ = ("then", contractSchema)
          else_ = ("else", contractSchema)
      whenSchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Wait for an action to be performed and apply the matching contract when it does. Apply the timeout contract if no actions have been performed in the timeout period."
        & required .~ fmap fst [when, timeout, timeout_continuation]
        & properties .~ [when, timeout, timeout_continuation]
        where
          when = ("when", casesSchema)
          timeout = ("timeout", timeoutSchema)
          timeout_continuation = ("timeout_continuation", contractSchema)
      letSchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Bind a value to a name within the scope of a sub-contract."
        & required .~ fmap fst [let_, be, then_]
        & properties .~ [let_, be, then_]
        where
          let_ = ("let", stringSchema)
          be = ("be", valueSchema)
          then_ = ("then", contractSchema)
      assertSchema = mempty
        & type_ ?~ OpenApiObject
        & description ?~ "Check an observation and produce a warning if it is false."
        & required .~ fmap fst [assert, then_]
        & properties .~ [assert, then_]
        where
          assert = ("assert", observationSchema)
          then_ = ("then", contractSchema)
    pure $ NamedSchema (Just "Contract") $ mempty
      & description ?~ "Contract terms specified in Marlowe"
      & oneOf ?~ fmap Inline [closeSchema, paySchema, ifSchema, whenSchema, letSchema, assertSchema]

instance ToSchema State where
  declareNamedSchema _ = do
    accountsSchema <- declareSchemaRef $ Proxy @[((AccountId, Token), Integer)]
    choicesSchema <- declareSchemaRef $ Proxy @[(ChoiceId, Integer)]
    boundValuesSchema <- declareSchemaRef $ Proxy @[(String, Integer)]
    integerSchema <- declareSchemaRef $ Proxy @Integer
    let accounts = ("accounts", accountsSchema)
    let choices = ("choices", choicesSchema)
    let boundValues = ("boundValues", boundValuesSchema)
    let minTime = ("minTime", integerSchema)
    pure $ NamedSchema (Just "MarloweState") $ mempty
      & type_ ?~ OpenApiObject
      & description ?~ "The on-chain state of a Marlowe contract."
      & required .~ fmap fst [accounts, choices, boundValues, minTime]
      & properties .~ [accounts, choices, boundValues, minTime]

instance ToSchema Input where
  declareNamedSchema _ = do
    contractSchema <- declareSchemaRef $ Proxy @Contract
    partySchema <- declareSchemaRef $ Proxy @Party
    tokenSchema <- declareSchemaRef $ Proxy @Token
    integerSchema <- declareSchemaRef $ Proxy @Integer
    stringSchema <- declareSchemaRef $ Proxy @String
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    let
      depositProperties, choiceProperties, merkleProperties :: [(Text, Referenced Schema)]
      depositProperties =
        [ ("input_from_party", partySchema)
        , ("that_deposits", integerSchema)
        , ("of_token", tokenSchema)
        , ("into_account", partySchema)
        ]
      choiceProperties =
        [ ("input_that_chooses_num", integerSchema)
        , ("for_choice_id", choiceIdSchema)
        ]
      merkleProperties =
        [ ("merkleized_continuation", contractSchema)
        , ("continuation_hash", stringSchema)
        ]
      objInputSchema props desc merkle = Inline $ mempty @Schema
        & type_ ?~ OpenApiObject
        & description ?~ (desc <> if merkle then " and provide the continuation of the contract" else "")
        & required .~ (fst <$> allProps)
        & properties .~ fromList allProps
        where
          allProps = props <> if merkle then merkleProperties else []
      depositSchema = objInputSchema depositProperties "Deposit funds into an account in a contract"
      choiceSchema = objInputSchema choiceProperties "Make a choice in a contract"
      notifySchema True = objInputSchema [] "Notify a contract to check a condition" True
      notifySchema False = Inline $ mempty
        & type_ ?~ OpenApiString
        & description ?~ "Notify a contract to check a condition"
        & enum_ ?~ ["input_notify"]
    pure $ NamedSchema (Just "Input") $ mempty
      & description ?~ "An input to a Marlowe transaction"
      & oneOf ?~ ([notifySchema, choiceSchema, depositSchema] <*> [True, False])
