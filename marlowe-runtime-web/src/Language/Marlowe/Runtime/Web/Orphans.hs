{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Orphans (

) where

import Control.Lens ((&), (.~), (?~))
import Data.Data (Typeable)
import Data.OpenApi (
  HasDescription (description),
  HasEnum (enum_),
  HasFormat (format),
  HasOneOf (oneOf),
  HasProperties (properties),
  HasRequired (required),
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiItems (OpenApiItemsObject),
  OpenApiType (..),
  Referenced (Inline),
  Schema,
  ToParamSchema (toParamSchema),
  ToSchema (..),
  declareSchemaRef,
  sketchSchema,
 )
import Data.OpenApi.Lens (HasItems (..))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Exts (IsList (fromList))
import qualified Language.Marlowe.Analysis.Safety.Types as V1
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Core.V1.Semantics.Types (
  AccountId,
  Action,
  Bound,
  Case,
  ChoiceId,
  Contract,
  Input,
  Observation,
  Party,
  Payee,
  State,
  Token,
  Value,
 )
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Object.Types (Label, LabelledObject, ObjectBundle)
import qualified Language.Marlowe.Object.Types as Object
import Numeric.Natural (Natural)
import Pipes (X)
import qualified Pipes
import qualified PlutusLedgerApi.V2 as P

data Address

instance ToSchema Address where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "Address") $
        mempty
          & type_ ?~ OpenApiString
          & description ?~ "A cardano address"

instance ToSchema ChoiceId where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    partySchema <- declareSchemaRef $ Proxy @Party
    let choice_name = ("choice_name", stringSchema)
    let choice_owner = ("choice_owner", partySchema)
    pure $
      NamedSchema (Just "ChoiceId") $
        mempty
          & type_ ?~ OpenApiObject
          & description ?~ "Refers to a party by role name."
          & required .~ fmap fst [choice_name, choice_owner]
          & properties .~ [choice_name, choice_owner]

instance ToSchema Party where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    addressSchema <- declareSchemaRef $ Proxy @Address
    let rolePartySchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Refers to a party by role name."
            & required .~ fmap fst [role_token]
            & properties .~ [role_token]
          where
            role_token = ("role_token", stringSchema)
        addressPartySchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Refers to a party by Cardano address."
            & required .~ fmap fst [address]
            & properties .~ [address]
          where
            address = ("address", addressSchema)
    pure $
      NamedSchema (Just "Party") $
        mempty
          & description ?~ "A participant in a contract"
          & oneOf ?~ fmap Inline [rolePartySchema, addressPartySchema]

instance ToSchema Payee where
  declareNamedSchema _ = do
    partySchema <- declareSchemaRef $ Proxy @Party
    let accountPayeeSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Pays funds into a party's account in the contract."
            & required .~ fmap fst [account]
            & properties .~ [account]
          where
            account = ("account", partySchema)
        partyPayeeSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Pays funds to a party."
            & required .~ fmap fst [party]
            & properties .~ [party]
          where
            party = ("party", partySchema)
    pure $
      NamedSchema (Just "Payee") $
        mempty
          & description ?~ "A recipient of a payment"
          & oneOf ?~ fmap Inline [accountPayeeSchema, partyPayeeSchema]

instance ToSchema Token where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    let currency_symbol = ("currency_symbol", stringSchema)
    let token_name = ("token_name", stringSchema)
    pure $
      NamedSchema (Just "Token") $
        mempty
          & type_ ?~ OpenApiObject
          & description ?~ "A token with a currency symbol (minting policy ID) and token name."
          & required .~ fmap fst [currency_symbol, token_name]
          & properties .~ [currency_symbol, token_name]

instance (ToSchema a) => ToSchema (Value a) where
  declareNamedSchema _ = do
    accountIdSchema <- declareSchemaRef $ Proxy @AccountId
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    tokenSchema <- declareSchemaRef $ Proxy @Token
    valueSchema <- declareSchemaRef $ Proxy @(Value a)
    observationSchema <- declareSchemaRef $ Proxy @a
    stringSchema <- declareSchemaRef $ Proxy @String
    let timeIntervalSchema =
          mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ ["time_interval_start", "time_interval_end"]
        constantSchema =
          mempty
            & type_ ?~ OpenApiInteger
            & format ?~ "int64"
        availableMoneySchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [amount_of_token, in_account]
            & properties .~ [amount_of_token, in_account]
          where
            amount_of_token = ("amount_of_token", tokenSchema)
            in_account = ("in_account", accountIdSchema)
        negValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [negate_]
            & properties .~ [negate_]
          where
            negate_ = ("negate", valueSchema)
        addValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("add", valueSchema)
            val2 = ("and", valueSchema)
        subValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("value", valueSchema)
            val2 = ("minus", valueSchema)
        mulValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("multiply", valueSchema)
            val2 = ("times", valueSchema)
        divValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("divide", valueSchema)
            val2 = ("by", valueSchema)
        choiceValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value_of_choice]
            & properties .~ [value_of_choice]
          where
            value_of_choice = ("value_of_choice", choiceIdSchema)
        useValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [use_value]
            & properties .~ [use_value]
          where
            use_value = ("use_value", stringSchema)
        condSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [if_, then_, else_]
            & properties .~ [if_, then_, else_]
          where
            if_ = ("if", observationSchema)
            then_ = ("then", valueSchema)
            else_ = ("else", valueSchema)
    pure $
      NamedSchema (Just "Value") $
        mempty
          & description ?~ "A time-varying expression that evaluates to a boolean"
          & oneOf
            ?~ fmap
              Inline
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
    let constantSchema = mempty & type_ ?~ OpenApiBoolean
        andObsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [both, and_]
            & properties .~ [both, and_]
          where
            both = ("both", observationSchema)
            and_ = ("and", observationSchema)
        orObsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [either_, or_]
            & properties .~ [either_, or_]
          where
            either_ = ("either", observationSchema)
            or_ = ("or", observationSchema)
        notObsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [p]
            & properties .~ [p]
          where
            p = ("not", observationSchema)
        choseSomethingSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [chose_something_for]
            & properties .~ [chose_something_for]
          where
            chose_something_for = ("chose_something_for", choiceIdSchema)
        valueGESchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, ge_than]
            & properties .~ [value, ge_than]
          where
            value = ("value", valueSchema)
            ge_than = ("ge_than", valueSchema)
        valueGTSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, gt]
            & properties .~ [value, gt]
          where
            value = ("value", valueSchema)
            gt = ("gt", valueSchema)
        valueLESchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, le_than]
            & properties .~ [value, le_than]
          where
            value = ("value", valueSchema)
            le_than = ("le_than", valueSchema)
        valueLTSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, lt]
            & properties .~ [value, lt]
          where
            value = ("value", valueSchema)
            lt = ("lt", valueSchema)
        valueEQSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, equal_to]
            & properties .~ [value, equal_to]
          where
            value = ("value", valueSchema)
            equal_to = ("equal_to", valueSchema)
    pure $
      NamedSchema (Just "Observation") $
        mempty
          & description ?~ "A time-varying expression that evaluates to an integer"
          & oneOf
            ?~ fmap
              Inline
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
    pure $
      NamedSchema (Just "Bound") $
        mempty
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
    let depositSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [party, deposits, of_token, into_account]
            & properties .~ [party, deposits, of_token, into_account]
          where
            party = ("party", partySchema)
            deposits = ("deposits", valueSchema)
            of_token = ("of_token", tokenSchema)
            into_account = ("into_account", partySchema)
        choiceSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [for_choice, choose_between]
            & properties .~ [for_choice, choose_between]
          where
            for_choice = ("for_choice", choiceIdSchema)
            choose_between = ("choose_between", boundSchema)
        notifySchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [notify_if]
            & properties .~ [notify_if]
          where
            notify_if = ("notify_if", observationSchema)
    pure $
      NamedSchema (Just "Action") $
        mempty
          & description ?~ "A contract which becomes active when an action occurs."
          & oneOf ?~ fmap Inline [depositSchema, choiceSchema, notifySchema]

instance (ToSchema a) => ToSchema (Case a) where
  declareNamedSchema _ = do
    actionSchema <- declareSchemaRef $ Proxy @Action
    contractSchema <- declareSchemaRef $ Proxy @a
    stringSchema <- declareSchemaRef $ Proxy @String
    let caseSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [case_, then_]
            & properties .~ [case_, then_]
          where
            case_ = ("case", actionSchema)
            then_ = ("then", contractSchema)
        merkleizedCaseSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [case_, merkleized_then]
            & properties .~ [case_, merkleized_then]
          where
            case_ = ("case", actionSchema)
            merkleized_then = ("merkleized_then", stringSchema)
    pure $
      NamedSchema (Just "Case") $
        mempty
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
    let closeSchema =
          mempty
            & type_ ?~ OpenApiString
            & description ?~ "No more payments will be sent and the balance of the contract is 0."
            & enum_ ?~ ["close"]
        paySchema =
          mempty
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
        ifSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "If an observation is true, the first contract applies, otherwise the second contract applies."
            & required .~ fmap fst [if_, then_, else_]
            & properties .~ [if_, then_, else_]
          where
            if_ = ("if", observationSchema)
            then_ = ("then", contractSchema)
            else_ = ("else", contractSchema)
        whenSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description
              ?~ "Wait for an action to be performed and apply the matching contract when it does. Apply the timeout contract if no actions have been performed in the timeout period."
            & required .~ fmap fst [when, timeout, timeout_continuation]
            & properties .~ [when, timeout, timeout_continuation]
          where
            when = ("when", casesSchema)
            timeout = ("timeout", timeoutSchema)
            timeout_continuation = ("timeout_continuation", contractSchema)
        letSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Bind a value to a name within the scope of a sub-contract."
            & required .~ fmap fst [let_, be, then_]
            & properties .~ [let_, be, then_]
          where
            let_ = ("let", stringSchema)
            be = ("be", valueSchema)
            then_ = ("then", contractSchema)
        assertSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Check an observation and produce a warning if it is false."
            & required .~ fmap fst [assert, then_]
            & properties .~ [assert, then_]
          where
            assert = ("assert", observationSchema)
            then_ = ("then", contractSchema)
    pure $
      NamedSchema (Just "Contract") $
        mempty
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
    pure $
      NamedSchema (Just "MarloweState") $
        mempty
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
    let depositProperties, choiceProperties, merkleProperties :: [(Text, Referenced Schema)]
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
        objInputSchema props desc merkle =
          Inline $
            mempty @Schema
              & type_ ?~ OpenApiObject
              & description ?~ (desc <> if merkle then " and provide the continuation of the contract" else "")
              & required .~ (fst <$> allProps)
              & properties .~ fromList allProps
          where
            allProps = props <> if merkle then merkleProperties else []
        depositSchema = objInputSchema depositProperties "Deposit funds into an account in a contract"
        choiceSchema = objInputSchema choiceProperties "Make a choice in a contract"
        notifySchema True = objInputSchema [] "Notify a contract to check a condition" True
        notifySchema False =
          Inline $
            mempty
              & type_ ?~ OpenApiString
              & description ?~ "Notify a contract to check a condition"
              & enum_ ?~ ["input_notify"]
    pure $
      NamedSchema (Just "Input") $
        mempty
          & description ?~ "An input to a Marlowe transaction"
          & oneOf ?~ ([notifySchema, choiceSchema, depositSchema] <*> [True, False])

instance ToSchema V1.SafetyError where
  declareNamedSchema _ =
    do
      stringSchema <- declareSchemaRef $ Proxy @String
      boolSchema <- declareSchemaRef $ Proxy @Bool
      naturalSchema <- declareSchemaRef $ Proxy @Natural
      warningSchema <- ("warning",) <$> declareSchemaRef (Proxy @V1.TransactionWarning)
      tokenSchema <- ("token",) <$> declareSchemaRef (Proxy @Token)
      accountIdSchema <- ("account-id",) <$> declareSchemaRef (Proxy @AccountId)
      choiceIdSchema <- ("choice-id",) <$> declareSchemaRef (Proxy @ChoiceId)
      costSchema <- ("cost",) <$> declareSchemaRef (Proxy @P.ExBudget)
      transactionSchema <- ("transaction",) <$> declareSchemaRef (Proxy @(V1.Transaction ()))
      addressSchema <- ("address",) <$> declareSchemaRef (Proxy @P.Address)
      let errorSchema = ("error", stringSchema)
          detailSchema = ("detail", stringSchema)
          fatalSchema = ("fatal", boolSchema)
          currencySymbolSchema = ("currency-symbol", stringSchema)
          tokenNameSchema = ("token-name", stringSchema)
          roleNameSchema = ("role-name", stringSchema)
          valueIdSchema = ("value-id", stringSchema)
          bytesSchema = ("bytes", naturalSchema)
          messageSchema = ("message", stringSchema)
          hashSchema = ("hash", stringSchema)
      pure $
        NamedSchema (Just "SafetyError") $
          mempty
            & description ?~ "Information about the safety of a Marlowe contract and its state."
            & required .~ fmap fst [errorSchema, detailSchema, fatalSchema]
            & properties
              .~ [ errorSchema
                 , detailSchema
                 , fatalSchema
                 , roleNameSchema
                 , currencySymbolSchema
                 , tokenNameSchema
                 , tokenSchema
                 , accountIdSchema
                 , choiceIdSchema
                 , valueIdSchema
                 , bytesSchema
                 , transactionSchema
                 , costSchema
                 , messageSchema
                 , warningSchema
                 , hashSchema
                 , addressSchema
                 ]

instance ToSchema V1.ValueId where
  declareNamedSchema _ = pure . NamedSchema (Just "ValueId") $ sketchSchema $ V1.ValueId "x"

instance ToSchema V1.Payment where
  declareNamedSchema _ =
    do
      accountIdSchema <- ("payment_from",) <$> declareSchemaRef (Proxy @V1.AccountId)
      payeeSchema <- ("to",) <$> declareSchemaRef (Proxy @V1.Payee)
      tokenSchema <- ("token",) <$> declareSchemaRef (Proxy @V1.Token)
      amountSchema <- ("amount",) <$> declareSchemaRef (Proxy @Integer)
      pure $
        NamedSchema (Just "Payment") $
          mempty
            & description ?~ "A Marlowe payment."
            & required .~ fmap fst [accountIdSchema, payeeSchema, tokenSchema, amountSchema]
            & properties .~ [accountIdSchema, payeeSchema, tokenSchema, amountSchema]

instance ToSchema (V1.Transaction ()) where
  declareNamedSchema _ =
    do
      stateSchema <- ("state",) <$> declareSchemaRef (Proxy @State)
      contractSchema <- ("contract",) <$> declareSchemaRef (Proxy @Contract)
      inputSchema <- ("input",) <$> declareSchemaRef (Proxy @V1.TransactionInput)
      outputSchema <- ("output",) <$> declareSchemaRef (Proxy @V1.TransactionOutput)
      pure $
        NamedSchema (Just "Transaction") $
          mempty
            & description ?~ "Information about a Marlowe transaction."
            & required .~ fmap fst [stateSchema, contractSchema, inputSchema, outputSchema]
            & properties .~ [stateSchema, contractSchema, inputSchema, outputSchema]

instance ToSchema V1.TransactionInput where
  declareNamedSchema _ =
    do
      integerSchema <- declareSchemaRef $ Proxy @Integer
      let intervalSchema' =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "Time interval."
              & required .~ fmap fst [lower, upper]
              & properties .~ [lower, upper]
            where
              lower = ("from", integerSchema)
              upper = ("to", integerSchema)
          intervalSchema = ("tx_interval", Inline intervalSchema')
      inputsSchema <- ("tx_inputs",) <$> declareSchemaRef (Proxy @[V1.Input])
      pure $
        NamedSchema (Just "TransactionInput") $
          mempty
            & description ?~ "Marlowe transaction input."
            & required .~ fmap fst [intervalSchema, inputsSchema]
            & properties .~ [intervalSchema, inputsSchema]
            & type_ ?~ OpenApiObject

instance ToSchema V1.TransactionOutput where
  declareNamedSchema _ =
    do
      txErrorSchema <- declareSchemaRef $ Proxy @V1.TransactionError
      warningsSchema <- ("warnings",) <$> declareSchemaRef (Proxy @[V1.TransactionWarning])
      paymentsSchema <- ("payments",) <$> declareSchemaRef (Proxy @[V1.Payment])
      stateSchema <- ("state",) <$> declareSchemaRef (Proxy @V1.State)
      contractSchema <- ("contract",) <$> declareSchemaRef (Proxy @V1.Contract)
      let noErrorSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "Marlowe transaction output information."
              & required .~ fmap fst [warningsSchema, paymentsSchema, stateSchema, contractSchema]
              & properties .~ [warningsSchema, paymentsSchema, stateSchema, contractSchema]
          errorSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "Marlowe transaction error."
              & required .~ fmap fst [message]
              & properties .~ [message]
            where
              message = ("transaction_error", txErrorSchema)
      pure $
        NamedSchema (Just "TransactionOutput") $
          mempty
            & description ?~ "Marlowe transaction output."
            & oneOf ?~ fmap Inline [noErrorSchema, errorSchema]

instance ToSchema V1.TransactionError where
  declareNamedSchema _ =
    do
      ieSchema <- declareSchemaRef $ Proxy @V1.IntervalError
      let ambiguousIntervalSchema =
            mempty
              & type_ ?~ OpenApiString
              & description ?~ "Ambiguous time interval."
              & enum_ ?~ ["TEAmbiguousTimeIntervalError"]
          applyNoMatchSchema =
            mempty
              & type_ ?~ OpenApiString
              & description ?~ "No match on applying input."
              & enum_ ?~ ["TEApplyNoMatchError"]
          uselessTransactionSchema =
            mempty
              & type_ ?~ OpenApiString
              & description ?~ "A useless application of input."
              & enum_ ?~ ["TEUselessTransaction"]
          intervalErrorSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "An invalid time interval."
              & required .~ fmap fst [message, interval]
              & properties .~ [message, interval]
            where
              message = ("error", Inline $ mempty & type_ ?~ OpenApiString & enum_ ?~ ["TEIntervalError"])
              interval = ("context", ieSchema)
          hashMismatchSchema =
            mempty
              & type_ ?~ OpenApiString
              & description ?~ "A mismatch in the continuation hash."
              & enum_ ?~ ["TEHashMismatch"]
      pure $
        NamedSchema (Just "TransactionError") $
          mempty
            & description ?~ "A Marlowe transaction error."
            & oneOf
              ?~ fmap
                Inline
                [ambiguousIntervalSchema, applyNoMatchSchema, intervalErrorSchema, uselessTransactionSchema, hashMismatchSchema]

instance ToSchema V1.IntervalError where
  declareNamedSchema _ =
    do
      integerSchema <- declareSchemaRef $ Proxy @Integer
      let from = ("from", integerSchema)
          to = ("to", integerSchema)
          minTime = ("minTime", integerSchema)
          invalidIntervalSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "Invalid Marlowe transaction interval."
              & required .~ fmap fst [invalid]
              & properties .~ [invalid]
            where
              invalid =
                ( "invalidInterval"
                , Inline $
                    mempty
                      & type_ ?~ OpenApiObject
                      & required .~ fmap fst [from, to]
                      & properties .~ [from, to]
                )
          intervalInPastSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "Marlowe transaction interval in past."
              & required .~ fmap fst [past]
              & properties .~ [past]
            where
              past =
                ( "intervalInPastError"
                , Inline $
                    mempty
                      & type_ ?~ OpenApiObject
                      & required .~ fmap fst [minTime, from, to]
                      & properties .~ [minTime, from, to]
                )
      pure $
        NamedSchema (Just "IntervalError") $
          mempty
            & description ?~ "A Marlowe transaction interval error."
            & oneOf ?~ fmap Inline [invalidIntervalSchema, intervalInPastSchema]

instance ToSchema V1.TransactionWarning where
  declareNamedSchema _ =
    do
      integerSchema <- declareSchemaRef $ Proxy @Integer
      partySchema <- declareSchemaRef $ Proxy @V1.Party
      tokenSchema <- declareSchemaRef $ Proxy @V1.Token
      accountIdSchema <- declareSchemaRef $ Proxy @V1.AccountId
      payeeSchema <- declareSchemaRef $ Proxy @V1.Payee
      valueIdSchema <- declareSchemaRef $ Proxy @String
      let nonPositiveDepositSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A warning for a non-positive deposit."
              & required .~ fmap fst [party, amount, tok, accId]
              & properties .~ [party, amount, tok, accId]
            where
              party = ("party", partySchema)
              amount = ("asked_to_deposit", integerSchema)
              tok = ("of_token", tokenSchema)
              accId = ("in_account", accountIdSchema)
          nonPositivePaySchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A warning for a non-positive payment."
              & required .~ fmap fst [accId, amount, tok, payee]
              & properties .~ [accId, amount, tok, payee]
            where
              accId = ("account", accountIdSchema)
              amount = ("asked_to_pay", integerSchema)
              tok = ("of_token", tokenSchema)
              payee = ("to_payee", payeeSchema)
          partialPaySchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A warning for partial payment."
              & required .~ fmap fst [accId, expected, tok, payee, paid]
              & properties .~ [accId, expected, tok, payee, paid]
            where
              accId = ("account", accountIdSchema)
              expected = ("asked_to_pay", integerSchema)
              tok = ("of_token", tokenSchema)
              payee = ("to_payee", payeeSchema)
              paid = ("but_only_paid", integerSchema)
          shadowingSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A variable-name shadowing warning."
              & required .~ fmap fst [valId, oldVal, newVal]
              & properties .~ [valId, oldVal, newVal]
            where
              valId = ("value_id", valueIdSchema)
              oldVal = ("had_value", integerSchema)
              newVal = ("is_now_assigned", integerSchema)
          assertionSchema =
            mempty
              & type_ ?~ OpenApiString
              & description ?~ "A semantics assertion failed."
              & enum_ ?~ ["assertion_failed"]
      pure $
        NamedSchema (Just "TransactionWarning") $
          mempty
            & description ?~ "A transaction semantics warning."
            & oneOf
              ?~ fmap Inline [nonPositiveDepositSchema, nonPositivePaySchema, partialPaySchema, shadowingSchema, assertionSchema]

instance ToSchema P.Address where
  declareNamedSchema _ =
    do
      addressCredentialSchema <- ("addressCredential",) <$> declareSchemaRef (Proxy @P.Credential)
      stakingCredentialSchema <- ("addressStakingCredential",) <$> declareSchemaRef (Proxy @(Maybe P.StakingCredential))
      pure $
        NamedSchema (Just "Plutus.Address") $
          mempty
            & description ?~ "A Plutus address."
            & required .~ fmap fst [addressCredentialSchema]
            & properties .~ [addressCredentialSchema, stakingCredentialSchema]

instance ToSchema P.Credential where
  declareNamedSchema _ =
    do
      stringSchema <- declareSchemaRef $ Proxy @String
      let pubKeyCredentialSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A Plutus public key credential."
              & required .~ fmap fst [item]
              & properties .~ [item]
            where
              item = ("pubKeyCredential", stringSchema)
          scriptCredentialSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A Plutus script credential."
              & required .~ fmap fst [item]
              & properties .~ [item]
            where
              item = ("scriptCredential", stringSchema)
      pure $
        NamedSchema (Just "Plutus.Credential") $
          mempty
            & description ?~ "A Plutus credential."
            & oneOf ?~ fmap Inline [pubKeyCredentialSchema, scriptCredentialSchema]

instance ToSchema P.StakingCredential where
  declareNamedSchema _ =
    do
      credentialSchema <- declareSchemaRef $ Proxy @P.Credential
      integerTripletSchema <- declareSchemaRef $ Proxy @(Integer, Integer, Integer)
      let stakingHashSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A Plutus staking hash."
              & required .~ fmap fst [item]
              & properties .~ [item]
            where
              item = ("stakingHash", credentialSchema)
          stakingPtrSchema =
            mempty
              & type_ ?~ OpenApiObject
              & description ?~ "A Plutus staking pointer."
              & required .~ fmap fst [item]
              & properties .~ [item]
            where
              item = ("stakingHash", integerTripletSchema)
      pure $
        NamedSchema (Just "Plutus.StakingCredential") $
          mempty
            & description ?~ "A Plutus staking credential."
            & oneOf ?~ fmap Inline [stakingHashSchema, stakingPtrSchema]

instance ToSchema P.ExBudget where
  declareNamedSchema _ = pure . NamedSchema (Just "ExBudget") $ sketchSchema $ P.ExBudget 10 10

instance ToParamSchema Label where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & description ?~ "An arbitrary text identifier for an object in a Marlowe object bundle."

instance ToSchema Label where
  declareNamedSchema p = pure $ NamedSchema (Just "Label") $ toParamSchema p

instance (Typeable m, ToSchema a) => ToSchema (Pipes.Proxy X () () a m ()) where
  declareNamedSchema _ = declareNamedSchema $ Proxy @a

instance ToSchema ObjectBundle where
  declareNamedSchema _ = do
    ref <- declareSchemaRef $ Proxy @LabelledObject
    pure $
      NamedSchema (Just "ObjectBundle") $
        mempty
          & type_ ?~ OpenApiArray
          & items ?~ OpenApiItemsObject ref
          & description ?~ "A bundle of labelled Marlowe objects in define-before-use order."

instance ToSchema LabelledObject where
  declareNamedSchema _ = do
    labelRef <- declareSchemaRef $ Proxy @Label
    valueRef <- declareSchemaRef $ Proxy @Object.Value
    observationRef <- declareSchemaRef $ Proxy @Object.Observation
    contractRef <- declareSchemaRef $ Proxy @Object.Contract
    partyRef <- declareSchemaRef $ Proxy @Object.Party
    tokenRef <- declareSchemaRef $ Proxy @Object.Token
    actionRef <- declareSchemaRef $ Proxy @Object.Action
    let typeSchema =
          Inline $
            mempty
              & type_ ?~ OpenApiString
              & enum_ ?~ ["value", "observation", "contract", "party", "token", "action"]
    let valueSchema =
          Inline $
            mempty
              & oneOf
                ?~ [ valueRef
                   , observationRef
                   , contractRef
                   , partyRef
                   , tokenRef
                   , actionRef
                   ]
    let props = [("label", labelRef), ("type", typeSchema), ("value", valueSchema)]
    pure $
      NamedSchema (Just "LabelledObject") $
        mempty
          & type_ ?~ OpenApiObject
          & description ?~ "A bundle of labelled Marlowe objects in define-before-use order."
          & required .~ fmap fst props
          & properties .~ fromList props

instance ToSchema Object.Value where
  declareNamedSchema _ = do
    accountIdSchema <- declareSchemaRef $ Proxy @Object.Party
    choiceIdSchema <- declareSchemaRef $ Proxy @Object.ChoiceId
    tokenSchema <- declareSchemaRef $ Proxy @Object.Token
    valueSchema <- declareSchemaRef $ Proxy @Object.Value
    observationSchema <- declareSchemaRef $ Proxy @Object.Observation
    stringSchema <- declareSchemaRef $ Proxy @String
    labelSchema <- declareSchemaRef $ Proxy @Label
    let timeIntervalSchema =
          mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ ["time_interval_start", "time_interval_end"]
        constantSchema =
          mempty
            & type_ ?~ OpenApiInteger
            & format ?~ "int64"
        availableMoneySchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [amount_of_token, in_account]
            & properties .~ [amount_of_token, in_account]
          where
            amount_of_token = ("amount_of_token", tokenSchema)
            in_account = ("in_account", accountIdSchema)
        negValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [negate_]
            & properties .~ [negate_]
          where
            negate_ = ("negate", valueSchema)
        addValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("add", valueSchema)
            val2 = ("and", valueSchema)
        subValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("value", valueSchema)
            val2 = ("minus", valueSchema)
        mulValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("multiply", valueSchema)
            val2 = ("times", valueSchema)
        divValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [val1, val2]
            & properties .~ [val1, val2]
          where
            val1 = ("divide", valueSchema)
            val2 = ("by", valueSchema)
        choiceValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value_of_choice]
            & properties .~ [value_of_choice]
          where
            value_of_choice = ("value_of_choice", choiceIdSchema)
        useValueSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [use_value]
            & properties .~ [use_value]
          where
            use_value = ("use_value", stringSchema)
        condSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [if_, then_, else_]
            & properties .~ [if_, then_, else_]
          where
            if_ = ("if", observationSchema)
            then_ = ("then", valueSchema)
            else_ = ("else", valueSchema)
        refSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [ref]
            & properties .~ [ref]
          where
            ref = ("ref", labelSchema)
    pure $
      NamedSchema (Just "ValueObject") $
        mempty
          & description ?~ "A time-varying expression that evaluates to a boolean"
          & oneOf
            ?~ fmap
              Inline
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
              , refSchema
              ]

instance ToSchema Object.Observation where
  declareNamedSchema _ = do
    choiceIdSchema <- declareSchemaRef $ Proxy @Object.ChoiceId
    valueSchema <- declareSchemaRef $ Proxy @Object.Value
    observationSchema <- declareSchemaRef $ Proxy @Object.Observation
    labelSchema <- declareSchemaRef $ Proxy @Label
    let constantSchema = mempty & type_ ?~ OpenApiBoolean
        andObsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [both, and_]
            & properties .~ [both, and_]
          where
            both = ("both", observationSchema)
            and_ = ("and", observationSchema)
        orObsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [either_, or_]
            & properties .~ [either_, or_]
          where
            either_ = ("either", observationSchema)
            or_ = ("or", observationSchema)
        notObsSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [p]
            & properties .~ [p]
          where
            p = ("not", observationSchema)
        choseSomethingSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [chose_something_for]
            & properties .~ [chose_something_for]
          where
            chose_something_for = ("chose_something_for", choiceIdSchema)
        valueGESchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, ge_than]
            & properties .~ [value, ge_than]
          where
            value = ("value", valueSchema)
            ge_than = ("ge_than", valueSchema)
        valueGTSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, gt]
            & properties .~ [value, gt]
          where
            value = ("value", valueSchema)
            gt = ("gt", valueSchema)
        valueLESchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, le_than]
            & properties .~ [value, le_than]
          where
            value = ("value", valueSchema)
            le_than = ("le_than", valueSchema)
        valueLTSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, lt]
            & properties .~ [value, lt]
          where
            value = ("value", valueSchema)
            lt = ("lt", valueSchema)
        valueEQSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [value, equal_to]
            & properties .~ [value, equal_to]
          where
            value = ("value", valueSchema)
            equal_to = ("equal_to", valueSchema)
        refSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [ref]
            & properties .~ [ref]
          where
            ref = ("ref", labelSchema)
    pure $
      NamedSchema (Just "ObservationObject") $
        mempty
          & description ?~ "A time-varying expression that evaluates to an integer"
          & oneOf
            ?~ fmap
              Inline
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
              , refSchema
              ]

instance ToSchema Object.Contract where
  declareNamedSchema _ = do
    accountIdSchema <- declareSchemaRef $ Proxy @Object.AccountId
    payeeSchema <- declareSchemaRef $ Proxy @Object.Payee
    tokenSchema <- declareSchemaRef $ Proxy @Object.Token
    valueSchema <- declareSchemaRef $ Proxy @Object.Value
    observationSchema <- declareSchemaRef $ Proxy @Object.Observation
    contractSchema <- declareSchemaRef $ Proxy @Object.Contract
    casesSchema <- declareSchemaRef $ Proxy @[Object.Case]
    timeoutSchema <- declareSchemaRef $ Proxy @Integer
    stringSchema <- declareSchemaRef $ Proxy @String
    labelSchema <- declareSchemaRef $ Proxy @Label
    let closeSchema =
          mempty
            & type_ ?~ OpenApiString
            & description ?~ "No more payments will be sent and the balance of the contract is 0."
            & enum_ ?~ ["close"]
        paySchema =
          mempty
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
        ifSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "If an observation is true, the first contract applies, otherwise the second contract applies."
            & required .~ fmap fst [if_, then_, else_]
            & properties .~ [if_, then_, else_]
          where
            if_ = ("if", observationSchema)
            then_ = ("then", contractSchema)
            else_ = ("else", contractSchema)
        whenSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description
              ?~ "Wait for an action to be performed and apply the matching contract when it does. Apply the timeout contract if no actions have been performed in the timeout period."
            & required .~ fmap fst [when, timeout, timeout_continuation]
            & properties .~ [when, timeout, timeout_continuation]
          where
            when = ("when", casesSchema)
            timeout = ("timeout", timeoutSchema)
            timeout_continuation = ("timeout_continuation", contractSchema)
        letSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Bind a value to a name within the scope of a sub-contract."
            & required .~ fmap fst [let_, be, then_]
            & properties .~ [let_, be, then_]
          where
            let_ = ("let", stringSchema)
            be = ("be", valueSchema)
            then_ = ("then", contractSchema)
        assertSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Check an observation and produce a warning if it is false."
            & required .~ fmap fst [assert, then_]
            & properties .~ [assert, then_]
          where
            assert = ("assert", observationSchema)
            then_ = ("then", contractSchema)
        refSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [ref]
            & properties .~ [ref]
          where
            ref = ("ref", labelSchema)
    pure $
      NamedSchema (Just "ContractObject") $
        mempty
          & description ?~ "Contract terms specified in Marlowe"
          & oneOf ?~ fmap Inline [closeSchema, paySchema, ifSchema, whenSchema, letSchema, assertSchema, refSchema]

instance ToSchema Object.ChoiceId where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    partySchema <- declareSchemaRef $ Proxy @Object.Party
    let choice_name = ("choice_name", stringSchema)
    let choice_owner = ("choice_owner", partySchema)
    pure $
      NamedSchema (Just "ChoiceIdObject") $
        mempty
          & type_ ?~ OpenApiObject
          & description ?~ "Refers to a party by role name."
          & required .~ fmap fst [choice_name, choice_owner]
          & properties .~ [choice_name, choice_owner]

instance ToSchema Object.Party where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    addressSchema <- declareSchemaRef $ Proxy @Address
    labelSchema <- declareSchemaRef $ Proxy @Label
    let rolePartySchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Refers to a party by role name."
            & required .~ fmap fst [role_token]
            & properties .~ [role_token]
          where
            role_token = ("role_token", stringSchema)
        addressPartySchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Refers to a party by Cardano address."
            & required .~ fmap fst [address]
            & properties .~ [address]
          where
            address = ("address", addressSchema)
        refSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [ref]
            & properties .~ [ref]
          where
            ref = ("ref", labelSchema)
    pure $
      NamedSchema (Just "PartyObject") $
        mempty
          & description ?~ "A participant in a contract"
          & oneOf ?~ fmap Inline [rolePartySchema, addressPartySchema, refSchema]

instance ToSchema Object.Payee where
  declareNamedSchema _ = do
    partySchema <- declareSchemaRef $ Proxy @Object.Party
    let accountPayeeSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Pays funds into a party's account in the contract."
            & required .~ fmap fst [account]
            & properties .~ [account]
          where
            account = ("account", partySchema)
        partyPayeeSchema =
          mempty
            & type_ ?~ OpenApiObject
            & description ?~ "Pays funds to a party."
            & required .~ fmap fst [party]
            & properties .~ [party]
          where
            party = ("party", partySchema)
    pure $
      NamedSchema (Just "PayeeObject") $
        mempty
          & description ?~ "A recipient of a payment"
          & oneOf ?~ fmap Inline [accountPayeeSchema, partyPayeeSchema]

instance ToSchema Object.Token where
  declareNamedSchema _ = do
    tokenSchema <- declareSchemaRef $ Proxy @Token
    labelSchema <- declareSchemaRef $ Proxy @Label
    let refSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [ref]
            & properties .~ [ref]
          where
            ref = ("ref", labelSchema)
    pure $
      NamedSchema (Just "TokenObject") $
        mempty
          & description ?~ "A token with a currency symbol (minting policy ID) and token name."
          & oneOf ?~ [tokenSchema, Inline refSchema]

instance ToSchema Object.Action where
  declareNamedSchema _ = do
    partySchema <- declareSchemaRef $ Proxy @Party
    tokenSchema <- declareSchemaRef $ Proxy @Token
    valueSchema <- declareSchemaRef $ Proxy @(Value Observation)
    choiceIdSchema <- declareSchemaRef $ Proxy @ChoiceId
    boundSchema <- declareSchemaRef $ Proxy @[Bound]
    observationSchema <- declareSchemaRef $ Proxy @Observation
    labelSchema <- declareSchemaRef $ Proxy @Label
    let depositSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [party, deposits, of_token, into_account]
            & properties .~ [party, deposits, of_token, into_account]
          where
            party = ("party", partySchema)
            deposits = ("deposits", valueSchema)
            of_token = ("of_token", tokenSchema)
            into_account = ("into_account", partySchema)
        choiceSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [for_choice, choose_between]
            & properties .~ [for_choice, choose_between]
          where
            for_choice = ("for_choice", choiceIdSchema)
            choose_between = ("choose_between", boundSchema)
        notifySchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [notify_if]
            & properties .~ [notify_if]
          where
            notify_if = ("notify_if", observationSchema)
        refSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [ref]
            & properties .~ [ref]
          where
            ref = ("ref", labelSchema)
    pure $
      NamedSchema (Just "ActionObject") $
        mempty
          & description ?~ "A contract which becomes active when an action occurs."
          & oneOf ?~ fmap Inline [depositSchema, choiceSchema, notifySchema, refSchema]

instance ToSchema Object.Case where
  declareNamedSchema _ = do
    actionSchema <- declareSchemaRef $ Proxy @Object.Action
    contractSchema <- declareSchemaRef $ Proxy @Object.Contract
    stringSchema <- declareSchemaRef $ Proxy @String
    let caseSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [case_, then_]
            & properties .~ [case_, then_]
          where
            case_ = ("case", actionSchema)
            then_ = ("then", contractSchema)
        merkleizedCaseSchema =
          mempty
            & type_ ?~ OpenApiObject
            & required .~ fmap fst [case_, merkleized_then]
            & properties .~ [case_, merkleized_then]
          where
            case_ = ("case", actionSchema)
            merkleized_then = ("merkleized_then", stringSchema)
    pure $
      NamedSchema (Just "CaseObject") $
        mempty
          & description ?~ "A contract which becomes active when an action occurs."
          & oneOf ?~ fmap Inline [caseSchema, merkleizedCaseSchema]
