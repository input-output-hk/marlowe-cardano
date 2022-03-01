module Examples.PureScript.ContractForDifferences
  ( contractTemplate
  , metaData
  , defaultTimeContent
  , extendedContract
  ) where

import Prelude

import Data.BigInt.Argonaut (fromInt)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Examples.Metadata as Metadata
import Marlowe.Extended
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Observation(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Marlowe.Extended.Metadata (ContractTemplate, MetaData)
import Marlowe.Semantics
  ( Bound(..)
  , ChoiceId(..)
  , Party(..)
  , Token(..)
  , ValueId(..)
  )
import Marlowe.Time (unsafeInstantFromInt)

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract }

metaData :: MetaData
metaData = Metadata.contractForDifferences

defaultTimeContent :: Map String Instant
defaultTimeContent =
  Map.fromFoldable
    [ "Party deposit deadline" /\ unsafeInstantFromInt 300000
    , "Counterparty deposit deadline" /\ unsafeInstantFromInt 600000
    , "First window beginning" /\ unsafeInstantFromInt 900000
    , "First window deadline" /\ unsafeInstantFromInt 1200000
    , "Second window beginning" /\ unsafeInstantFromInt 1500000
    , "Second window deadline" /\ unsafeInstantFromInt 1800000
    ]

ada :: Token
ada = Token "" ""

party :: Party
party = Role "Party"

counterparty :: Party
counterparty = Role "Counterparty"

oracle :: Party
oracle = Role "Oracle"

partyDeposit :: Value
partyDeposit = ConstantParam "Amount paid by party"

counterpartyDeposit :: Value
counterpartyDeposit = ConstantParam "Amount paid by counterparty"

priceBeginning :: ChoiceId
priceBeginning = ChoiceId "Price in first window" oracle

priceEnd :: ChoiceId
priceEnd = ChoiceId "Price in second window" oracle

decreaseInPrice :: ValueId
decreaseInPrice = ValueId "Decrease in price"

increaseInPrice :: ValueId
increaseInPrice = ValueId "Increase in price"

initialDeposit :: Party -> Value -> Timeout -> Contract -> Contract -> Contract
initialDeposit by deposit timeout timeoutContinuation continuation =
  When [ Case (Deposit by by ada deposit) continuation ]
    timeout
    timeoutContinuation

oracleInput :: ChoiceId -> Timeout -> Contract -> Contract -> Contract
oracleInput choiceId timeout timeoutContinuation continuation =
  When
    [ Case (Choice choiceId [ Bound zero (fromInt 1000000000) ]) continuation ]
    timeout
    timeoutContinuation

wait :: Timeout -> Contract -> Contract
wait = When []

gtLtEq :: Value -> Value -> Contract -> Contract -> Contract -> Contract
gtLtEq value1 value2 gtContinuation ltContinuation eqContinuation =
  If (ValueGT value1 value2) gtContinuation
    $ If (ValueLT value1 value2) ltContinuation
        eqContinuation

recordDifference :: ValueId -> ChoiceId -> ChoiceId -> Contract -> Contract
recordDifference name choiceId1 choiceId2 = Let name
  (SubValue (ChoiceValue choiceId1) (ChoiceValue choiceId2))

transferUpToDeposit :: Party -> Value -> Party -> Value -> Contract -> Contract
transferUpToDeposit from payerDeposit to amount = Pay from (Account to) ada
  (Cond (ValueLT amount payerDeposit) amount payerDeposit)

extendedContract :: Contract
extendedContract =
  initialDeposit party partyDeposit (TimeParam "Party deposit deadline") Close
    $ initialDeposit counterparty counterpartyDeposit
        (TimeParam "Counterparty deposit deadline")
        Close
    $ wait (TimeParam "First window beginning")
    $ oracleInput priceBeginning (TimeParam "First window deadline") Close
    $ wait (TimeParam "Second window beginning")
    $ oracleInput priceEnd (TimeParam "Second window deadline") Close
    $ gtLtEq (ChoiceValue priceBeginning) (ChoiceValue priceEnd)
        ( recordDifference decreaseInPrice priceBeginning priceEnd
            $ transferUpToDeposit counterparty counterpartyDeposit party
                (UseValue decreaseInPrice)
                Close
        )
        ( recordDifference increaseInPrice priceEnd priceBeginning
            $ transferUpToDeposit party partyDeposit counterparty
                (UseValue increaseInPrice)
                Close
        )
        Close
