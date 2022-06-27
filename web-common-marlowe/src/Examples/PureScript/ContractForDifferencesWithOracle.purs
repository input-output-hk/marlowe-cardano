module Examples.PureScript.ContractForDifferencesWithOracle
  ( contractTemplate
  , metaData
  , extendedContract
  ) where

import Prelude

import Data.BigInt.Argonaut (fromInt)
import Examples.Metadata as Metadata
import Language.Marlowe.Core.V1.Semantics.Types
  ( Bound(..)
  , ChoiceId(..)
  , Party(..)
  , Token(..)
  , ValueId(..)
  )
import Language.Marlowe.Extended.V1
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Observation(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Language.Marlowe.Extended.V1.Metadata (ContractTemplate, MetaData)

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract }

metaData :: MetaData
metaData = Metadata.contractForDifferencesWithOracle

ada :: Token
ada = Token "" ""

party :: Party
party = Role "Party"

counterparty :: Party
counterparty = Role "Counterparty"

oracle :: Party
oracle = Role "kraken"

partyDeposit :: Value
partyDeposit = ConstantParam "Amount paid by party"

counterpartyDeposit :: Value
counterpartyDeposit = ConstantParam "Amount paid by counterparty"

priceBeginning :: Value
priceBeginning = ConstantParam "Amount of Ada to use as asset"

priceEnd :: ValueId
priceEnd = ValueId "Price in second window"

exchangeBeginning :: ChoiceId
exchangeBeginning = ChoiceId "dir-adausd" oracle

exchangeEnd :: ChoiceId
exchangeEnd = ChoiceId "inv-adausd" oracle

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
    [ Case (Choice choiceId [ Bound zero (fromInt 100000 * fromInt 1000000) ])
        continuation
    ]
    timeout
    timeoutContinuation

wait :: Timeout -> Contract -> Contract
wait = When []

gtLtEq :: Value -> Value -> Contract -> Contract -> Contract -> Contract
gtLtEq value1 value2 gtContinuation ltContinuation eqContinuation =
  If (ValueGT value1 value2) gtContinuation
    $ If (ValueLT value1 value2) ltContinuation
        eqContinuation

recordEndPrice :: ValueId -> ChoiceId -> ChoiceId -> Contract -> Contract
recordEndPrice name choiceId1 choiceId2 = Let name
  ( DivValue
      ( MulValue priceBeginning
          (MulValue (ChoiceValue choiceId1) (ChoiceValue choiceId2))
      )
      (Constant ((fromInt 100000000) * (fromInt 100000000)))
  )

recordDifference :: ValueId -> Value -> Value -> Contract -> Contract
recordDifference name val1 val2 = Let name (SubValue val1 val2)

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
    $ oracleInput exchangeBeginning (TimeParam "First window deadline") Close
    $ wait (TimeParam "Second window beginning")
    $ oracleInput exchangeEnd (TimeParam "Second window deadline") Close
    $ recordEndPrice priceEnd exchangeBeginning exchangeEnd
    $ gtLtEq priceBeginning (UseValue priceEnd)
        ( recordDifference decreaseInPrice priceBeginning (UseValue priceEnd)
            $ transferUpToDeposit counterparty counterpartyDeposit party
                (UseValue decreaseInPrice)
                Close
        )
        ( recordDifference increaseInPrice (UseValue priceEnd) priceBeginning
            $ transferUpToDeposit party partyDeposit counterparty
                (UseValue increaseInPrice)
                Close
        )
        Close
