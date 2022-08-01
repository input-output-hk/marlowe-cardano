module Examples.PureScript.ContractForDifferencesWithOracle
  ( contractModule
  , metadata
  , contract
  ) where

import Prelude

import Data.BigInt.Argonaut (fromInt)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Tuple.Nested ((/\))
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
  , Module(..)
  , Observation(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Language.Marlowe.Extended.V1.Metadata (oracleRatioFormat)
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ContractType(..)
  , MetaData
  , NumberFormat(..)
  )

contractModule :: Module
contractModule = Module { metadata, contract }

metadata :: MetaData
metadata =
  { contractType: ContractForDifferences
  , contractName: "CFD with Oracle"
  , contractShortDescription:
      "Contract For Differences with Oracle. Two parties deposit Ada in a contract and after some time the Ada is redistributed among them depending on the change in price of an asset."
  , contractLongDescription:
      "At the beginning of the contract, _**party**_ and _**counterparty**_ deposit some Ada in the contract. At the end of the contract, all Ada deposited is redistributed depending on the change in price in dollars of an asset (as reported by the _**oracle**_). The asset in this contract is an amount of Ada. If the price in dollars of the asset increases, the difference goes to _**counterparty**_; if it decreases, the difference goes to _**party**_, up to a maximum of the amount deposited at the beginning."
  , choiceInfo:
      ( Map.fromFoldable
          [ "dir-adausd"
              /\
                { choiceFormat: oracleRatioFormat "ADA/USD"
                , choiceDescription:
                    "Exchange rate ADA/USD in the first window."
                }
          , "inv-adausd"
              /\
                { choiceFormat: oracleRatioFormat "USD/ADA"
                , choiceDescription:
                    "Exchange rate USD/ADA in the second window."
                }
          ]
      )
  , roleDescriptions:
      ( Map.fromFoldable
          [ "Counterparty" /\
              "The _**counterparty**_ will get the difference in the price of the asset if it increases."
          , "Party" /\
              "The _**party**_ will get the difference in the price of the asset if it decreases."
          , "kraken" /\
              "The _**oracle**_ provides the price of the asset at the beginning (first window) and at the end (second window) of the contract (in this case the _**oracle**_ provides the conversion rate between Ada and dollars)."
          ]
      )
  , timeParameterDescriptions:
      ( OMap.fromFoldable
          [ "Party deposit deadline" /\
              "The _amount paid by party_ must be deposited by this deadline, otherwise the contract is cancelled."
          , "Counterparty deposit deadline" /\
              "The _amount paid by counterparty_ must be deposited by this deadline, otherwise the contract is cancelled and money is refunded."
          , "First window beginning" /\
              "The first _**oracle**_ reading must be taken after this."
          , "First window deadline" /\
              "The first _**oracle**_ reading must be taken before this, otherwise the contract is cancelled and money is refunded."
          , "Second window beginning" /\
              "The second _**oracle**_ reading must be taken after this."
          , "Second window deadline" /\
              "The second _**oracle**_ reading must be taken before this, otherwise the contract is cancelled and money is refunded."
          ]
      )
  , valueParameterInfo:
      ( OMap.fromFoldable
          [ "Amount paid by party"
              /\
                { valueParameterFormat: DecimalFormat 6 "₳"
                , valueParameterDescription:
                    "Amount that the _**party**_ will deposit at the beginning of the contract."
                }
          , "Amount paid by counterparty"
              /\
                { valueParameterFormat: DecimalFormat 6 "₳"
                , valueParameterDescription:
                    "Amount that the _**counterparty**_ will deposit at the beginning of the contract."
                }
          , "Amount of Ada to use as asset"
              /\
                { valueParameterFormat: DecimalFormat 6 "₳"
                , valueParameterDescription:
                    "Amount of Ada whose price in dollars change to monitor."
                }
          ]
      )
  }

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

contract :: Contract
contract =
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
