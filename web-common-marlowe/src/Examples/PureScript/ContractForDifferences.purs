module Examples.PureScript.ContractForDifferences
  ( contractModule
  , metadata
  , defaultTimeContent
  , contract
  ) where

import Prelude

import Data.BigInt.Argonaut (fromInt)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
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
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ContractType(..)
  , MetaData
  , NumberFormat(..)
  )
import Marlowe.Time (unsafeInstantFromInt)

contractModule :: Module
contractModule = Module { metadata, contract }

metadata :: MetaData
metadata =
  { contractType: ContractForDifferences
  , contractName: "CFD"
  , contractShortDescription:
      "Contract For Differences. Two parties deposit Ada in a contract and after some time the Ada is redistributed among them depending on the change in price of an asset as reported by a third party (_**oracle**_)."
  , contractLongDescription:
      "At the beginning of the contract, _**party**_ and _**counterparty**_ deposit some Ada in the contract. At the end of the contract, all Ada deposited is redistributed depending on the change in price in Ada of an asset (as reported by the _**oracle**_). If the price in Ada of the asset increases, the difference goes to _**counterparty**_; if it decreases, the difference goes to _**party**_, up to a maximum of the amount deposited at the beginning."
  , choiceInfo:
      ( Map.fromFoldable
          [ "Price in first window"
              /\
                { choiceFormat: DecimalFormat 6 "₳"
                , choiceDescription:
                    "Price in ADA of the asset in the first window."
                }
          , "Price in second window"
              /\
                { choiceFormat: DecimalFormat 6 "₳"
                , choiceDescription:
                    "Price in ADA of the asset in the second window."
                }
          ]
      )
  , roleDescriptions:
      ( Map.fromFoldable
          [ "Counterparty" /\
              "The _**counterparty**_ will get the difference in the price of the asset if it increases."
          , "Party" /\
              "The _**party**_ will get the difference in the price of the asset if it decreases."
          , "Oracle" /\
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
          ]
      )
  }

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

contract :: Contract
contract =
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
