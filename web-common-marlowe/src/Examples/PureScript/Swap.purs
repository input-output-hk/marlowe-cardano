module Examples.PureScript.Swap
  ( contractModule
  , fullExtendedContract
  , metadata
  , fixedTimeoutContract
  , defaultTimeContent
  ) where

import Prelude

import Data.BigInt.Argonaut (fromInt)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Core.V1.Semantics.Types (Party(..), Token(..))
import Language.Marlowe.Extended.V1
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Module(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ContractType(..)
  , MetaData
  , NumberFormat(..)
  )
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Time (unsafeInstantFromInt)

contractModule :: Module
contractModule = Module { metadata, contract: fullExtendedContract }

fixedTimeoutContract :: Contract
fixedTimeoutContract =
  fillTemplate
    ( TemplateContent
        { timeContent: defaultTimeContent
        , valueContent: Map.empty
        }
    )
    fullExtendedContract

defaultTimeContent :: Map String Instant
defaultTimeContent =
  Map.fromFoldable
    [ "Timeout for Ada deposit" /\ unsafeInstantFromInt 600000
    , "Timeout for dollar deposit" /\ unsafeInstantFromInt 1200000
    ]

metadata :: MetaData
metadata =
  { contractType: Swap
  , contractName: "Swap of Ada and dollar tokens"
  , contractShortDescription: "Atomically exchange of Ada and dollar tokens."
  , contractLongDescription:
      "Waits until one party deposits Ada and the other party deposits dollar tokens. If both parties collaborate it carries the exchange atomically, otherwise parties are refunded."
  , choiceInfo: Map.empty
  , roleDescriptions:
      ( Map.fromFoldable
          [ "Ada provider" /\ "The party that provides the Ada."
          , "Dollar provider" /\ "The party that provides the dollar tokens."
          ]
      )
  , timeParameterDescriptions:
      ( OMap.fromFoldable
          [ "Timeout for Ada deposit" /\
              "Deadline by which Ada must be deposited."
          , "Timeout for dollar deposit" /\
              "Deadline by which dollar tokens must be deposited (must be after the deadline for Ada deposit)."
          ]
      )
  , valueParameterInfo:
      ( OMap.fromFoldable
          [ "Amount of Ada"
              /\
                { valueParameterFormat: DecimalFormat 0 "₳"
                , valueParameterDescription:
                    "Amount of Ada to be exchanged for dollars."
                }
          , "Amount of dollars"
              /\
                { valueParameterFormat: DecimalFormat 0 "$"
                , valueParameterDescription:
                    "Amount of dollar tokens to be exchanged for Ada."
                }
          ]
      )
  }

ada :: Token
ada = Token "" ""

lovelacePerAda :: Value
lovelacePerAda = Constant (fromInt 1000000)

amountOfAda :: Value
amountOfAda = ConstantParam "Amount of Ada"

amountOfLovelace :: Value
amountOfLovelace = MulValue lovelacePerAda amountOfAda

amountOfDollars :: Value
amountOfDollars = ConstantParam "Amount of dollars"

adaDepositTimeout :: Timeout
adaDepositTimeout = TimeParam "Timeout for Ada deposit"

dollarDepositTimeout :: Timeout
dollarDepositTimeout = TimeParam "Timeout for dollar deposit"

dollars :: Token
dollars = Token "85bb65" "dollar"

type SwapParty =
  { party :: Party
  , currency :: Token
  , amount :: Value
  }

adaProvider :: SwapParty
adaProvider =
  { party: Role "Ada provider"
  , currency: ada
  , amount: amountOfLovelace
  }

dollarProvider :: SwapParty
dollarProvider =
  { party: Role "Dollar provider"
  , currency: dollars
  , amount: amountOfDollars
  }

makeDeposit :: SwapParty -> Timeout -> Contract -> Contract -> Contract
makeDeposit src timeout timeoutContinuation continuation =
  When
    [ Case (Deposit src.party src.party src.currency src.amount)
        continuation
    ]
    timeout
    timeoutContinuation

makePayment :: SwapParty -> SwapParty -> Contract -> Contract
makePayment src dest continuation = Pay src.party (Party $ dest.party)
  src.currency
  src.amount
  continuation

fullExtendedContract :: Contract
fullExtendedContract =
  makeDeposit adaProvider adaDepositTimeout Close
    $ makeDeposit dollarProvider dollarDepositTimeout Close
    $ makePayment adaProvider dollarProvider
    $ makePayment dollarProvider adaProvider
        Close
