module Examples.PureScript.Swap
  ( contractTemplate
  , fullExtendedContract
  , metaData
  , fixedTimeoutContract
  , defaultTimeContent
  ) where

import Prelude

import Data.BigInt.Argonaut (fromInt)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Examples.Metadata as Metadata
import Language.Marlowe.Core.V1.Semantics.Types (Party(..), Token(..))
import Language.Marlowe.Extended.V1
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Language.Marlowe.Extended.V1.Metadata (ContractTemplate, MetaData)
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Time (unsafeInstantFromInt)

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract: fullExtendedContract }

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

metaData :: MetaData
metaData = Metadata.swap

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
