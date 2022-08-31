module Examples.PureScript.ZeroCouponBond
  ( contractModule
  , fullExtendedContract
  , metadata
  , fixedTimeoutContract
  , defaultTimeContent
  ) where

import Prelude

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
import Language.Marlowe.Extended.V1.Metadata (lovelaceFormat)
import Language.Marlowe.Extended.V1.Metadata.Types (ContractType(..), MetaData)
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
    [ "Loan deadline" /\ unsafeInstantFromInt 600000
    , "Payback deadline" /\ unsafeInstantFromInt 1500000
    ]

metadata :: MetaData
metadata =
  { contractType: ZeroCouponBond
  , contractName: "Loan"
  , contractShortDescription:
      "A simple loan: the _**borrower**_ borrows the _amount_ from the _**lender**_, and at the _payback deadline_ pays back the _amount_ plus _interest_."
  , contractLongDescription:
      "This is a high risk/high reward contract. There is no guarantee that the _**borrower**_ will pay back the loan. However there is an opportunity for the _**lender**_ to set a high _interest_ rate at the cost of taking on this risk."
  , choiceInfo: Map.empty
  , roleDescriptions:
      ( Map.fromFoldable
          [ "Lender" /\ "The party that lends the _amount_."
          , "Borrower" /\ "The party that borrows the _amount_."
          ]
      )
  , timeParameterDescriptions:
      ( OMap.fromFoldable
          [ "Loan deadline" /\
              "The _**lender**_ needs to deposit the _amount_ by this time."
          , "Payback deadline" /\
              "The _**borrower**_ needs to deposit the repayment (_amount_ plus _interest_) by this time."
          ]
      )
  , valueParameterInfo:
      ( OMap.fromFoldable
          [ "Interest"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription:
                    "The interest paid by the _**borrower**_."
                }
          , "Amount"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription:
                    "The amount borrowed by the _**borrower**_."
                }
          ]
      )
  }

ada :: Token
ada = Token "" ""

discountedPrice :: Value
discountedPrice = ConstantParam "Amount"

notionalPrice :: Value
notionalPrice = AddValue (ConstantParam "Interest") discountedPrice

investor :: Party
investor = Role "Lender"

issuer :: Party
issuer = Role "Borrower"

initialExchange :: Timeout
initialExchange = TimeParam "Loan deadline"

maturityExchangeTimeout :: Timeout
maturityExchangeTimeout = TimeParam "Payback deadline"

transfer :: Timeout -> Party -> Party -> Value -> Contract -> Contract
transfer timeout from to amount continuation =
  When
    [ Case (Deposit from from ada amount)
        (Pay from (Party to) ada amount continuation)
    ]
    timeout
    Close

fullExtendedContract :: Contract
fullExtendedContract =
  transfer initialExchange investor issuer discountedPrice
    $ transfer maturityExchangeTimeout issuer investor notionalPrice
        Close
