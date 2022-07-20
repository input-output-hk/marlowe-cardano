module Examples.PureScript.ZeroCouponBond
  ( contractTemplate
  , fullExtendedContract
  , metaData
  , fixedTimeoutContract
  , defaultTimeContent
  ) where

import Prelude

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
    [ "Loan deadline" /\ unsafeInstantFromInt 600000
    , "Payback deadline" /\ unsafeInstantFromInt 1500000
    ]

metaData :: MetaData
metaData = Metadata.zeroCouponBond

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
