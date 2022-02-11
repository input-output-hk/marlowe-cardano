module Examples.PureScript.ZeroCouponBond
  ( contractTemplate
  , fullExtendedContract
  , metaData
  , fixedTimeoutContract
  , defaultSlotContent
  ) where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Examples.Metadata as Metadata
import Marlowe.Extended
  ( Action(..)
  , Case(..)
  , Contract(..)
  , Payee(..)
  , Timeout(..)
  , Value(..)
  )
import Marlowe.Extended.Metadata (ContractTemplate, MetaData)
import Marlowe.Semantics (Party(..), Token(..))
import Marlowe.Slot (secondsSinceShelley)
import Marlowe.Template (TemplateContent(..), fillTemplate)

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract: fullExtendedContract }

fixedTimeoutContract :: Contract
fixedTimeoutContract =
  fillTemplate
    ( TemplateContent
        { slotContent: defaultSlotContent
        , valueContent: Map.empty
        }
    )
    fullExtendedContract

defaultSlotContent :: Map String BigInt
defaultSlotContent =
  Map.fromFoldable
    [ "Loan deadline" /\ secondsSinceShelley 600
    , "Payback deadline" /\ secondsSinceShelley 1500
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
initialExchange = SlotParam "Loan deadline"

maturityExchangeTimeout :: Timeout
maturityExchangeTimeout = SlotParam "Payback deadline"

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
