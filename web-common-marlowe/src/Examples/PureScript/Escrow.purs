module Examples.PureScript.Escrow
  ( contractTemplate
  , fullExtendedContract
  , metaData
  , fixedTimeoutContract
  , defaultTimeContent
  ) where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
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
import Marlowe.Semantics
  ( Bound(..)
  , ChoiceId(..)
  , ChoiceName
  , Party(..)
  , Token(..)
  )
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
    [ "Payment deadline" /\ unsafeInstantFromInt 600000
    , "Complaint deadline" /\ unsafeInstantFromInt 1800000
    , "Complaint response deadline" /\ unsafeInstantFromInt 2400000
    , "Mediation deadline" /\ unsafeInstantFromInt 3600000
    ]

metaData :: MetaData
metaData = Metadata.escrow

ada :: Token
ada = Token "" ""

buyer :: Party
buyer = Role "Buyer"

seller :: Party
seller = Role "Seller"

arbiter :: Party
arbiter = Role "Mediator"

price :: Value
price = ConstantParam "Price"

depositTimeout :: Timeout
depositTimeout = TimeParam "Payment deadline"

disputeTimeout :: Timeout
disputeTimeout = TimeParam "Complaint response deadline"

answerTimeout :: Timeout
answerTimeout = TimeParam "Complaint deadline"

arbitrageTimeout :: Timeout
arbitrageTimeout = TimeParam "Mediation deadline"

choice :: ChoiceName -> Party -> BigInt -> Contract -> Case
choice choiceName chooser choiceValue continuation =
  Case
    ( Choice (ChoiceId choiceName chooser)
        [ Bound choiceValue choiceValue ]
    )
    continuation

deposit :: Timeout -> Contract -> Contract -> Contract
deposit timeout timeoutContinuation continuation =
  When [ Case (Deposit seller buyer ada price) continuation ]
    timeout
    timeoutContinuation

choices
  :: Timeout
  -> Party
  -> Contract
  -> Array (BigInt /\ ChoiceName /\ Contract)
  -> Contract
choices timeout chooser timeoutContinuation list =
  When
    ( do
        (choiceValue /\ choiceName /\ continuation) <- list
        pure $ choice choiceName chooser choiceValue continuation
    )
    timeout
    timeoutContinuation

sellerToBuyer :: Contract -> Contract
sellerToBuyer = Pay seller (Account buyer) ada price

paySeller :: Contract -> Contract
paySeller = Pay buyer (Party seller) ada price

fullExtendedContract :: Contract
fullExtendedContract =
  deposit depositTimeout Close
    $ choices disputeTimeout buyer Close
        [ (zero /\ "Everything is alright" /\ Close)
        , ( one /\ "Report problem"
              /\
                ( sellerToBuyer
                    $ choices answerTimeout seller Close
                        [ (one /\ "Confirm problem" /\ Close)
                        , ( zero /\ "Dispute problem"
                              /\ choices arbitrageTimeout arbiter Close
                                [ (zero /\ "Dismiss claim" /\ paySeller Close)
                                , (one /\ "Confirm problem" /\ Close)
                                ]
                          )
                        ]
                )
          )
        ]
