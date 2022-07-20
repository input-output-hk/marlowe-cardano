module Examples.PureScript.Escrow
  ( contractModule
  , fullExtendedContract
  , metadata
  , fixedTimeoutContract
  , defaultTimeContent
  ) where

import Prelude

import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Tuple.Nested (type (/\), (/\))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Bound(..)
  , ChoiceId(..)
  , ChoiceName
  , Party(..)
  , Token(..)
  )
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
    [ "Payment deadline" /\ unsafeInstantFromInt 600000
    , "Complaint deadline" /\ unsafeInstantFromInt 1800000
    , "Complaint response deadline" /\ unsafeInstantFromInt 2400000
    , "Mediation deadline" /\ unsafeInstantFromInt 3600000
    ]

metadata :: MetaData
metadata =
  { contractType: Escrow
  , contractName: "Purchase"
  , contractShortDescription:
      "In this contract a _**seller**_ wants to sell an item (like a bicycle) to a _**buyer**_ for a _price_."
  , contractLongDescription:
      "Neither trusts each other, but they both trust a _**mediator**_. The _**buyer**_ pays the _price_ into the contract account: if both the _**buyer**_ and the _**seller**_ agree that the _**buyer**_ has received the item, then the _**seller**_ receives the _price_; if not, then the _**mediator**_ ensures that the _**buyer**_ gets their money back."
  , choiceInfo:
      ( Map.fromFoldable
          [ "Confirm problem"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "Acknowledge there was a problem and a refund must be granted."
                }
          , "Dismiss claim"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "The _**Mediator**_ does not see any problem with the exchange and the _**Seller**_ must be paid."
                }
          , "Dispute problem"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "The _**Seller**_ disagrees with the _**Buyer**_ about the claim that something went wrong."
                }
          , "Everything is alright"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "The transaction was uneventful, _**Buyer**_ agrees to pay the _**Seller**_."
                }
          , "Report problem"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "The _**Buyer**_ claims not having received the product that was paid for as agreed and would like a refund."
                }
          ]
      )
  , roleDescriptions:
      ( Map.fromFoldable
          [ "Mediator" /\
              "The mediator decides who is right in the case of dispute."
          , "Buyer" /\ "The buyer of the item."
          , "Seller" /\ "The seller of the item."
          ]
      )
  , timeParameterDescriptions:
      ( OMap.fromFoldable
          [ "Payment deadline" /\
              "The _**buyer**_ must pay the _price_ of the item by this time, otherwise the contract is cancelled."
          , "Complaint deadline" /\
              "The _**buyer**_ can only complain until this deadline, otherwise the contract will assume the transaction went smoothly and pay the _**seller**_."
          , "Complaint response deadline" /\
              "If the _**buyer**_ complained, the _**seller**_ must respond before this deadline, otherwise the contract will assume there was a problem with the transaction and refund the _**buyer**_."
          , "Mediation deadline" /\
              "If the _**buyer**_ and the _**seller**_ disagree, the _**mediator**_ must weigh in before this deadline, otherwise the contract will assume there was a problem with the transaction and refund the _**buyer**_."
          ]
      )
  , valueParameterInfo:
      ( OMap.fromFoldable
          [ "Price"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription: "The price of the item."
                }
          ]
      )
  }

ada :: Token
ada = Token "" ""

buyer :: Party
buyer = Role "Buyer23"

seller :: Party
seller = Role "Seller"

arbiter :: Party
arbiter = Role "Mediator"

price :: Value
price = ConstantParam "Price"

depositTimeout :: Timeout
depositTimeout = TimeParam "Payment deadline"

disputeTimeout :: Timeout
disputeTimeout = TimeParam "Complaint deadline"

answerTimeout :: Timeout
answerTimeout = TimeParam "Complaint response deadline"

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
