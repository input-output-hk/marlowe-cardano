module Examples.PureScript.EscrowWithCollateral
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
    [ "Collateral deposit by seller timeout" /\ unsafeInstantFromInt 600000
    , "Deposit of collateral by buyer timeout" /\ unsafeInstantFromInt 1200000
    , "Deposit of price by buyer timeout" /\ unsafeInstantFromInt 1800000
    , "Dispute by buyer timeout" /\ unsafeInstantFromInt 3000000
    , "Complaint deadline" /\ unsafeInstantFromInt 3600000
    ]

metadata :: MetaData
metadata =
  { contractType: Escrow
  , contractName: "Escrow with collateral"
  , contractShortDescription:
      "In this contract a _**seller**_ wants to sell an item (like a bicycle) to a _**buyer**_ for a _price_."
  , contractLongDescription:
      "In order to incentivise collaboration between the _**seller**_ and the _**buyer**_, at the beginning of the contract both parties deposit the _collateral amount_ that is burned if the parties disagree."
  , choiceInfo:
      ( Map.fromFoldable
          [ "Confirm problem"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "Acknowledge that there was a problem and a refund must be granted."
                }
          , "Dispute problem"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "The _**Seller**_ disagrees with the _**Buyer**_ about the claim that something went wrong and the collateral will be burnt."
                }
          , "Everything is alright"
              /\
                { choiceFormat: DefaultFormat
                , choiceDescription:
                    "The exchange was successful and the _**Buyer**_ agrees to pay the _**Seller**_."
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
          [ "Buyer" /\ "The party that pays for the item on sale."
          , "Seller" /\
              "The party that sells the item and gets the money if the exchange is successful."
          ]
      )
  , timeParameterDescriptions:
      ( OMap.fromFoldable
          [ "Collateral deposit by seller timeout" /\
              "The deadline by which the _**Seller**_ must deposit the _**Collateral amount**_ in the contract."
          , "Deposit of collateral by buyer timeout" /\
              "The deadline by which the _**Buyer**_ must deposit the _**Collateral amount**_ in the contract."
          , "Deposit of price by buyer timeout" /\
              "The deadline by which the _**Buyer**_ must deposit the _**Price**_ in the contract."
          , "Dispute by buyer timeout" /\
              "The deadline by which, if the _**Buyer**_ has not opened a dispute, the _**Seller**_ will be paid."
          , "Complaint deadline" /\
              "The deadline by which, if the _**Seller**_ has not responded to the dispute, the _**Buyer**_ will be refunded."
          ]
      )
  , valueParameterInfo:
      ( OMap.fromFoldable
          [ "Collateral amount"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription:
                    "The amount of Lovelace to be deposited by both parties at the start of the contract to serve as an incentive for collaboration."
                }
          , "Price"
              /\
                { valueParameterFormat: lovelaceFormat
                , valueParameterDescription:
                    "The amount of Lovelace to be paid by the _**Buyer**_ as part of the exchange."
                }
          ]
      )
  }

ada :: Token
ada = Token "" ""

buyer :: Party
buyer = Role "Buyer"

seller :: Party
seller = Role "Seller"

burnAddress :: Party
burnAddress = PK
  "0000000000000000000000000000000000000000000000000000000000000000"

price :: Value
price = ConstantParam "Price"

collateral :: Value
collateral = ConstantParam "Collateral amount"

sellerCollateralTimeout :: Timeout
sellerCollateralTimeout = TimeParam "Collateral deposit by seller timeout"

buyerCollateralTimeout :: Timeout
buyerCollateralTimeout = TimeParam "Deposit of collateral by buyer timeout"

depositTimeout :: Timeout
depositTimeout = TimeParam "Deposit of price by buyer timeout"

disputeTimeout :: Timeout
disputeTimeout = TimeParam "Dispute by buyer timeout"

answerTimeout :: Timeout
answerTimeout = TimeParam "Complaint deadline"

depositCollateral :: Party -> Timeout -> Contract -> Contract -> Contract
depositCollateral party timeout timeoutContinuation continuation =
  When [ Case (Deposit party party ada collateral) continuation ]
    timeout
    timeoutContinuation

burnCollaterals :: Contract -> Contract
burnCollaterals continuation =
  Pay seller (Party burnAddress) ada collateral
    $ Pay buyer (Party burnAddress) ada collateral
    $ continuation

deposit :: Timeout -> Contract -> Contract -> Contract
deposit timeout timeoutContinuation continuation =
  When [ Case (Deposit seller buyer ada price) continuation ]
    timeout
    timeoutContinuation

choice :: ChoiceName -> Party -> BigInt -> Contract -> Case
choice choiceName chooser choiceValue continuation =
  Case
    ( Choice (ChoiceId choiceName chooser)
        [ Bound choiceValue choiceValue ]
    )
    continuation

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

fullExtendedContract :: Contract
fullExtendedContract =
  depositCollateral seller sellerCollateralTimeout Close
    $ depositCollateral buyer buyerCollateralTimeout Close
    $ deposit depositTimeout Close
    $ choices disputeTimeout buyer Close
        [ (zero /\ "Everything is alright" /\ Close)
        , ( one /\ "Report problem"
              /\
                ( sellerToBuyer
                    $ choices answerTimeout seller Close
                        [ (one /\ "Confirm problem" /\ Close)
                        , (zero /\ "Dispute problem" /\ burnCollaterals Close)
                        ]
                )
          )
        ]
