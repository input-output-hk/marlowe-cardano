module Component.Transfer.View (transfer) where

import Prologue hiding (div)

import Component.Amount (amount)
import Component.Avatar.Types (Size(..)) as Avatar
import Component.Avatar.View (avatar)
import Component.Column (column)
import Component.Column as Column
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon)
import Component.Row (row)
import Component.Row as Row
import Component.Transfer.Types (Participant, Termini(..), Transfer)
import Data.Foldable (foldMap)
import Data.String (Pattern(..))
import Data.String.Extra (capitalize, endsWith)
import Halogen.Css (classNames)
import Halogen.HTML (HTML, div, span, text)
import Language.Marlowe.Core.V1.Semantics.Types (Party(..))

transfer :: forall w i. Transfer -> HTML w i
transfer { sender, recipient, token, quantity, termini } = case termini of
  AccountToAccount from to -> layout from "account" to "account"
  AccountToWallet from to -> layout from "account" to "wallet"
  WalletToAccount from to -> layout from "wallet" to "account"
  where
  layout from fromLabel to toLabel =
    column Column.Cramped [ "border-l-2", "px-2" ]
      [ account from sender fromLabel
      , row Row.Snug [ "px-1", "items-center" ]
          [ icon Icon.South [ "text-purple", "text-xs", "font-semibold" ]
          , amount token quantity [ "text-xs", "text-green" ]
          ]
      , account to recipient toLabel
      ]

-- TODO add read more icon and figure out action. Should it open the contact in
-- the contacts menu?
account :: forall w i. Party -> Participant -> String -> HTML w i
account party { nickname } accountTypeLabel =
  row Row.Cramped [ "items-center" ]
    [ avatar
        { nickname: avatarName
        , background: [ "bg-gradient-to-r", "from-purple", "to-lightpurple" ]
        , size: Avatar.Small
        }
    , div
        [ classNames
            [ "text-xs", "whitespace-nowrap", "overflow-hidden", "flex" ]
        ]
        [ span [ classNames [ "whitespace-pre" ] ]
            [ text
                $ capitalize
                $ case party of
                    PK _ -> accountTypeLabel <> " of "
                    Role role -> posessive role <> " "
            ]
        , span [ classNames [ "overflow-ellipsis", "overflow-hidden" ] ]
            [ text $ case party of
                PK pk -> pk
                Role _ -> accountTypeLabel
            ]
        , span [ classNames [ "whitespace-pre" ] ]
            [ text $ foldMap (\n -> " (" <> n <> ")") nickname ]
        ]

    ]
  where
  avatarName = case party of
    PK pk -> pk
    Role role -> role

  posessive noun
    | isPlural noun = noun <> "'"
    | otherwise = noun <> "'s"

  isPlural = endsWith $ Pattern "s"
