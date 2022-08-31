module Component.ConfirmContractActionDialog.View
  ( render
  ) where

import Prologue hiding (div)

import Component.Amount (amount)
import Component.Box (box)
import Component.Box as Box
import Component.Button.Types as Button
import Component.Button.View (button)
import Component.Column (column)
import Component.Column as Column
import Component.ConfirmContractActionDialog.Types
  ( Action(..)
  , ComponentHTML
  , State
  , _action
  , _executionState
  , _roleTokens
  , _rolesCurrency
  , _transactionFeeQuote
  , _txInput
  , _wallet
  )
import Component.Expand as Expand
import Component.Heading (Preset(..), heading)
import Component.IconButton.View (iconButton)
import Component.Icons (icon_)
import Component.Icons as Icon
import Component.Link (link)
import Component.LoadingSubmitButton.State (loadingSubmitButton)
import Component.Row (row)
import Component.Row as Row
import Component.Transfer.Types
  ( Termini(..)
  , partyToParticipant
  , paymentToTransfer
  )
import Component.Transfer.View (transfer)
import Data.Array (fromFoldable)
import Data.Default (default)
import Data.Foldable (length)
import Data.Lens ((^.))
import Data.PABConnectedWallet (_assets)
import Effect.Aff.Class (class MonadAff)
import Halogen.Css (classNames)
import Halogen.HTML (HTML, div, div_, p, span, text)
import Language.Marlowe.Core.V1.Semantics (computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types
  ( ChoiceId(..)
  , Contract(..)
  , TransactionOutput(..)
  ) as Semantics
import Language.Marlowe.Core.V1.Semantics.Types (Token(..), getAda)
import MainFrame.Types (ChildSlots)
import Marlowe.Execution.State (currentStep)
import Marlowe.Execution.Types (NamedAction(..))

render :: forall m. MonadAff m => State -> ComponentHTML m
render state =
  let
    action = state ^. _action
    executionState = state ^. _executionState
    stepNumber = currentStep executionState + 1
  in
    column Column.Divided [ "h-full", "grid", "grid-rows-auto-1fr-auto" ]
      [ sectionBox [ "lg:p-5" ]
          $ heading H2 [ "leading-none" ]
              [ text
                  $ "Step "
                      <> show stepNumber
                      <> " "
                      <> case action of
                        MakeDeposit _ _ _ _ -> "deposit"
                        MakeChoice _ _ -> "choice"
                        CloseContract -> "close"
                        _ -> ""
              ]
      , summary state
      , confirmation state
      ]

summary :: forall m. Monad m => State -> ComponentHTML m
summary state = do
  let action = state ^. _action
  let roleTokens = state ^. _roleTokens
  let currencySymbol = state ^. _rolesCurrency
  sectionBox [ "overflow-y-scroll" ]
    $ column Column.Divided [ "space-y-4" ]
        [ column default []
            [ heading H3 []
                [ text
                    $
                      case action of
                        MakeDeposit _ _ _ _ -> "Deposit"
                        MakeChoice _ _ -> "Choice"
                        CloseContract -> "Close"
                        _ -> ""
                        <> " summary"
                ]
            , box Box.Card [] case action of
                MakeDeposit recipient sender token quantity ->
                  transfer
                    { sender:
                        partyToParticipant roleTokens currencySymbol sender
                    , recipient:
                        partyToParticipant roleTokens currencySymbol recipient
                    , token
                    , quantity
                    , termini: WalletToAccount sender recipient
                    }
                MakeChoice (Semantics.ChoiceId key _) _ ->
                  row Row.Between []
                    [ span [ classNames [ "font-semibold", "text-sm" ] ]
                        [ text "Your choice:" ]
                    , span [ classNames [ "font-semibold", "text-sm" ] ]
                        [ text key ]
                    ]
                CloseContract ->
                  row default []
                    [ span [ classNames [ "font-semibold", "text-sm" ] ]
                        [ text "You are closing the contract" ]
                    ]
                _ -> text ""
            ]
        , box Box.NoSpace [ "pt-4" ]
            $ Expand.expand_ "resultingAction" Expand.Closed (results state)
        ]

results
  :: forall m
   . Monad m
  => State
  -> Expand.State
  -> Expand.ComponentHTML ChildSlots Void m
results state = case _ of
  Expand.Opened ->
    layout Icon.ExpandLess
      $ box Box.Card []
          <$>
            ( fromFoldable $ map
                (transfer <<< paymentToTransfer roleTokens currencySymbol)
                payments
            )
              <>
                if willClose then
                  [ row Row.Between []
                      [ text "Contract finishes"
                      , icon_ Icon.Task
                      ]
                  ]
                else
                  []
  Expand.Closed -> layout Icon.ExpandMore []
  where
  roleTokens = state ^. _roleTokens
  action = state ^. _action
  executionState = state ^. _executionState
  currencySymbol = state ^. _rolesCurrency
  txInput = state ^. _txInput
  layout icon children =
    column Column.Snug []
      $
        [ row Row.Between [ "items-center" ]
            [ heading H4 [] [ text $ show count <> " resulting actions" ]
            , iconButton icon $ Just Expand.toggle
            ]
        ]
          <> children

  contract = executionState.contract

  semanticState = executionState.semanticState

  txOutput = computeTransaction txInput semanticState contract

  payments = case txOutput of
    Semantics.TransactionOutput { txOutPayments } ->
      fromFoldable txOutPayments
    _ -> []

  willClose = case txOutput of
    Semantics.TransactionOutput { txOutContract } ->
      txOutContract == Semantics.Close
    _ -> action == CloseContract

  count = length payments + if willClose then 1 else 0

confirmation :: forall m. MonadAff m => State -> ComponentHTML m
confirmation state =
  column Column.Divided []
    [ sectionBox [ "bg-lightgray" ]
        $ row Row.Between []
            [ span [ classNames [ "font-semibold", "text-sm" ] ]
                [ text "Wallet balance:" ]
            , amount (Token "" "") walletBalance [ "text-sm" ]
            ]
    , sectionBox []
        $ column default []
            [ div_
                [ heading H4 [ "font-semibold" ]
                    [ text "Confirm payment of" ]
                , p [ classNames [ "text-xs", "leading-none" ] ]
                    [ amount (Token "" "") total [ "text-2xl", "text-purple" ]
                    ]
                , p [ classNames [ "text-xxs", "opacity-50" ] ]
                    [ text "Transaction fee: ~"
                    , amount (Token "" "") transactionFeeQuote [ "text-xxs" ]
                    ]
                ]
            , div [ classNames [ "grid", "grid-cols-2", "gap-4" ] ]
                [ button
                    Button.Secondary
                    (Just $ CancelConfirmation)
                    []
                    [ text "Cancel" ]
                , loadingSubmitButton
                    { ref: "action-confirm-button"
                    , caption: "Confirm"
                    , styles: [ "flex-1" ]
                    , enabled: true
                    , handler: ConfirmAction
                    }
                ]
            ]
    , sectionBox []
        $ p [ classNames [ "text-xs" ] ]
            [ text
                "*Transaction fees are estimates only and are shown in completed contract steps "
            , link
                "https://docs.cardano.org/explore-cardano/fee-structure"
                []
                [ text "Read more in Docs" ]
            , text "."
            ]
    ]
  where
  action = state ^. _action
  transactionFeeQuote = state ^. _transactionFeeQuote
  wallet = state ^. _wallet
  walletBalance = getAda $ wallet ^. _assets

  total =
    transactionFeeQuote
      + case action of
          MakeDeposit _ _ _ amount -> amount
          _ -> zero

sectionBox :: forall i w. Array String -> HTML i w -> HTML i w
sectionBox css = box default $ [ "lg:px-5" ] <> css
