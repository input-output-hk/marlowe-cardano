module Component.ContractPreview.View
  ( contractPreviewCard
  , contractStartingPreviewCard
  ) where

import Prologue hiding (div)

import Component.Contract.View
  ( currentStepActions
  , startingStepActions
  , timeoutString
  )
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon)
import Component.Progress.Circular as Progress
import Css as Css
import Data.ContractNickname as ContractNickname
import Data.Either (fromRight)
import Data.Lens ((^.))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.HTML (a, div, h3, input, p, text)
import Halogen.HTML.Events.Extra (onClick_, onValueInput_)
import Halogen.HTML.Properties (InputType(..), placeholder, type_, value)
import Humanize (contractIcon)
import MainFrame.Types (ChildSlots)
import Marlowe.Execution.State (numberOfConfirmedTxs)
import Marlowe.Execution.State as Execution
import Marlowe.Execution.Types as Execution
import Marlowe.Extended (ContractType(..))
import Marlowe.Extended.Metadata (_contractName, _contractType)
import Marlowe.Semantics (Slot)
import Page.Contract.Lenses (_executionState, _metadata)
import Page.Contract.State (currentStep)
import Page.Contract.Types (Action(..), StartedState, StartingState)

-- This card shows a preview of synced contracts (intended to be used in the dashboard)
contractPreviewCard
  :: forall m
   . MonadAff m
  => Slot
  -> Execution.State
  -> ComponentHTML Action ChildSlots m
contractPreviewCard currentSlot state =
  let
    -- FIXME-3208
    contractType = Escrow
    contractName = "fixme"
    -- contractType = state ^. (_metadata <<< _contractType)
    -- contractName = state ^. (_metadata <<< _contractName)

    nickname = Execution.contractName state

    stepPanel =
      div
        [ classNames [ "px-4", "py-2" ] ]
        [ p
            [ classNames [ "text-xs", "font-semibold" ] ]
            [ text $ "Current step:" <> show (numberOfConfirmedTxs state + 1) ]
        , p
            [ classNames [ "font-semibold" ] ]
            [ text $ timeoutString currentSlot state ]
        ]
    -- FIXME-3208
    -- stepActions = currentStepActions state
    stepActions = startingStepActions
  in
    div
      [ classNames
          [ "shadow", "bg-white", "rounded", "divide-y", "divide-gray" ]
      ]
      [ div
          [ classNames [ "flex", "gap-2", "px-4", "py-2" ] ]
          [ div
              [ classNames [ "flex-1" ] ]
              [ h3
                  [ classNames [ "flex", "gap-2", "items-center" ] ]
                  [ contractIcon contractType
                  , text contractName
                  ]
              -- FIXME-3208: Replace with form
              , input
                  [ classNames $ Css.inputNoBorder <> [ "-ml-2", "text-lg" ]
                  , type_ InputText
                  , value nickname
                  , onValueInput_
                      ( SetNickname <<< fromRight ContractNickname.unknown <<<
                          ContractNickname.fromString
                      )
                  , placeholder "Please rename"
                  ]
              ]
          , a
              [ classNames [ "flex", "items-center" ]
              , onClick_ SelectSelf
              ]
              [ icon Icon.ArrowRight [ "text-28px" ] ]
          ]
      , stepPanel
      , div
          [ classNames [ "h-dashboard-card-actions", "overflow-y-auto", "p-4" ]
          ]
          [ stepActions
          ]
      ]

-- FIXME-3208: Factor out commonalities between contractStartingPreviewCard and contractPreviewCard
contractStartingPreviewCard
  :: forall m. MonadAff m => StartingState -> ComponentHTML Action ChildSlots m
contractStartingPreviewCard state =
  let
    -- FIXME-3208:
    nickname = "Unknown"

    contractType = state ^. (_metadata <<< _contractType)

    contractName = state ^. (_metadata <<< _contractName)

    stepPanel =
      div
        [ classNames
            [ "px-4"
            , "py-2"
            , "flex"
            , "items-center"
            , "justify-between"
            ]
        ]
        [ p
            [ classNames [ "text-sm", "font-semibold" ] ]
            [ text $ "Starting contract…" ]
        , Progress.view Progress.defaultSpec
        ]
    stepActions = startingStepActions
  in
    div
      [ classNames
          [ "shadow", "bg-white", "rounded", "divide-y", "divide-gray" ]
      ]
      [ div
          [ classNames [ "flex", "gap-2", "px-4", "py-2" ] ]
          [ div
              [ classNames [ "flex-1" ] ]
              [ h3
                  [ classNames [ "flex", "gap-2", "items-center" ] ]
                  [ contractIcon contractType
                  , text contractName
                  ]
              -- FIXME-3208: Replace with form
              , input
                  [ classNames $ Css.inputNoBorder <> [ "-ml-2", "text-lg" ]
                  , type_ InputText
                  , value nickname
                  , onValueInput_
                      ( SetNickname <<< fromRight ContractNickname.unknown <<<
                          ContractNickname.fromString
                      )
                  , placeholder "Please rename"
                  ]
              ]
          , a
              [ classNames [ "flex", "items-center" ]
              , onClick_ SelectSelf
              ]
              [ icon Icon.ArrowRight [ "text-28px" ] ]
          ]
      , stepPanel
      , div
          [ classNames [ "h-dashboard-card-actions", "overflow-y-auto", "p-4" ]
          ]
          [ stepActions
          ]
      ]

