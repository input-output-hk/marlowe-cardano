module Component.ContractPreview.View
  ( contractPreviewCard
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
import Data.Either (fromLeft, fromRight)
import Data.Lens ((^.))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.HTML (a, div, h3, input, p, text)
import Halogen.HTML.Events.Extra (onClick_, onValueInput_)
import Halogen.HTML.Properties (InputType(..), placeholder, type_, value)
import Humanize (contractIcon)
import MainFrame.Types (ChildSlots)
import Marlowe.Extended.Metadata (_contractName, _contractType)
import Marlowe.Semantics (Slot)
import Page.Contract.Lenses (_executionState, _stateMetadata, _stateNickname)
import Page.Contract.State (currentStep)
import Page.Contract.Types (Action(..), State(..))

-- This card shows a preview of the contract (intended to be used in the dashboard)
contractPreviewCard
  :: forall m. MonadAff m => Slot -> State -> ComponentHTML Action ChildSlots m
contractPreviewCard currentSlot state =
  let
    nickname = state ^. _stateNickname

    contractType = state ^. (_stateMetadata <<< _contractType)

    contractName = state ^. (_stateMetadata <<< _contractName)

    stepPanel = case state of
      Started started ->
        let
          executionState = started ^. _executionState
        in
          div
            [ classNames [ "px-4", "py-2" ] ]
            [ p
                [ classNames [ "text-xs", "font-semibold" ] ]
                [ text $ "Current step:" <> show (currentStep started + 1) ]
            , p
                [ classNames [ "font-semibold" ] ]
                [ text $ timeoutString currentSlot executionState ]
            ]
      Starting _ ->
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
                  , value (ContractNickname.toString nickname)
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
          [ case state of
              Started started -> currentStepActions (currentStep started + 1)
                started
              Starting _ -> startingStepActions
          ]
      ]

