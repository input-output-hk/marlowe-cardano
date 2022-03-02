module Component.ContractPreview.View
  ( contractPreviewCard
  , contractStartingPreviewCard
  ) where

import Prologue hiding (div)

import Component.Contract.View (startingStepActions, timeoutString)
import Component.CurrentStepActions.State as CurrentStepActions
import Component.CurrentStepActions.Types (Msg(..), _currentStepActions)
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon)
import Component.Progress.Circular as Progress
import Data.ContractNickname as ContractNickname
import Data.ContractUserParties (ContractUserParties)
import Data.DateTime.Instant (Instant)
import Data.Lens ((^.))
import Data.NewContract (NewContract(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.HTML (a, div, h3, p, slot, text)
import Halogen.HTML.Events.Extra (onClick_)
import Humanize (contractIcon)
import MainFrame.Types (ChildSlots)
import Marlowe.Execution.State (contractName) as Execution
import Marlowe.Execution.State (currentStep)
import Marlowe.Extended.Metadata (_contractName, _contractType)
import Page.Contract.Lenses (_marloweParams, _metadata)
import Page.Dashboard.Types (Action(..), ContractState)

-- This card shows a preview of synced contracts (intended to be used in the dashboard)
contractPreviewCard
  :: forall m
   . MonadAff m
  => Instant
  -> ContractState
  -> ComponentHTML Action ChildSlots m
contractPreviewCard
  currentTime
  { executionState, contractUserParties, namedActions } =
  let
    contractType = executionState ^. (_metadata <<< _contractType)
    contractName = executionState ^. (_metadata <<< _contractName)
    marloweParams = executionState ^. _marloweParams
    nickname = Execution.contractName executionState

    stepPanel =
      div
        [ classNames [ "px-4", "py-2" ] ]
        [ p
            [ classNames [ "text-xs", "font-semibold" ] ]
            [ text $ "Current step:" <> show (currentStep executionState + 1) ]
        , p
            [ classNames [ "font-semibold" ] ]
            [ text $ timeoutString currentTime executionState ]
        ]
    stepActions = slot
      _currentStepActions
      marloweParams
      CurrentStepActions.component
      { executionState, contractUserParties, namedActions }
      ( \(ActionSelected action num) -> OnAskContractActionConfirmation
          marloweParams
          action
          num
      )
  in
    div
      [ classNames
          [ "shadow", "bg-white", "rounded", "divide-y", "divide-gray" ]
      ]
      [ div
          [ classNames [ "flex", "gap-2", "px-4", "py-2" ] ]
          [ div
              [ classNames [ "flex-1", "truncate" ] ]
              [ h3
                  [ classNames [ "flex", "gap-2", "items-center" ] ]
                  [ contractIcon contractType
                  , text contractName
                  ]
              -- FIXME-3562: Make an input again
              , text nickname

              -- , input
              --     [ classNames $ Css.inputNoBorder <> [ "-ml-2", "text-lg" ]
              --     , type_ InputText
              --     , value nickname
              --     , onValueInput_
              --         ( SetNickname <<< fromRight ContractNickname.unknown <<<
              --             ContractNickname.fromString
              --         )
              --     , placeholder "Please rename"
              --     ]
              ]
          , a
              [ classNames [ "flex", "items-center" ]
              , onClick_ $ SelectContract $ Just marloweParams
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

-- FIXME-3487: Factor out commonalities between contractStartingPreviewCard and contractPreviewCard
contractStartingPreviewCard
  :: forall m. MonadAff m => NewContract -> ComponentHTML Action ChildSlots m
contractStartingPreviewCard (NewContract contractNickname metadata) =
  let
    nickname = ContractNickname.toString contractNickname

    contractType = metadata ^. _contractType

    contractName = metadata ^. _contractName

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
              -- FIXME-3562: Make an input again
              , text nickname
              -- , input
              --     [ classNames $ Css.inputNoBorder <> [ "-ml-2", "text-lg" ]
              --     , type_ InputText
              --     , value nickname
              --     , onValueInput_
              --         ( SetNickname <<< fromRight ContractNickname.unknown <<<
              --             ContractNickname.fromString
              --         )
              --     , placeholder "Please rename"
              --     ]
              ]
          , a
              [ classNames [ "flex", "items-center" ]
              -- TODO: SCP-3487 Fix the flow that creates a contract
              -- , onClick_ SelectSelf
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
