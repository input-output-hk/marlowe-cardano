module MainFrame.View where

import Prologue hiding (div)

import Data.Lens (view, (^.))
import Data.MnemonicPhrase (class CheckMnemonic)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML (div)
import Halogen.HTML as H
import Halogen.Store.Monad (class MonadStore)
import MainFrame.Lenses
  ( _currentSlot
  , _dashboardState
  , _subState
  , _tzOffset
  , _welcomeState
  )
import MainFrame.Types (Action(..), ChildSlots, State, _toaster)
import Page.Dashboard.View (dashboardCard, dashboardScreen)
import Page.Welcome.View (welcomeCard, welcomeScreen)
import Store as Store
import Toast.State as Toast

render
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => CheckMnemonic m
  => State
  -> ComponentHTML Action ChildSlots m
render state =
  let
    currentSlot = state ^. _currentSlot

    tzOffset = state ^. _tzOffset
  in
    div [ classNames [ "h-full" ] ]
      $
        case view _subState state of
          Left _ ->
            [ renderSubmodule _welcomeState WelcomeAction welcomeScreen state
            , renderSubmodule _welcomeState WelcomeAction welcomeCard state
            ]
          Right _ ->
            [ renderSubmodule _dashboardState DashboardAction
                (dashboardScreen { currentSlot, tzOffset })
                state
            , renderSubmodule _dashboardState DashboardAction dashboardCard
                state
            ]
          <> [ H.slot_ _toaster unit Toast.component unit ]
