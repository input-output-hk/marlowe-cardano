module MainFrame.View where

import Prologue hiding (div)

import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.Toast (class Toast)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens ((^.), (^?))
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML (div)
import Halogen.HTML as H
import Halogen.Store.Monad (class MonadStore)
import MainFrame.Lenses
  ( _addressBook
  , _currentTime
  , _dashboardState
  , _store
  , _subState
  , _tzOffset
  , _welcomeState
  )
import MainFrame.Types (Action(..), ChildSlots, State, _toaster)
import Page.Dashboard.View (dashboardCard, dashboardScreen)
import Page.Welcome.View (welcomeCard, welcomeScreen)
import Store (_contracts, _wallet)
import Store as Store
import Store.Wallet (_connectedWallet)
import Toast.State as Toast

render
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML Action ChildSlots m
render state =
  let
    addressBook = state ^. _addressBook

    currentTime = state ^. _currentTime

    tzOffset = state ^. _tzOffset

    subState = state ^. _subState

    mWallet = state ^? _store <<< _wallet <<< _connectedWallet

    contracts = state ^. _store <<< _contracts
  in
    div [ classNames [ "h-full" ] ]
      $
        case mWallet, subState of
          Just wallet, Right _ ->
            [ renderSubmodule
                _dashboardState
                DashboardAction
                ( \dashboardState ->
                    dashboardScreen
                      { addressBook, currentTime, tzOffset, wallet, contracts }
                      dashboardState
                )
                state
            , renderSubmodule
                _dashboardState
                DashboardAction
                ( \dashboardState ->
                    dashboardCard
                      { addressBook, currentTime, tzOffset, wallet, contracts }
                      dashboardState
                )
                state
            ]
          _, _ ->
            [ renderSubmodule _welcomeState WelcomeAction welcomeScreen state
            , renderSubmodule
                _welcomeState
                WelcomeAction
                welcomeCard
                state
            ]
          <> [ H.slot_ _toaster unit Toast.component unit ]
