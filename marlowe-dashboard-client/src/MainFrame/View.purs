module MainFrame.View where

import Prologue hiding (div)

import Capability.Marlowe (class ManageMarlowe)
import Capability.Toast (class Toast)
import Clipboard (class MonadClipboard)
import Control.Monad.Fork.Class (class MonadKill)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadAsk)
import Data.Either (fromLeft)
import Data.Lens ((^.), (^?))
import Effect.Aff (Error, Fiber)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.Extra (mapComponentAction)
import Halogen.HTML (div)
import Halogen.HTML as H
import Halogen.Store.Monad (class MonadStore)
import MainFrame.Lenses (_currentTime, _store, _subState, _tzOffset)
import MainFrame.Types (Action(..), ChildSlots, State, _toaster)
import Page.Dashboard.View (dashboardCard, dashboardScreen)
import Page.Welcome.State as Welcome
import Page.Welcome.View (welcomeCard, welcomeScreen)
import Store (_contracts, _wallet)
import Store as Store
import Store.Wallet (_connectedWallet)
import Toast.State as Toast

render
  :: forall m
   . MonadAff m
  => MonadKill Error Fiber m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => MonadClipboard m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML Action ChildSlots m
render state =
  let
    currentTime = state ^. _currentTime

    tzOffset = state ^. _tzOffset

    subState = state ^. _subState

    mWallet = state ^? _store <<< _wallet <<< _connectedWallet

    contracts = state ^. _store <<< _contracts
  in
    div [ classNames [ "h-full" ] ] $ join
      [ case mWallet, subState of
          Just wallet, Right dashboardState ->
            mapComponentAction DashboardAction <$>
              [ dashboardScreen
                  { currentTime, tzOffset, wallet, contracts }
                  dashboardState
              , dashboardCard
                  { currentTime, tzOffset, wallet, contracts }
                  dashboardState
              ]
          _, _ ->
            let
              welcomeState = fromLeft Welcome.initialState subState
            in
              mapComponentAction WelcomeAction
                <$> [ welcomeScreen welcomeState, welcomeCard welcomeState ]
      , [ H.slot_ _toaster unit Toast.component unit ]
      ]
