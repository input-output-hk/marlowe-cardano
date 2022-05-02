module MainFrame.View where

import Prologue hiding (div)

import Capability.Marlowe (class ManageMarlowe)
import Capability.PAB (class ManagePAB)
import Capability.PlutusApps.FollowerApp (class FollowerApp)
import Capability.Toast (class Toast)
import Clipboard (class MonadClipboard)
import Component.Icons (icon)
import Component.Icons as Icon
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Fork.Class (class MonadKill)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadAsk)
import Data.Either (fromLeft)
import Data.Lens ((^.), (^?))
import Effect.Aff (Error, Fiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Env (Env)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.Extra (mapComponentAction)
import Halogen.HTML (div)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.Store.Monad (class MonadStore)
import MainFrame.Lenses (_enteringDashboardState, _store, _subState)
import MainFrame.Types (Action(..), ChildSlots, State, _toaster)
import Marlowe.Run.Server as MarloweRun
import Page.Dashboard.State as Dashboard
import Page.Dashboard.Types (_dashboard)
import Page.Dashboard.View (appTemplate, appTemplateHeader)
import Page.Welcome.State as Welcome
import Page.Welcome.View (welcomeCard, welcomeScreen)
import Servant.PureScript (class MonadAjax)
import Store (_wallet)
import Store as Store
import Store.Wallet (_connectedWallet)
import Toast.State as Toast

render
  :: forall m
   . MonadAff m
  => MonadKill Error Fiber m
  => MonadUnliftAff m
  => MonadAsk Env m
  => MonadAjax MarloweRun.Api m
  => MonadTime m
  => ManageMarlowe m
  => ManagePAB m
  => MonadClipboard m
  => MonadLogger StructuredLog m
  => FollowerApp m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML Action ChildSlots m
render state =
  let
    subState = state ^. _subState

    enteringDashboardState = state ^. _enteringDashboardState

    mWallet = state ^? _store <<< _wallet <<< _connectedWallet
  in
    div [ classNames [ "h-full" ] ] $ join
      [ case enteringDashboardState, mWallet, subState of
          _, Just wallet, Right _ ->
            [ HH.slot_ _dashboard unit Dashboard.component wallet ]
          true, _, _ ->
            [ enteringDashboardStateView ]
          _, _, _ ->
            let
              welcomeState = fromLeft Welcome.initialState subState
            in
              mapComponentAction WelcomeAction
                <$> [ welcomeScreen welcomeState, welcomeCard welcomeState ]
      , [ H.slot_ _toaster unit Toast.component unit ]
      ]

enteringDashboardStateView :: forall w i. HH.HTML w i
enteringDashboardStateView = do
  appTemplate false (appTemplateHeader Nothing false []) $
    HH.div
      [ classNames
          [ "h-full", "flex", "flex-col", "justify-center", "items-center" ]
      ]
      [ icon Icon.ImportExport [ "text-big-icon", "text-gray" ]
      , HH.p
          [ classNames [ "flex", "items-center", "gap-2" ] ]
          [ icon Icon.Sync [ "animate-spin" ]
          , HH.text "Connecting your wallet to the PAB..."
          ]
      ]
