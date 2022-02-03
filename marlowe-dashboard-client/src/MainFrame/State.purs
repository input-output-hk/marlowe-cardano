module MainFrame.State (mkMainFrame, handleAction) where

import Prologue

import Bridge (toFront)
import Capability.MainFrameLoop (class MainFrameLoop)
import Capability.Marlowe
  ( class ManageMarlowe
  , getFollowerApps
  , subscribeToPlutusApp
  , subscribeToWallet
  , unsubscribeFromPlutusApp
  , unsubscribeFromWallet
  )
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , getContractNicknames
  , walletLocalStorageKey
  )
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Component.Contacts.Lenses
  ( _companionAppId
  , _marloweAppId
  , _walletId
  , _walletInfo
  )
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (modify_)
import Data.AddressBook as AB
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Foldable (for_, traverse_)
import Data.Lens (_1, _2, _Just, _Left, assign, lens, preview, set, use, view)
import Data.Lens.Extra (peruse)
import Data.Map (keys)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.Set (toUnfoldable) as Set
import Data.Time.Duration (Minutes(..))
import Data.Traversable (for)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Env (Env)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen.Extra (imapState)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Language.Marlowe.Client (EndpointResponse(..), MarloweEndpointResult(..))
import LocalStorage (removeItem, setItem)
import MainFrame.Lenses
  ( _addressBook
  , _currentSlot
  , _dashboardState
  , _subState
  , _tzOffset
  , _webSocketStatus
  , _welcomeState
  )
import MainFrame.Types
  ( Action(..)
  , ChildSlots
  , Input
  , Msg
  , Query(..)
  , Slice
  , State
  , WebSocketStatus(..)
  )
import MainFrame.View (render)
import Marlowe.PAB (PlutusAppId)
import Page.Dashboard.Lenses (_contracts)
import Page.Dashboard.State (handleAction, mkInitialState) as Dashboard
import Page.Dashboard.Types (Action(..), State) as Dashboard
import Page.Welcome.State (handleAction, initialState) as Welcome
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient(..)
  , InstanceStatusToClient(..)
  )
import Store as Store
import Toast.Types
  ( decodedAjaxErrorToast
  , decodingErrorToast
  , errorToast
  , successToast
  )
import WebSocket.Support as WS

{-
The Marlowe Run app consists of six main workflows:

1. UC-WALLET-TESTNET-1 Create a testnet wallet (this only make sense for the centralized testnet site and may become redundant when we integrate with light wallets).
2. UC-WALLET-TESTNET-2 Restore a testnet wallet (same note as before).
3. Disconnect a wallet.
4. Start a contract.
5. Move a contract forward.
6. Redeem payments (this is triggered automatically - but may need to be manual when we
   integrate with real wallets).

There are two main application states: the `Welcome` state (for workflows 1 and 2), and the
`Dashboard` state (for workflows 3-6). Initially we are in the `Welcome` state. Connecting a wallet
(workflow 2) moves you into the `Dashboard` state; disconnecting a wallet (workflow 4) moves you
back into the `Welcome` state.

Because of the synchronous nature of the app, with messages passing between the browser and the PAB
(both through direct API calls and a WebSocket), these workflows are in general spread out all over
the code. Comments are added throughout with the format "[Workflow n][m]" - so you can search the
code for e.g. "[Workflow 4]" to see all of the steps involved in starting a contract.
-}
mkMainFrame
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => MonadRec m
  => ManageMarlowe m
  => Toast m
  => MonadClipboard m
  => MainFrameLoop m
  => Component Query Input Msg m
mkMainFrame =
  connect (selectEq \{ addressBook, wallet } -> { addressBook, wallet }) $
    mkComponent
      { initialState: deriveState emptyState
      , render: render
      , eval:
          mkEval defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Receive
            , initialize = Just Init
            }
      }

emptyState :: State
emptyState =
  { addressBook: AB.empty
  , tzOffset: Minutes 0.0 -- this will be updated in deriveState
  , webSocketStatus: WebSocketClosed Nothing
  , currentSlot: zero -- this will be updated as soon as the websocket connection is working
  , subState: Left (Tuple Nothing Welcome.initialState)
  }

deriveState :: State -> Connected Slice Input -> State
deriveState state { context, input } = state
  { addressBook = context.addressBook
  , subState = case context.wallet, state.subState of
      Just wallet, Right (Tuple _ ds) -> Right (Tuple wallet ds)
      Nothing, _ -> Left (Tuple Nothing Welcome.initialState)
      wallet, Left (Tuple _ ws) -> Left (Tuple wallet ws)
  , tzOffset = input.tzOffset
  }

handleQuery
  :: forall a m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => MainFrameLoop m
  => Query a
  -> HalogenM State Action ChildSlots Msg m (Maybe a)
handleQuery (ReceiveWebSocketMessage msg next) = do
  case msg of
    WS.WebSocketOpen -> do
      assign _webSocketStatus WebSocketOpen
      -- potentially renew websocket subscriptions
      mDashboardState <- peruse _dashboardState
      for_ mDashboardState \(Tuple walletDetails dashboardState) -> do
        let
          walletId = view (_walletInfo <<< _walletId) walletDetails

          followAppIds :: Array PlutusAppId
          followAppIds =
            Set.toUnfoldable $ keys $ view _contracts dashboardState
        subscribeToWallet walletId
        for followAppIds $ subscribeToPlutusApp
    (WS.WebSocketClosed closeEvent) -> do
      -- TODO: Consider whether we should show an error/warning when this happens. It might be more
      -- confusing than helpful, since the websocket is automatically reopened if it closes for any
      -- reason.
      assign _webSocketStatus (WebSocketClosed (Just closeEvent))
    (WS.ReceiveMessage (Left multipleErrors)) -> addToast $ decodingErrorToast
      "Failed to parse message from the server."
      multipleErrors
    (WS.ReceiveMessage (Right streamToClient)) -> case streamToClient of
      -- update the current slot
      -- NOTE: The PAB is currently sending this message when syncing up, and when it needs to rollback
      --       it restarts the slot count from zero, so we get thousands of calls. We should fix the PAB
      --       so that it only triggers this call once synced or ignore the message altogether and find
      --       a different approach.
      SlotChange slot -> do
        updateStore $ Store.AdvanceToSlot $ toFront slot
        -- TODO: remove currentSlot from Mainframe once the sub-components are replaced by proper components
        assign _currentSlot $ toFront slot
        -- TODO: SCP-3208 Move contract logic to global store and include AdvanceTimedoutSteps
        --       We need to have special care when modifying this function as there are some effectful
        --       computations that needs to happen in the view if the currently selected step is modified.
        handleAction $ DashboardAction Dashboard.AdvanceTimedoutSteps
      -- update the state when a contract instance changes
      -- note: we should be subscribed to updates from all (and only) the current wallet's contract
      -- instances, including its wallet companion contract
      InstanceUpdate contractInstanceId instanceStatusToClient ->
        case instanceStatusToClient of
          NewObservableState rawJson -> do
            -- TODO: If we receive a second status update for the same contract / plutus app, while
            -- the previous update is still being handled, then strange things could happen. This
            -- does not seem very likely. Still, it might be worth considering guarding against this
            -- possibility by e.g. keeping a list/array of updates and having a subscription that
            -- handles them synchronously in the order in which they arrive.
            let
              plutusAppId = toFront contractInstanceId
            mDashboardState <- peruse _dashboardState
            -- these updates should only ever be coming when we are in the Dashboard state (and if
            -- we're not, we don't care about them _)
            for_ mDashboardState \(Tuple walletDetails _) ->
              let
                walletCompanionAppId = view _companionAppId walletDetails
                marloweAppId = view _marloweAppId walletDetails
              in
                -- if this is the wallet's WalletCompanion app...
                if (plutusAppId == walletCompanionAppId) then
                  case parseDecodeJson $ unwrap rawJson of
                    Left decodingError -> addToast $ decodingErrorToast
                      "Failed to parse an update from the wallet companion."
                      decodingError
                    Right companionAppState -> do
                      -- this check shouldn't be necessary, but at the moment we are getting too many update notifications
                      -- through the PAB - so until that bug is fixed, this will have to mask it
                      updateStore
                        $ Store.NewCompanionAppStateObserved companionAppState
                      {- [Workflow 2][5] Connect a wallet -}
                      {- [Workflow 4][1] Start a contract
                    When we start a contract, our wallet will initially receive all the role tokens for that contract
                    (before they are paid out to the people we gave those roles to). And if someone else started a
                    contract and gave us a role, we will receive that role token. Either way, our `WalletCompanion` app
                    will notice, and its status will be updated to include the `MarloweParams` and initial `MarloweData`
                    of the contract in question. We can use that to start following the contract.
                    -}
                      handleAction
                        $ DashboardAction
                        $ Dashboard.UpdateFollowerApps companionAppState
                else do
                  -- if this is the wallet's MarloweApp...
                  if (plutusAppId == marloweAppId) then
                    case parseDecodeJson $ unwrap rawJson of
                      Left decodingError -> addToast $ decodingErrorToast
                        "Failed to parse an update from the marlowe controller."
                        decodingError
                      Right Nothing -> pure unit
                      Right (Just endpointResponse) -> do
                        -- The MarloweApp capability keeps track of the requests it makes to see if this
                        -- new observable state is a WS response for an action that we made. If we refresh
                        -- we get the last observable state, and if we have two tabs open we can get
                        -- a result of an action that the other tab made.
                        -- TODO: With a "significant refactor" (and the use of `MarloweApp.waitForResponse`)
                        --       we could rework the different workflows for creating a contract and apply
                        --       inputs so these toast are closer to the code that initiates the actions.
                        mEndpointResponse <- MarloweApp.onNewObservableState
                          endpointResponse
                        case mEndpointResponse of
                          Just
                            (EndpointSuccess _ (CreateResponse _)) ->
                            addToast $ successToast
                              "Contract initialised."
                          Just (EndpointSuccess _ ApplyInputsResponse) ->
                            addToast $ successToast
                              "Contract update applied."
                          Just (EndpointException _ "create" _) ->
                            addToast $
                              errorToast
                                "Failed to initialise contract."
                                Nothing
                          Just (EndpointException _ "apply-inputs" _) ->
                            addToast $
                              errorToast "Failed to update contract." Nothing
                          _ -> pure unit
                  -- otherwise this should be one of the wallet's `MarloweFollower` apps
                  else case parseDecodeJson $ unwrap rawJson of
                    Left decodingError -> addToast $ decodingErrorToast
                      "Failed to parse an update from a contract."
                      decodingError
                    Right contractHistory -> do
                      {- [Workflow 2][7] Connect a wallet -}
                      {- [Workflow 4][3] Start a contract -}
                      {- [Workflow 5][1] Move a contract forward -}
                      handleAction $ DashboardAction $ Dashboard.UpdateContract
                        plutusAppId
                        contractHistory
                      {- [Workflow 6][0] Redeem payments -}
                      handleAction $ DashboardAction $ Dashboard.RedeemPayments
                        plutusAppId
          NewActiveEndpoints activeEndpoints -> do
            mDashboardState <- peruse _dashboardState
            -- these updates should only ever be coming when we are in the Dashboard state (and if
            -- we're not, we don't care about them anyway)
            for_ mDashboardState \(Tuple walletDetails _) -> do
              let
                plutusAppId = toFront contractInstanceId

                marloweAppId = view _marloweAppId walletDetails
              when (plutusAppId == marloweAppId) $
                MarloweApp.onNewActiveEndpoints activeEndpoints
          ContractFinished _ -> pure unit
          NewYieldedExportTxs _ -> pure unit -- TODO how to handle this? What spago build --watch --clear-screenis this?
  pure $ Just next

handleQuery (MainFrameActionQuery action next) = do
  handleAction action
  pure $ Just next

-- Note [State]: Some actions belong logically in one part of the state, but
-- from the user's point of view in another. For example, the action of picking
-- up a wallet belongs logically in the MainFrame state (because it modifies
-- that state), but from the user's point of view it belongs in the Pickup
-- state (because that's the state the app is in when you perform it). To work
-- around this, we can either make our `handleAction` functions a bit awkward,
-- or our `render` functions a bit awkward. I prefer the former. Hence some
-- submodule actions (triggered straightforwardly in the submodule's `render`
-- functions) are handled by their parent module's `handleAction` function.
handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => MonadClipboard m
  => MainFrameLoop m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction Init = do
  mWalletDetails <- peruse $ _subState <<< _Left <<< _1 <<< _Just
  traverse_ (handleAction <<< EnterDashboardState) mWalletDetails

handleAction (Receive input) = do
  modify_ $ flip deriveState input
  liftEffect case input.context.wallet of
    Nothing -> removeItem walletLocalStorageKey
    Just wallet -> setItem walletLocalStorageKey $ encodeStringifyJson wallet

{- [Workflow 3][1] Disconnect a wallet
Here we move from the `Dashboard` state to the `Welcome` state. It's very straightfoward - we just
need to unsubscribe from all the apps related to the wallet that was previously connected.
-}
handleAction (EnterWelcomeState walletDetails followerApps) = do
  let
    followerAppIds :: Array PlutusAppId
    followerAppIds = Set.toUnfoldable $ keys followerApps
  unsubscribeFromWallet $ view (_walletInfo <<< _walletId) walletDetails
  unsubscribeFromPlutusApp $ view _companionAppId walletDetails
  unsubscribeFromPlutusApp $ view _marloweAppId walletDetails
  for_ followerAppIds unsubscribeFromPlutusApp
  assign _subState $ Left $ Tuple Nothing Welcome.initialState
  updateStore Store.DeactivateWallet

{- [Workflow 2][3] Connect a wallet
Here we move the app from the `Welcome` state to the `Dashboard` state. First, however, we query
the PAB to get the given wallet's `MarloweFollower` apps, and subscribe to all the relevant apps.
If the wallet has been given a role token for a new contract while the user was disconnected, they
will not yet have a `MarloweFollower` app for that contract. The business of creating and loading
these apps (and avoiding the UI bug of saying "you have no contracts" when in fact we haven't
finished loading them yet) is a bit convoluted - follow the trail of workflow comments to see how
it works.
-}
handleAction (EnterDashboardState walletDetails) = do
  ajaxFollowerApps <- getFollowerApps walletDetails
  currentSlot <- use _currentSlot
  case ajaxFollowerApps of
    Left decodedAjaxError -> addToast $ decodedAjaxErrorToast
      "Failed to load wallet contracts."
      decodedAjaxError
    Right followerApps -> do
      let
        followerAppIds :: Array PlutusAppId
        followerAppIds = Set.toUnfoldable $ keys followerApps
      subscribeToWallet $ view (_walletInfo <<< _walletId) walletDetails
      subscribeToPlutusApp $ view _companionAppId walletDetails
      subscribeToPlutusApp $ view _marloweAppId walletDetails
      for_ followerAppIds subscribeToPlutusApp
      -- We now have all the running contracts for this wallet, but if new role tokens have been
      -- given to the wallet since we last connected it, we'll need to create some more. Since
      -- we've just subscribed to this wallet's WalletCompanion app, however, the creation of new
      -- MarloweFollower apps will be triggered by the initial WebSocket status notification.
      contractNicknames <- getContractNicknames
      assign _subState
        $ Right
        $ Tuple walletDetails
        $ Dashboard.mkInitialState
            walletDetails
            followerApps
            contractNicknames
            currentSlot
      updateStore $ Store.ActivateWallet walletDetails

handleAction (WelcomeAction wa) = do
  mWelcomeState <- peruse _welcomeState
  for_ mWelcomeState \ws -> toWelcome ws $ Welcome.handleAction wa

handleAction (DashboardAction da) = do
  currentSlot <- use _currentSlot
  tzOffset <- use _tzOffset
  addressBook <- use _addressBook
  mDashboardState <- peruse _dashboardState
  for_ mDashboardState \(Tuple walletDetails ds) ->
    toDashboard ds
      $ Dashboard.handleAction
          { addressBook, currentSlot, tzOffset, walletDetails }
          da

------------------------------------------------------------
-- Note [dummyState]: In order to map a submodule whose state might not exist, we need
-- to provide a dummyState for that submodule. Halogen would use this dummyState to play
-- with if we ever tried to call one of these handlers when the submodule state does not
-- exist. In practice this should never happen.
toWelcome
  :: forall m msg slots
   . Functor m
  => Welcome.State
  -> HalogenM Welcome.State Welcome.Action slots msg m Unit
  -> HalogenM State Action slots msg m Unit
toWelcome s = mapAction WelcomeAction <<< imapState (lens getter setter)
  where
  getter = fromMaybe s <<< preview _welcomeState
  setter = flip $ set _welcomeState

toDashboard
  :: forall m msg slots
   . Functor m
  => Dashboard.State
  -> HalogenM Dashboard.State Dashboard.Action slots msg m Unit
  -> HalogenM State Action slots msg m Unit
toDashboard s = mapAction DashboardAction <<< imapState (lens getter setter)
  where
  getter = fromMaybe s <<< preview (_dashboardState <<< _2)
  setter = flip $ set (_dashboardState <<< _2)
