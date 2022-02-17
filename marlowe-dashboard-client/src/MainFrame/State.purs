module MainFrame.State (mkMainFrame, handleAction) where

import Prologue

import API.Lenses
  ( _cicContract
  , _cicCurrentState
  , _cicDefinition
  , _cicStatus
  , _observableState
  )
import Bridge (toFront)
import Capability.MainFrameLoop (class MainFrameLoop)
import Capability.Marlowe
  ( class ManageMarlowe
  , followContract
  , subscribeToPlutusApp
  , subscribeToWallet
  , unsubscribeFromPlutusApp
  , unsubscribeFromWallet
  )
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , getWallet
  , updateWallet
  )
import Capability.PAB (class ManagePAB)
import Capability.PAB as PAB
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Capability as Logger
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.AddressBook as AddressBook
import Data.Argonaut (decodeJson, stringify)
import Data.Array (filter, find) as Array
import Data.Array (mapMaybe)
import Data.Either (hush)
import Data.Foldable (for_, traverse_)
import Data.Lens (_1, assign, lens, preview, set, use, view, (^.), (^?))
import Data.Lens.Extra (peruse)
import Data.Map (Map)
import Data.Maybe (fromMaybe, maybe)
import Data.PABConnectedWallet (PABConnectedWallet, connectWallet)
import Data.PABConnectedWallet as Connected
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Wallet (SyncStatus(..), WalletDetails)
import Data.Wallet as Disconnected
import Data.WalletId (WalletId)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff)
import Env (Env, _pollingInterval)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.Extra (imapState)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Language.Marlowe.Client (EndpointResponse(..), MarloweEndpointResult(..))
import MainFrame.Lenses
  ( _addressBook
  , _currentSlot
  , _dashboardState
  , _store
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
import Marlowe.Client (ContractHistory, _chParams, getContract)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import MarloweContract (MarloweContract(..))
import Page.Contract.Types as Contract
import Page.Dashboard.State (handleAction, mkInitialState) as Dashboard
import Page.Dashboard.State (updateTotalFunds)
import Page.Dashboard.Types (Action(..), State) as Dashboard
import Page.Welcome.State (handleAction, initialState) as Welcome
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient(..)
  , ContractInstanceClientState
  , InstanceStatusToClient(..)
  )
import Store (_wallet)
import Store as Store
import Store.Contracts (emptyContractStore)
import Store.Wallet (WalletStore(..), _Connected, _connectedWallet)
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Toast.Types
  ( ajaxErrorToast
  , decodingErrorToast
  , errorToast
  , infoToast
  , successToast
  )
import Types (AjaxResponse)
import Wallet.Types (ContractActivityStatus(..))
import WebSocket.Support as WS

{-
The Marlowe Run App features are defined in the docs/use-cases/Readme.md file, and they have indices
that can be searched throughout the code.

There are two main application states: the `Welcome` state (UC-WALLET-TESTNET-1 and UC-WALLET-TESTNET-2),
and the `Dashboard` state (for the rest of the use cases). The application starts in the `Welcome` state.
Creating or restoring a wallet moves you into the `Dashboard` state; disconnecting a wallet (UC-WALLET-3)
moves you back into the `Welcome` state.
-}
mkMainFrame
  :: forall m
   . MonadAff m
  => MonadLogger String m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => Toast m
  => MonadClipboard m
  => MainFrameLoop m
  => Component Query Input Msg m
mkMainFrame =
  connect
    ( selectEq \{ addressBook, wallet, contracts } ->
        { addressBook, wallet, contracts }
    ) $
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
  { tzOffset: Minutes 0.0 -- this will be updated in deriveState
  , webSocketStatus: WebSocketClosed Nothing
  , currentSlot: zero -- this will be updated as soon as the websocket connection is working
  , subState: Left Welcome.initialState
  , store:
      { addressBook: AddressBook.empty
      , wallet: Disconnected
      , contracts: emptyContractStore
      }
  }

deriveState :: State -> Connected Slice Input -> State
deriveState state { context, input } = state
  { subState = case state.subState of
      Right ds -> Right ds
      Left ws -> Left ws
  , tzOffset = input.tzOffset
  , store =
      { addressBook: context.addressBook
      , wallet: context.wallet
      , contracts: context.contracts
      }
  }

handleQuery
  :: forall a m
   . MonadAff m
  => MonadLogger String m
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
      mWalletId <- peruse $ _store <<< _wallet <<< WalletStore._walletId
      let walletIdXDashboardState = Tuple <$> mWalletId <*> mDashboardState
      for_ walletIdXDashboardState \(Tuple walletId _dashboardState) -> do
        -- TODO: SCP-3543 Encapsultate subscribe/unsubscribe logic into a capability
        subscribeToWallet walletId
        ajaxPlutusApps <- PAB.getWalletContractInstances walletId
        case ajaxPlutusApps of
          Left _ -> pure unit
          Right plutusApps -> for_
            (Array.filter (eq Active <<< view _cicStatus) plutusApps)
            \app ->
              subscribeToPlutusApp $ view _cicContract app
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
      InstanceUpdate plutusAppId instanceStatusToClient ->
        case instanceStatusToClient of
          NewObservableState rawJson -> do
            -- TODO: If we receive a second status update for the same contract / plutus app, while
            -- the previous update is still being handled, then strange things could happen. This
            -- does not seem very likely. Still, it might be worth considering guarding against this
            -- possibility by e.g. keeping a list/array of updates and having a subscription that
            -- handles them synchronously in the order in which they arrive.
            mWallet <-
              peruse $ _store <<< _wallet <<< WalletStore._connectedWallet
            -- these updates should only ever be coming when we are in the Dashboard state (and if
            -- we're not, we don't care about them _)
            for_ mWallet \wallet ->
              let
                walletCompanionAppId = view Connected._companionAppId wallet
                marloweAppId = view Connected._marloweAppId wallet
              in
                -- if this is the wallet's WalletCompanion app...
                if (plutusAppId == walletCompanionAppId) then
                  case decodeJson rawJson of
                    Left decodingError -> addToast $ decodingErrorToast
                      "Failed to parse an update from the wallet companion."
                      decodingError
                    Right companionAppState -> do
                      -- this check shouldn't be necessary, but at the moment we are getting too many update notifications
                      -- through the PAB - so until that bug is fixed, this will have to mask it
                      updateStore
                        $ Store.NewCompanionAppStateObserved companionAppState
                      {- [UC-CONTRACT-1][X] Starting a Marlowe contract
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
                    case decodeJson rawJson of
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
                            ( EndpointSuccess _
                                (CreateResponse marloweParams)
                            ) -> do
                            {- [UC-CONTRACT-1][2] Starting a Marlowe contract
                              After the PAB endpoint finishes creating the contract, it modifies
                              its observable state with the MarloweParams of the newly created
                              contract. We take this opportunity to create a FollowerContract
                              that will give us updates on the state of the contract
                            -}
                            -- TODO: refactor into a function and co-locate this handler with
                            --       step 0.
                            mFollower <- followContract wallet
                              marloweParams
                            case mFollower of
                              Left _ -> addToast $ errorToast
                                "Can't follow the contract"
                                Nothing
                              Right (_ /\ _) -> do
                                -- FIXME-3208: swap store contract from new to running
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
                  else case decodeJson rawJson of
                    Left decodingError -> addToast $ decodingErrorToast
                      "Failed to parse an update from a contract."
                      decodingError
                    Right contractHistory -> do
                      let
                        marloweParams = view (_chParams <<< _1) contractHistory
                      {- [UC-CONTRACT-1][3] Start a contract -}
                      {- [UC-CONTRACT-3][1] Apply an input to a contract -}
                      handleAction $ DashboardAction $ Dashboard.UpdateContract
                        marloweParams
                        contractHistory
                      {- [UC-CONTRACT-4][0] Redeem payments -}
                      handleAction $ DashboardAction $ Dashboard.RedeemPayments
                        marloweParams
          NewActiveEndpoints activeEndpoints -> do
            mWallet <-
              peruse $ _store <<< _wallet <<< WalletStore._connectedWallet
            -- these updates should only ever be coming when we are in the Dashboard state (and if
            -- we're not, we don't care about them anyway)
            for_ mWallet \wallet -> do
              let
                marloweAppId = view Connected._marloweAppId wallet
              when (plutusAppId == marloweAppId) $
                MarloweApp.onNewActiveEndpoints activeEndpoints
          -- If one of the control apps closes unexpectedly we re-activate them
          ContractFinished mVal -> do
            mWallet <-
              peruse $ _store <<< _wallet <<< WalletStore._connectedWallet
            -- these updates should only ever be coming when we are in the Dashboard state (and if
            -- we're not, we don't care about them _)
            for_ mWallet \walletDetails ->
              let
                walletId = view Connected._walletId walletDetails
                walletCompanionAppId = view Connected._companionAppId
                  walletDetails
                marloweAppId = view Connected._marloweAppId walletDetails

                reActivatePlutusScript contractType = do
                  Logger.error $ "Plutus script " <> show contractType
                    <> " has closed unexpectedly: "
                    <> maybe "" stringify mVal
                  mNewAppId <- PAB.activateContract contractType walletId
                  for_ mNewAppId
                    ( updateStore <<< Store.Wallet <<<
                        Wallet.OnPlutusScriptChanged contractType
                    )
              in
                if (plutusAppId == walletCompanionAppId) then
                  reActivatePlutusScript WalletCompanion
                else if (plutusAppId == marloweAppId) then
                  reActivatePlutusScript MarloweApp
                else pure unit
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
  => MonadLogger String m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => MonadClipboard m
  => MainFrameLoop m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction Init = do
  {- [UC-WALLET-TESTNET-2][4b] Restore a testnet wallet
  This is another path for "restoring" a wallet. When we initialize the app,
  if we have some wallet details in the local storage, we try to enter the dashboard
  state with it.
  -}
  mWalletDetails <- getWallet
  traverse_
    (updateStore <<< Store.Wallet <<< WalletStore.OnConnect)
    mWalletDetails
  traverse_
    ( \w -> handleAction $ OnPoll
        (view Disconnected._syncStatus w)
        (view Disconnected._walletId w)
    )
    mWalletDetails
  pure unit

handleAction (Receive input) = do
  oldStore <- use _store
  { store } <- H.modify $ flip deriveState input
  -- Persist the wallet details so that when we Init, we can try to recover it
  updateWallet
    $ map
        ( \wallet ->
            (wallet ^. Connected._walletNickname)
              /\ (wallet ^. Connected._walletId)
              /\ (wallet ^. Connected._pubKeyHash)
              /\ (wallet ^. Connected._address)
        )
    $ store ^? _wallet <<< _connectedWallet
  let
    { contracts } = store
  -- React to changes in the wallet store
  case oldStore.wallet, store.wallet of
    Disconnected, Connecting details -> enterDashboardState details
    Connecting _, Connected connectedWallet -> do
      assign _subState $ Right $ Dashboard.mkInitialState contracts
      handleAction $ OnPoll OutOfSync $ connectedWallet ^. Connected._walletId
    Connected _, Disconnecting connectedWallet ->
      enterWelcomeState connectedWallet
    Disconnecting _, Disconnected ->
      assign _subState $ Left Welcome.initialState
    _, _ -> pure unit

handleAction (OnPoll lastSyncStatus walletId) = do
  updateTotalFunds walletId >>= traverse_ \syncStatus -> do
    case lastSyncStatus, syncStatus of
      Synchronizing _, Synchronized -> addToast $ successToast
        "Wallet backend in sync."
      OutOfSync, Synchronizing _ -> addToast $ infoToast
        "Wallet backend synchronizing..."
      _, _ -> pure unit
    updateStore $ Store.Wallet $ Wallet.OnSyncStatusChanged syncStatus
    pollingInterval <- case syncStatus of
      -- We poll more frequently when the wallet backend is synchronizing so we
      -- get more rapid feedback.
      Synchronizing _ -> pure $ Milliseconds 500.0
      _ -> asks $ view _pollingInterval
    H.liftAff $ delay pollingInterval
    newWalletId <-
      peruse $ _store <<< _wallet <<< _Connected <<< Connected._walletId
    when (Just walletId == newWalletId) do
      void $ H.subscribe $ pure $ OnPoll syncStatus walletId

handleAction (WelcomeAction wa) = do
  mWelcomeState <- peruse _welcomeState
  for_ mWelcomeState \ws -> toWelcome ws $ Welcome.handleAction wa

handleAction (DashboardAction da) = void $ runMaybeT $ do
  currentSlot <- H.lift $ use _currentSlot
  tzOffset <- H.lift $ use _tzOffset
  addressBook <- H.lift $ use _addressBook
  dashboardState <- MaybeT $ peruse _dashboardState
  wallet <- MaybeT $ peruse $ _store <<< _wallet <<< _connectedWallet
  H.lift $ toDashboard dashboardState
    $ Dashboard.handleAction { addressBook, currentSlot, tzOffset, wallet } da

{- [UC-WALLET-3][1] Disconnect a wallet
Here we move from the `Dashboard` state to the `Welcome` state. It's very straightfoward - we just
need to unsubscribe from all the apps related to the wallet that was previously connected.
-}
enterWelcomeState
  :: forall m
   . ManageMarlowe m
  => PABConnectedWallet
  -> HalogenM State Action ChildSlots Msg m Unit
enterWelcomeState walletDetails = do
  let
    walletId = view Connected._walletId walletDetails
  -- We need to unsubscribe from the wallet per se
  unsubscribeFromWallet walletId
  -- And also from the individual plutus apps that we are
  -- subscribed to.
  -- TODO: SCP-3543 Encapsultate subscribe/unsubscribe logic into a capability
  ajaxPlutusApps <- PAB.getWalletContractInstances walletId
  case ajaxPlutusApps of
    Left _ -> pure unit
    Right plutusApps -> for_ plutusApps \app ->
      unsubscribeFromPlutusApp $ view _cicContract app
  -- FIXME-3208: Make the disconnect wallet a Store global action that clears the wallet
  --             and also the running contracts.
  updateStore $ Store.Wallet $ WalletStore.OnDisconnected

{- [UC-WALLET-TESTNET-2][5] Restore a testnet wallet
Here we move the app from the `Welcome` state to the `Dashboard` state. First, however, we query
the PAB to get the given wallet's `MarloweFollower` apps, and subscribe to all the relevant apps.
If the wallet has been given a role token for a new contract while the user was disconnected, they
will not yet have a `MarloweFollower` app for that contract. The business of creating and loading
these apps (and avoiding the UI bug of saying "you have no contracts" when in fact we haven't
finished loading them yet) is a bit convoluted - follow the trail of workflow comments to see how
it works.
-}
-- FIXME-3208: Change comments
enterDashboardState
  :: forall m
   . ManagePAB m
  => MonadLogger String m
  => ManageMarlowe m
  => Toast m
  => WalletDetails
  -> HalogenM State Action ChildSlots Msg m Unit
enterDashboardState disconnectedWallet = do
  let
    walletId = view Disconnected._walletId disconnectedWallet
  ajaxPlutusApps <- PAB.getWalletContractInstances walletId
  -- TODO: Refactor with runExceptT
  case ajaxPlutusApps of
    Left ajaxError -> addToast $ ajaxErrorToast
      "Failed to access the plutus contracts."
      ajaxError
    Right plutusApps -> do
      -- Get notified of wallet messages
      subscribeToWallet $ walletId
      -- Subscribe to WalletCompanion and the MarloweApp control app
      -- we try to reutilize an active plutus contract if possible,
      -- if not we activate new ones
      ajaxCompanionContracts <- activateOrRestorePlutusCompanionContracts
        walletId
        plutusApps
      case ajaxCompanionContracts of
        Left ajaxError -> addToast $ ajaxErrorToast
          "Failed to create the companion plutus contracts."
          ajaxError
        Right { companionAppId, marloweAppId } -> do
          -- TODO: SCP-3543 Encapsultate subscribe/unsubscribe logic into a capability
          -- Get notified of new contracts
          subscribeToPlutusApp companionAppId
          -- Get notified of the results of our control app
          subscribeToPlutusApp marloweAppId
          Logger.info $ "Subscribed to companion app " <> show companionAppId
            <> " and control app "
            <> show marloweAppId
          -- Get notified on contract changes
          let
            connectedWallet = connectWallet { companionAppId, marloweAppId }
              disconnectedWallet
            followerApps = filterFollowerApps plutusApps
          currentSlot <- use _currentSlot
          for_ followerApps \(followerAppId /\ contractHistory) -> do
            subscribeToPlutusApp followerAppId
            let
              contract = getContract contractHistory
              mMetadata = _.metaData <$> findTemplate contract
            for_ mMetadata \metadata ->
              updateStore $ Store.AddFollowerContract
                currentSlot
                followerAppId
                metadata
                contractHistory
          updateStore $ Store.Wallet $ Wallet.OnConnected connectedWallet

------------------------------------------------------------
filterFollowerApps
  :: Array (ContractInstanceClientState MarloweContract)
  -> Array (PlutusAppId /\ ContractHistory)
filterFollowerApps plutusContracts =
  let
    isActiveFollowerContract cic =
      let
        definition = view _cicDefinition cic
        status = view _cicStatus cic
      in
        definition == MarloweFollower && status == Active
    idAndHistory cic =
      let
        plutusId = view _cicContract cic
        observableStateJson = view (_cicCurrentState <<< _observableState) cic
      in
        Tuple plutusId <$> (hush $ decodeJson observableStateJson)
  in
    mapMaybe idAndHistory $ Array.filter isActiveFollowerContract
      plutusContracts

activateOrRestorePlutusCompanionContracts
  :: forall m
   . ManagePAB m
  => WalletId
  -> Array (ContractInstanceClientState MarloweContract)
  -> m
       ( AjaxResponse
           { companionAppId :: PlutusAppId, marloweAppId :: PlutusAppId }
       )
activateOrRestorePlutusCompanionContracts walletId plutusContracts = runExceptT
  do
    let
      isActiveContract contractType cic =
        let
          definition = view _cicDefinition cic
          status = view _cicStatus cic
        in
          definition == contractType && status == Active

      findOrActivateContract contractType =
        -- Try to find the contract by its type
        Array.find (isActiveContract contractType) plutusContracts # case _ of
          Nothing ->
            -- If we cannot find it, activate a new one
            ExceptT $ PAB.activateContract contractType walletId
          Just contract ->
            -- If we find it, return the id
            pure $ view _cicContract contract
    { companionAppId: _, marloweAppId: _ }
      <$> findOrActivateContract WalletCompanion
      <*> findOrActivateContract MarloweApp

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
  getter = fromMaybe s <<< preview _dashboardState
  setter = flip $ set _dashboardState
