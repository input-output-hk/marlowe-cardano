module MainFrame.State (mkMainFrame, handleAction) where

import Prologue

import API.Lenses
  ( _cicContract
  , _cicCurrentState
  , _cicDefinition
  , _cicStatus
  , _observableState
  )
import Capability.MainFrameLoop (class MainFrameLoop)
import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , getWallet
  , updateWallet
  )
import Capability.PAB
  ( class ManagePAB
  , onNewActiveEndpoints
  , subscribeToPlutusApp
  , subscribeToWallet
  , unsubscribeFromPlutusApp
  , unsubscribeFromWallet
  )
import Capability.PAB (activateContract, getWalletContractInstances) as PAB
import Capability.PlutusApps.FollowerApp (class FollowerApp)
import Capability.PlutusApps.FollowerApp as FollowerApp
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Control.Alt ((<|>))
import Control.Alternative (empty)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Logger.Structured as Logger
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Now (class MonadTime, makeClock, now, timezoneOffset)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.State (class MonadState)
import Data.AddressBook as AddressBook
import Data.Argonaut (Json, decodeJson, jsonNull)
import Data.Argonaut.Decode.Aeson as D
import Data.Array (filter, find) as Array
import Data.Array (mapMaybe)
import Data.Either (hush)
import Data.Foldable (for_, traverse_)
import Data.Lens (assign, lens, preview, set, use, view, (^.), (^?))
import Data.Lens.Extra (peruse)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _companionAppId
  , _marloweAppId
  , _syncStatus
  , connectWallet
  )
import Data.PABConnectedWallet as Connected
import Data.Time.Duration (Minutes(..), Seconds(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Wallet (SyncStatus(..), WalletDetails)
import Data.Wallet as Disconnected
import Data.WalletId (WalletId)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Env
  ( Env
  , _applyInputListeners
  , _createListeners
  , _redeemListeners
  , _regularPollInterval
  , _sources
  , _syncPollInterval
  )
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.Extra (imapState)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad
  ( class MonadStore
  , emitSelected
  , getStore
  , updateStore
  )
import Halogen.Store.Select (selectEq)
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Language.Marlowe.Client
  ( ContractHistory
  , EndpointResponse(..)
  , MarloweEndpointResult(..)
  )
import MainFrame.Lenses
  ( _addressBook
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
  , Msg
  , Query(..)
  , Slice
  , State
  , WebSocketStatus(..)
  )
import MainFrame.View (render)
import Marlowe.Client (getContract, getMarloweParams)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.PAB (PlutusAppId)
import MarloweContract (MarloweContract(..))
import Page.Dashboard.State (handleAction, mkInitialState) as Dashboard
import Page.Dashboard.State (updateTotalFunds)
import Page.Dashboard.Types (Action(..), State) as Dashboard
import Page.Welcome.State (handleAction, initialState) as Welcome
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient(..)
  , ContractInstanceClientState
  )
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , InstanceStatusToClient(..)
  ) as PAB
import Store (_contracts, _wallet)
import Store as Store
import Store.Contracts (emptyContractStore)
import Store.Wallet (WalletStore(..), _Connected, _connectedWallet, _walletId)
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Toast.Types
  ( ajaxErrorToast
  , errorToast
  , explainableErrorToast
  , infoToast
  , successToast
  )
import Types (AjaxResponse)
import Wallet.Types (ContractActivityStatus(..))
import WebSocket.Support (FromSocket)
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
  => MonadLogger StructuredLog m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => MonadTime m
  => ManageMarlowe m
  => FollowerApp m
  => Toast m
  => MonadClipboard m
  => MainFrameLoop m
  => Component Query Unit Msg m
mkMainFrame =
  connect
    ( selectEq \{ addressBook, wallet, contracts, currentTime } ->
        { addressBook, wallet, contracts, currentTime }
    ) $
    mkComponent
      { initialState: deriveState emptyState
      , render: render
      , eval:
          mkEval defaultEval
            { handleQuery = handleQuery
            , handleAction = handleAction
            , receive = Just <<< Receive <<< _.context
            , initialize = Just Init
            }
      }

emptyState :: State
emptyState =
  { tzOffset: Minutes 0.0 -- this will be updated in Init
  , webSocketStatus: WebSocketClosed Nothing
  , subState: Left Welcome.initialState
  , store:
      { addressBook: AddressBook.empty
      , wallet: Disconnected
      , contracts: emptyContractStore
      , currentTime: bottom
      }
  }

deriveState :: State -> Connected Slice Unit -> State
deriveState state { context } = state
  { subState = case state.subState of
      Right ds -> Right ds
      Left ws -> Left ws
  , store = context
  }

handleQuery
  :: forall a m
   . MonadAff m
  => MonadLogger StructuredLog m
  => MonadTime m
  => MonadAsk Env m
  => ManageMarlowe m
  => FollowerApp m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => MainFrameLoop m
  => Query a
  -> HalogenM State Action ChildSlots Msg m (Maybe a)
handleQuery = case _ of
  MainFrameActionQuery action next -> do
    handleAction action
    pure $ Just next

reActivatePlutusScript
  :: forall m
   . MonadState State m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManagePAB m
  => MarloweContract
  -> Maybe Json
  -> MaybeT m Unit
reActivatePlutusScript contractType mVal = do
  walletId <- MaybeT $ peruse $ _store <<< _wallet <<< _walletId
  Logger.error
    ("Plutus script " <> show contractType <> " has closed unexpectedly")
    mVal
  newAppId <- MaybeT $ hush <$> PAB.activateContract contractType walletId
  H.lift
    $ updateStore
    $ Store.Wallet
    $ Wallet.OnPlutusScriptChanged contractType newAppId

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
  => MonadTime m
  => MonadLogger StructuredLog m
  => ManageMarlowe m
  => FollowerApp m
  => ManageMarloweStorage m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => MonadClipboard m
  => MainFrameLoop m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction Tick = updateStore <<< Store.Tick =<< now

handleAction Init = do
  subscribeToSources
  {- [UC-WALLET-TESTNET-2][4b] Restore a testnet wallet
  This is another path for "restoring" a wallet. When we initialize the app,
  if we have some wallet details in the local storage, we try to enter the dashboard
  state with it.
  -}
  mWalletDetails <- getWallet
  traverse_
    (updateStore <<< Store.Wallet <<< WalletStore.OnConnect)
    mWalletDetails
  assign _tzOffset =<< timezoneOffset

handleAction (Receive context) = do
  oldStore <- use _store
  { store } <- H.modify $ flip deriveState { context, input: unit }
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
      currentTime <- now
      assign _subState $ Right $ Dashboard.mkInitialState currentTime
        connectedWallet
        contracts
    Connected _, Disconnecting connectedWallet ->
      enterWelcomeState connectedWallet
    Disconnecting _, Disconnected ->
      assign _subState $ Left Welcome.initialState
    Connected _, Connected _ ->
      handleAction $ DashboardAction $ Dashboard.Receive
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

handleAction (WelcomeAction wa) = do
  mWelcomeState <- peruse _welcomeState
  for_ mWelcomeState \ws -> toWelcome ws $ Welcome.handleAction wa

handleAction (DashboardAction da) = void $ runMaybeT $ do
  currentTime <- now
  tzOffset <- H.lift $ use _tzOffset
  addressBook <- H.lift $ use _addressBook
  contracts <- H.lift $ use (_store <<< _contracts)
  dashboardState <- MaybeT $ peruse _dashboardState
  wallet <- MaybeT $ peruse $ _store <<< _wallet <<< _connectedWallet
  H.lift $ toDashboard dashboardState
    $ Dashboard.handleAction
        { addressBook, currentTime, tzOffset, wallet, contracts }
        da

handleAction (NewWebSocketStatus status) = do
  assign _webSocketStatus status
  case status of
    WebSocketOpen -> do
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
    -- TODO: Consider whether we should show an error/warning when this happens. It might be more
    -- confusing than helpful, since the websocket is automatically reopened if it closes for any
    -- reason.
    _ -> pure unit

handleAction (NotificationParseFailed whatFailed value error) = do
  let
    shortDescription = "Failed to parse " <> whatFailed <>
      " from websocket message"
  addToast $ explainableErrorToast shortDescription error
  Logger.error shortDescription { value, error }

handleAction (CompanionAppStateUpdated companionAppState) = do
  handleAction
    $ DashboardAction
    $ Dashboard.UpdateFollowerApps companionAppState

{- [UC-CONTRACT-1][2] Starting a Marlowe contract
  After the PAB endpoint finishes creating the contract, it modifies
  its observable state with the MarloweParams of the newly created
  contract. We take this opportunity to create a FollowerContract
  that will give us updates on the state of the contract
-}
handleAction (MarloweContractCreated reqId marloweParams) = void $ runMaybeT do
  listenersAVar <- asks $ view _createListeners
  listeners <- liftAff $ AVar.read listenersAVar
  subscription /\ listener <- hoistMaybe $ Map.lookup reqId listeners
  liftEffect do
    HS.notify listener marloweParams
    traverse_ HS.unsubscribe subscription

handleAction (InputsApplied reqId) = void $ runMaybeT $ do
  listenersAVar <- asks $ view _applyInputListeners
  listeners <- liftAff $ AVar.read listenersAVar
  subscription /\ listener <- hoistMaybe $ Map.lookup reqId listeners
  liftEffect do
    HS.notify listener unit
    traverse_ HS.unsubscribe subscription

handleAction (PaymentRedeemed reqId) = void $ runMaybeT $ do
  listenersAVar <- asks $ view _redeemListeners
  listeners <- liftAff $ AVar.read listenersAVar
  subscription /\ listener <- hoistMaybe $ Map.lookup reqId listeners
  liftEffect do
    HS.notify listener unit
    traverse_ HS.unsubscribe subscription

handleAction (CreateFailed reqId error) = void $ runMaybeT $ do
  listenersAVar <- asks $ view _createListeners
  listeners <- liftAff $ AVar.read listenersAVar
  subscription /\ _ <- hoistMaybe $ Map.lookup reqId listeners
  addToast $ errorToast "Failed to create contract" $ Just $ show error
  liftEffect $ traverse_ HS.unsubscribe subscription

handleAction (ApplyInputsFailed reqId error) = void $ runMaybeT $ do
  listenersAVar <- asks $ view _applyInputListeners
  listeners <- liftAff $ AVar.read listenersAVar
  subscription /\ _ <- hoistMaybe $ Map.lookup reqId listeners
  addToast $ errorToast "Failed to update contract" $ Just $ show error
  liftEffect $ traverse_ HS.unsubscribe subscription

handleAction (RedeemFailed reqId error) = void $ runMaybeT $ do
  listenersAVar <- asks $ view _redeemListeners
  listeners <- liftAff $ AVar.read listenersAVar
  subscription /\ _ <- hoistMaybe $ Map.lookup reqId listeners
  addToast $ errorToast "Failed to redeem payment" $ Just $ show error
  liftEffect $ traverse_ HS.unsubscribe subscription

handleAction (ContractHistoryUpdated plutusAppId contractHistory) = do
  {- [UC-CONTRACT-1][3] Start a contract -}
  {- [UC-CONTRACT-3][1] Apply an input to a contract -}
  FollowerApp.onNewObservableState plutusAppId contractHistory
  {- [UC-CONTRACT-4][0] Redeem payments -}
  let
    marloweParams = getMarloweParams contractHistory
  handleAction
    $ DashboardAction
    $ Dashboard.RedeemPayments marloweParams

handleAction (NewActiveEndpoints plutusAppId activeEndpoints) = do
  -- TODO move into main directly and remove this from the PAB capability API
  onNewActiveEndpoints plutusAppId activeEndpoints

handleAction (MarloweAppClosed mVal) = void $ runMaybeT do
  reActivatePlutusScript MarloweApp mVal

handleAction (WalletCompanionAppClosed mVal) = void $ runMaybeT do
  reActivatePlutusScript WalletCompanion mVal

{- [UC-WALLET-3][1] Disconnect a wallet
Here we move from the `Dashboard` state to the `Welcome` state. It's very straightfoward - we just
need to unsubscribe from all the apps related to the wallet that was previously connected.
-}
enterWelcomeState
  :: forall m
   . ManageMarlowe m
  => MonadStore Store.Action Store.Store m
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
  updateStore $ Store.Disconnect

{- [UC-WALLET-TESTNET-2][5] Restore a testnet wallet
Here we move the app from the `Welcome` state to the `Dashboard` state. First, however, we query
the PAB to get all the running contracts (`WalletCompanion`, `MarloweApp` and `MarloweFollower`)
and subscribe to notifications.
-}
enterDashboardState
  :: forall m
   . ManagePAB m
  => MonadLogger StructuredLog m
  => ManageMarlowe m
  => MonadStore Store.Action Store.Store m
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
          Logger.info' $ "Subscribed to companion app " <> show companionAppId
            <> " and control app "
            <> show marloweAppId
          -- Get notified on contract changes
          let
            connectedWallet = connectWallet { companionAppId, marloweAppId }
              disconnectedWallet
            followerApps = filterFollowerApps plutusApps
          for_ followerApps \(followerAppId /\ contractHistory) -> do
            subscribeToPlutusApp followerAppId
            let
              mMetadata = _.metaData <$>
                (findTemplate $ getContract contractHistory)
            for_ mMetadata \metadata ->
              updateStore $ Store.AddFollowerContract
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

        -- The FollowerContract state is a `Maybe ContractHistory`, the outer
        -- Maybe is the result of an invalid serialization. Eventually we
        -- join the Maybes and we don't care if we couldn't get the state
        -- because there wasn't one yet or because we couldn't decode it it
        mmContractHistory :: Maybe (Maybe ContractHistory)
        mmContractHistory = hush $ decodeJson observableStateJson
      in
        Tuple plutusId <$> join mmContractHistory
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

subscribeToSources
  :: forall m msg slots
   . MonadAsk Env m
  => MonadTime m
  => MonadStore Store.Action Store.Store m
  => HalogenM State Action slots msg m Unit
subscribeToSources = do
  walletInitial <- liftEffect HS.create
  walletUpdates <- liftEffect HS.create
  storeEmitter <- emitSelected $ selectEq $ preview $ _wallet <<< _Connected
  -- emitSelected does not start by emitting the current value in the store, so
  -- we need to do that ourselves. Because it also uses Refs to track the
  -- latest value, it can't be used more than once. This means we need to
  -- manually create a new emitter that can be used to "fan out" one value
  -- many.
  _ <- liftEffect $ HS.subscribe storeEmitter $ HS.notify walletUpdates.listener
  let
    wallet =
      walletInitial.emitter <|> walletUpdates.emitter :: Emitter
        (Maybe PABConnectedWallet)
  void <<< H.subscribe <<< compactEmitter =<< actionsFromSources wallet
  store <- getStore
  liftEffect
    $ HS.notify walletInitial.listener
    $ store ^? _wallet <<< _Connected

actionsFromSources
  :: forall m
   . MonadTime m
  => MonadAsk Env m
  => Emitter (Maybe PABConnectedWallet)
  -> m (Emitter (Maybe Action))
actionsFromSources wallet = do
  clock <- makeClock $ Seconds 1.0
  syncPoll <- makeClock =<< asks (view _syncPollInterval)
  regularPoll <- makeClock =<< asks (view _regularPollInterval)
  let walletPoller = switchEmitter $ pollWallet syncPoll regularPoll <$> wallet
  { pabWebsocket } <- asks $ view _sources
  pure $ actionFromWebsocket <$> wallet <*> pabWebsocket
    <|> walletPoller
    <|> Just Tick <$ clock

pollWallet
  :: Emitter Unit
  -> Emitter Unit
  -> Maybe PABConnectedWallet
  -> Emitter (Maybe Action)
pollWallet syncPoll regularPoll = case _ of
  Nothing -> empty
  Just wallet ->
    Just (OnPoll (wallet ^. _syncStatus) (wallet ^. Connected._walletId))
      <$ case wallet ^. _syncStatus of
        Synchronizing _ -> syncPoll
        _ -> regularPoll

actionFromWebsocket
  :: Maybe PABConnectedWallet
  -> FromSocket CombinedWSStreamToClient
  -> Maybe Action
actionFromWebsocket Nothing = const Nothing
actionFromWebsocket (Just wallet) = case _ of
  WS.WebSocketOpen ->
    Just $ NewWebSocketStatus WebSocketOpen
  WS.WebSocketClosed closeEvent ->
    Just $ NewWebSocketStatus $ WebSocketClosed $ Just closeEvent
  WS.ReceiveMessage (Left jsonDecodeError) ->
    Just $ NotificationParseFailed
      "websocket message"
      jsonNull
      jsonDecodeError
  WS.ReceiveMessage (Right stream) -> actionFromStream wallet stream

actionFromStream
  :: PABConnectedWallet
  -> PAB.CombinedWSStreamToClient
  -> Maybe Action
actionFromStream wallet = case _ of
  SlotChange _ -> Nothing
  -- TODO handle with lite wallet support
  -- NOTE: The PAB is currently sending this message when syncing up, and when it needs to rollback
  --       it restarts the slot count from zero, so we get thousands of calls. We should fix the PAB
  --       so that it only triggers this call once synced or ignore the message altogether and find
  --       a different approach.
  -- TODO: If we receive a second status update for the same contract / plutus app, while
  -- the previous update is still being handled, then strange things could happen. This
  -- does not seem very likely. Still, it might be worth considering guarding against this
  -- possibility by e.g. keeping a list/array of updates and having a subscription that
  -- handles them synchronously in the order in which they arrive.
  InstanceUpdate _ (PAB.NewYieldedExportTxs _) -> Nothing
  InstanceUpdate appId (PAB.NewActiveEndpoints activeEndpoints) ->
    Just $ NewActiveEndpoints appId activeEndpoints
  InstanceUpdate appId (PAB.ContractFinished message)
    | appId == companionAppId ->
        Just $ WalletCompanionAppClosed message
    | appId == marloweAppId ->
        Just $ MarloweAppClosed message
    | otherwise ->
        Just $ MarloweAppClosed message
  InstanceUpdate appId (PAB.NewObservableState state)
    | appId == companionAppId ->
        case D.decode (D.maybe D.value) state of
          Left error ->
            Just $ NotificationParseFailed
              "wallet companion state"
              state
              error
          Right (Just companionAppState) ->
            Just $ CompanionAppStateUpdated companionAppState
          _ -> Nothing
    | appId == marloweAppId -> case D.decode (D.maybe D.value) state of
        Left error ->
          Just $ NotificationParseFailed "marlowe app response" state
            error
        Right Nothing ->
          Nothing
        Right (Just (EndpointException uuid "create" error)) ->
          Just $ CreateFailed uuid error
        Right (Just (EndpointException uuid "apply-inputs-nonmerkleized" error)) ->
          Just $ ApplyInputsFailed uuid error
        Right (Just (EndpointException uuid "redeem" error)) ->
          Just $ RedeemFailed uuid error
        Right (Just (EndpointSuccess uuid (CreateResponse marloweParams))) ->
          Just $ MarloweContractCreated uuid marloweParams
        Right (Just (EndpointSuccess uuid ApplyInputsResponse)) ->
          Just $ InputsApplied uuid
        Right (Just (EndpointSuccess uuid RedeemResponse)) ->
          Just $ PaymentRedeemed uuid
        _ -> Nothing
    | otherwise -> case D.decode (D.maybe D.value) state of
        Left error ->
          Just $ NotificationParseFailed "follower app response" state
            error
        Right (Just history) ->
          Just $ ContractHistoryUpdated appId history
        Right _ -> Nothing
  where
  companionAppId = wallet ^. _companionAppId
  marloweAppId = wallet ^. _marloweAppId

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

compactEmitter
  :: forall a
   . Emitter (Maybe a)
  -> Emitter a
compactEmitter emitter = HS.makeEmitter $
  pure <<< HS.unsubscribe <=< HS.subscribe emitter <<< traverse

switchEmitter
  :: forall a
   . Emitter (Emitter a)
  -> Emitter a
switchEmitter emitter = HS.makeEmitter \push -> do
  innerSub <- Ref.new Nothing
  outerSub <- HS.subscribe emitter \emitter' -> do
    traverse_ HS.unsubscribe =<< Ref.read innerSub
    flip Ref.write innerSub <<< Just =<< HS.subscribe emitter' push
  pure do
    traverse_ HS.unsubscribe =<< Ref.read innerSub
    HS.unsubscribe outerSub
