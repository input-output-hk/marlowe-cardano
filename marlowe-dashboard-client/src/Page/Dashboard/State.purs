module Page.Dashboard.State (component) where

import Prologue

import API.Lenses (_cicContract)
import Capability.Marlowe (class ManageMarlowe, redeem)
import Capability.PAB
  ( class ManagePAB
  , onNewActiveEndpoints
  , subscribeToPlutusApp
  , unsubscribeFromPlutusApp
  )
import Capability.PAB
  ( activateContract
  , getWalletContractInstances
  , subscribeToPlutusApp
  ) as PAB
import Capability.PlutusApps.FollowerApp
  ( class FollowerApp
  , followContract
  , stopFollower
  )
import Capability.PlutusApps.FollowerApp as FollowerApp
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Types as Contacts
import Component.Template.Types as Template
import Component.Toast.Types (infoToast, successToast)
import Control.Alt ((<|>))
import Control.Bind (bindFlipped)
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog, error, info, info')
import Control.Logger.Structured as Logger
import Control.Monad.Fork.Class (class MonadKill)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Now (class MonadTime, timezoneOffset)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.State (class MonadState)
import Control.Parallel (parTraverse_)
import Data.Align (align)
import Data.Argonaut (Json, JsonDecodeError, encodeJson, jsonNull)
import Data.Argonaut.Decode.Aeson as D
import Data.Array (intercalate)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.BigInt.Argonaut (BigInt)
import Data.ContractStatus (ContractStatus(..))
import Data.DateTime.Instant (Instant)
import Data.Either (either)
import Data.Filterable (filter)
import Data.Foldable (foldMap, for_, traverse_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Lens (_Just, assign, modifying, set, to, use, view, (^.), (^?))
import Data.Lens.Record (prop)
import Data.LocalContractNicknames (LocalContractNicknames, getContractNickname)
import Data.Map (Map, filterKeys, toUnfoldable)
import Data.Map as Map
import Data.Map.Alignable (AlignableMap(..))
import Data.Maybe (fromMaybe, maybe)
import Data.NewContract (NewContract(..))
import Data.Newtype (under2, unwrap)
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _assets
  , _companionAppId
  , _initialFollowers
  , _marloweAppId
  , _walletId
  )
import Data.Set (Set)
import Data.Set as Set
import Data.Slot (Slot, fromPlutusSlot, zeroSlot)
import Data.Slot as Slot
import Data.These (These(..))
import Data.Time.Duration (Minutes(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.UserNamedActions (userNamedActions)
import Effect.Aff (Error, Fiber, launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, askUnliftAff, unliftAff)
import Effect.Class (liftEffect)
import Env (Env, _applyInputBus, _createBus, _redeemBus, _sources)
import Errors (globalError)
import Halogen (RefLabel(..), modify_)
import Halogen as H
import Halogen.Animation (waitForAllAnimations)
import Halogen.Component.Reactive (mkReactiveComponent)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription (Emitter, makeEmitter)
import Halogen.Subscription as HS
import Language.Marlowe.Client
  ( EndpointResponse(..)
  , MarloweEndpointResult(..)
  , _ContractHistory
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( Assets
  , MarloweData
  , MarloweParams
  , Party(..)
  , Token(..)
  , _rolesCurrency
  )
import Marlowe.Execution.State (extractNamedActions, isClosed)
import Marlowe.Execution.Types as Execution
import Marlowe.HasParties (getParties)
import Marlowe.Run.Server
  ( getApiContractsV1ByCurrencysymbolRoletokensByTokenname
  )
import Marlowe.Run.Server as MarloweRun
import MarloweContract (MarloweContract(..))
import Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _contractFilter
  , _contractStore
  , _contracts
  , _currentSlot
  , _menuOpen
  , _roleTokens
  , _selectedContractIndex
  , _tipSlot
  , _tzOffset
  , _wallet
  , _walletCompanionStatus
  )
import Page.Dashboard.Types
  ( Action(..)
  , Card(..)
  , ChildSlots
  , Component
  , ContractFilter(..)
  , ContractState
  , DerivedState
  , Msg
  , NotificationParseFailedError(..)
  , Slice
  , State
  , WalletCompanionStatus(..)
  )
import Page.Dashboard.View (render)
import Plutus.PAB.Webserver.Types (CombinedWSStreamToClient(..))
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , InstanceStatusToClient(..)
  ) as PAB
import Servant.PureScript (class MonadAjax)
import Store as Store
import Store.Contracts
  ( ContractStore
  , followerContractExists
  , getContract
  , getContractNicknames
  , getMarloweParamsForFollower
  , isFollowerContract
  , partitionContracts
  )
import Store.RoleTokens (Payout, RoleTokenStore, getEligiblePayouts)
import Store.Wallet (_connectedWallet)
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Type.Proxy (Proxy(..))
import WebSocket.Support (FromSocket)
import WebSocket.Support as WS

subscribeToPlutusApps
  :: forall m
   . ManagePAB m
  => MonadLogger StructuredLog m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => HalogenM m Unit
subscribeToPlutusApps = do
  wallet <- use _wallet
  let companionAppId = wallet ^. _companionAppId
  let marloweAppId = wallet ^. _marloweAppId
  let initialFollowers = wallet ^. _initialFollowers
  -- Register initial follower contracts before subscribing to companion so
  -- existing follower instances never get duplicated by companion updates.
  updateStore $ Store.FollowerAppsActivated initialFollowers

  -- TODO: SCP-3543 Encapsultate subscribe/unsubscribe logic into a capability
  -- Get notified of new contracts
  subscribeToPlutusApp companionAppId

  -- Get notified of the results of our control app
  subscribeToPlutusApp marloweAppId
  Logger.info' $ "Subscribed to companion app " <> show companionAppId
    <> " and control app "
    <> show marloweAppId

  -- subscribe to followers
  traverse_ (subscribeToPlutusApp <<< snd) initialFollowers
  Logger.info' $ "Subscribed to follower apps "
    <> show (Array.fromFoldable initialFollowers)

{- [UC-WALLET-3][1] Disconnect a wallet
Here we move from the `Dashboard` state to the `Welcome` state. It's very straightfoward - we just
need to unsubscribe from all the apps related to the wallet that was previously connected.
-}
unsubscribeFromPlutusApps
  :: forall m
   . ManagePAB m
  => Toast m
  => MonadLogger StructuredLog m
  => HalogenM m Unit
unsubscribeFromPlutusApps = do
  wallet <- use _wallet
  let walletId = view _walletId wallet
  -- And also from the individual plutus apps that we are
  -- subscribed to.
  -- TODO: SCP-3543 Encapsultate subscribe/unsubscribe logic into a capability
  ajaxPlutusApps <- PAB.getWalletContractInstances walletId
  case ajaxPlutusApps of
    Left err -> globalError "Can't unsubscribe from the backend notifications"
      err
    Right plutusApps -> do
      traverse_ (unsubscribeFromPlutusApp <<< view _cicContract) plutusApps

component
  :: forall m
   . MonadAff m
  => MonadAjax MarloweRun.Api m
  => MonadLogger StructuredLog m
  => MonadUnliftAff m
  => MonadAsk Env m
  => MonadTime m
  => MonadKill Error Fiber m
  => ManageMarlowe m
  => ManagePAB m
  => FollowerApp m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => Component m
component = connect sliceSelector $ mkReactiveComponent
  { deriveState
  , initialTransient:
      { walletCompanionStatus: WaitingToSync
      , menuOpen: false
      , card: Nothing
      , cardOpen: false
      , contractFilter: Running
      , selectedContractIndex: Nothing
      , tzOffset: Minutes zero
      }
  , render
  , eval:
      { initialize: do
          subscribeToSources
          subscribeToPlutusApps
          tzOffset <- timezoneOffset
          assign _tzOffset tzOffset
      , finalize: \_ -> unsubscribeFromPlutusApps
      , handleInput
      , handleAction
      }
  }
  where
  sliceSelector =
    selectEq \{ currentTime, tipSlot, currentSlot, contracts, roleTokens } ->
      { currentTime
      , tipSlot
      , currentSlot
      , contracts
      , roleTokens
      }

deriveState :: Connected Slice PABConnectedWallet -> DerivedState
deriveState { context } =
  let
    { contracts, currentTime, roleTokens } = context
    { started, starting } = partitionContracts contracts
    nicknames = getContractNicknames contracts
  in
    { newContracts: starting
    , contracts:
        deriveContractState currentTime nicknames roleTokens <$> started
    }

deriveContractState
  :: Instant
  -> LocalContractNicknames
  -> RoleTokenStore
  -> Execution.State
  -> ContractState
deriveContractState currentTime nicknames roleTokens executionState =
  { executionState
  , namedActions: userNamedActions roleTokens executionState
      $ extractNamedActions currentTime executionState
  , isClosed: isClosed executionState
  , nickname: getContractNickname executionState.marloweParams nicknames
  }

type HalogenM = H.HalogenM State Action ChildSlots Msg

handleInput
  :: forall m
   . MonadAjax MarloweRun.Api m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => MonadAff m
  => Unit
  -> Maybe State
  -> HalogenM m Unit
handleInput _ oldState = do
  let oldContracts = maybe Map.empty (view _contracts) oldState
  let oldPayouts = oldState ^. _Just <<< _roleTokens <<< to getEligiblePayouts
  let oldAssets = oldState ^. _Just <<< _wallet <<< _assets
  let oldTipSlot = maybe zeroSlot (view _tipSlot) oldState
  handlePayoutChanges oldPayouts
  handleContractChanges oldContracts
  handleAssetsChanges oldAssets
  handleTipSlotChanges oldTipSlot

handleContractChanges
  :: forall m
   . MonadAjax MarloweRun.Api m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => MonadAff m
  => Map MarloweParams ContractState
  -> HalogenM m Unit
handleContractChanges oldContracts = do
  currentContracts <- use _contracts
  let
    addedContracts = Array.fromFoldable
      $ Map.difference currentContracts oldContracts
    addedTokens = addedContracts # foldMap \{ executionState } ->
      let
        currencySymbol = executionState.marloweParams ^. _rolesCurrency
        parties = getParties executionState.contract
      in
        parties # Set.mapMaybe case _ of
          Role tokenName -> Just $ Token currencySymbol tokenName
          _ -> Nothing
  updateStore $ Store.LoadRoleTokens addedTokens
  parTraverse_ loadRoleToken addedTokens
  where
  loadRoleToken token@(Token currencySymbol tokenName) = do
    info "Loading role token" { currencySymbol, tokenName }
    result <- H.lift $ getApiContractsV1ByCurrencysymbolRoletokensByTokenname
      currencySymbol
      tokenName
    case result of
      Left err -> do
        error "Failed to load role token"
          { currencySymbol, tokenName, error: err }
        updateStore $ Store.LoadRoleTokenFailed token err
      Right roleToken -> do
        info "Role token loaded" $ encodeJson roleToken
        updateStore $ Store.RoleTokenLoaded roleToken

handlePayoutChanges
  :: forall m
   . MonadAjax MarloweRun.Api m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => MonadAff m
  => Set Payout
  -> HalogenM m Unit
handlePayoutChanges oldPayouts = do
  wallet <- use _wallet
  currentPayouts <- use (_roleTokens <<< to getEligiblePayouts)
  let newPayouts = Set.difference currentPayouts oldPayouts
  parTraverse_ (redeem wallet) newPayouts

handleAssetsChanges
  :: forall m
   . MonadAjax MarloweRun.Api m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => MonadAff m
  => Assets
  -> HalogenM m Unit
handleAssetsChanges oldAssets = do
  currentAssets <- use (_wallet <<< _assets)
  let leftAssocTuple (Tuple a (Tuple b c)) = Tuple (Tuple a b) c
  let
    -- Assets is a nested Map of Maps, which is a pain to work with here.
    -- Flatten them out by merging the inner and outer keys into a Token
    flattenAssets :: Assets -> Map Token BigInt
    flattenAssets = Map.fromFoldable
      -- left-associate the nested tuples and convert to a Token
      <<< map (lmap (uncurry Token) <<< leftAssocTuple)
      -- flatten the nested array
      <<< bindFlipped
        -- distribute the outer key into the inner array
        -- i.e. Go from Tuple a (Array (Tuple b c)) to Array (Tuple a (Tuple b c))
        ( traverse
            -- enumerate the inner key value pairs
            (Map.toUnfoldable :: _ -> Array _)
        )
      -- enumerate the outer key value pairs
      <<< (Map.toUnfoldable :: _ -> Array _)
      -- Unwrap to a Map
      <<< unwrap
  let oldFlat = flattenAssets oldAssets
  let currentFlat = flattenAssets currentAssets
  let
    diffAssetClass (This oldValue) = negate oldValue
    diffAssetClass (That newValue) = newValue
    diffAssetClass (Both oldValue newValue) = newValue - oldValue
  let
    assetsChanged = filter (notEq zero)
      $ under2 AlignableMap (align diffAssetClass) oldFlat currentFlat
  forWithIndex_ assetsChanged \(Token currencySymbol tokenName) change -> do
    let
      { assetType, changeDisplayed } =
        if currencySymbol == "" then
          { assetType: "🪙 Ada"
          , changeDisplayed:
              encodeJson $ BigInt.toNumber (unwrap change) / 1_000_000.0
          }
        else
          { assetType: "Tokens"
          , changeDisplayed:
              encodeJson $ fromMaybe 0 $ BigInt.toInt $ unwrap change
          }
      msg
        | change > zero = " added to wallet"
        | otherwise = " removed from wallet"
    info (assetType <> msg) $ encodeJson
      { currencySymbol, tokenName, change: changeDisplayed }

handleTipSlotChanges
  :: forall m
   . MonadAjax MarloweRun.Api m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => MonadAff m
  => Slot
  -> HalogenM m Unit
handleTipSlotChanges oldTipSlot = do
  currentTipSlot <- use _tipSlot
  currentSlot <- use _currentSlot
  when (currentTipSlot > oldTipSlot) do
    info' $ intercalate " "
      [ "⛓️ Chain extended, Node tip slot:"
      , Slot.format currentTipSlot
      , ", PAB current slot:"
      , Slot.format currentSlot
      , ", Sync status"
      , Slot.asFormattedPercentage currentSlot currentTipSlot
      ]

handleAction
  :: forall m
   . MonadAff m
  => MonadLogger StructuredLog m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => ManagePAB m
  => FollowerApp m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => Action
  -> HalogenM m Unit
handleAction (OnContactsMsg Contacts.Closed) =
  handleAction CloseCard

{- [UC-WALLET-3][0] Disconnect a wallet -}
handleAction (DisconnectWallet mErr) = do
  wallet <- use _wallet
  traverse_ (error "Failed to load wallet details, disconnecting.") mErr
  updateStore $ Store.Wallet $ WalletStore.OnDisconnect wallet

handleAction ToggleMenu = modifying _menuOpen not

handleAction (ClipboardAction action) = do
  Clipboard.handleAction action
  addToast $ successToast "Copied to clipboard"

handleAction (OpenCard card) = do
  modify_ $ set _card (Just card)
  -- then we set the card to open (and close the mobile menu) in a separate `modify_`, so that the
  -- CSS transition animation works
  modify_
    $ set _cardOpen true
        <<< set _menuOpen false

handleAction CloseCard = do
  assign _cardOpen false
  void
    $ H.fork
    $ H.getHTMLElementRef (RefLabel "card") >>= traverse_ \cardElement -> do
        liftAff $ waitForAllAnimations cardElement
        unlessM (use _cardOpen) do
          assign _card Nothing

handleAction (SetContractFilter contractFilter) = assign _contractFilter
  contractFilter

handleAction (SelectContract marloweParams) = assign
  _selectedContractIndex
  marloweParams

handleAction (OnTemplateMsg (Template.Closed)) = do
  handleAction CloseCard

handleAction
  (OnTemplateMsg (Template.ContractStarted newContract awaitContractCreation)) =
  do
    handleAction CloseCard
    wallet <- use _wallet
    mMarloweParams <- liftAff awaitContractCreation
    case mMarloweParams of
      Left contractError ->
        globalError "Failed to create contract" contractError
      Right marloweParams -> do
        -- If the UI is showing the Starting contract we change the index to
        -- show the newly Started contract
        let NewContract newContractUUID _ _ _ _ = newContract
        mSelectedContract <- use _selectedContractIndex
        when (mSelectedContract == (Just $ Starting newContractUUID))
          do
            assign _selectedContractIndex
              $ Just
              $ Started marloweParams
        addToast $ successToast "Contract created."
        -- Follow the contract here, because the wallet companion takes
        -- too long to respond now.
        ajaxFollow <- followContract wallet marloweParams
        case ajaxFollow of
          Left err -> globalError "Can't follow the contract" err
          Right _ -> do
            addToast $ successToast "Contract initialized."

-- FIXME: SCP-3468
handleAction (SetContactForRole _ _) = do
  pure unit
-- handleAction input $ TemplateAction Template.UpdateRoleWalletValidators
-- handleAction input
--   $ TemplateAction
--   $ Template.RoleWalletInputAction tokenName
--   $ InputField.SetValue
--   $ WN.toString walletNickname
-- -- we assign the card directly rather than calling the OpenCard action, because this action is
-- -- triggered when the ContactsCard is open, and we don't want to animate opening and closing
-- -- cards - we just want to switch instantly back to this card
-- assign _card $ Just ContractTemplateCard

handleAction
  (OnAskContractActionConfirmation marloweParams action chosenNum) = do
  contracts <- use _contractStore
  wallet <- use _wallet
  let
    mExecutionState = getContract marloweParams contracts
  for_ mExecutionState \executionState ->
    handleAction $ OpenCard $ ContractActionConfirmationCard
      { action, chosenNum, executionState, wallet }

handleAction (NotificationParseFailed error) = do
  let
    NotificationParseFailedError { whatFailed } = error
    shortDescription = "Failed to parse " <> whatFailed <>
      " from websocket message"
  globalError shortDescription error

{- [UC-CONTRACT-2][1] Receive a role token for a marlowe contract -}
handleAction (CompanionAppStateUpdated companionAppState) = do
  contracts <- use _contractStore
  wallet <- use _wallet
  walletCompanionStatus <- use _walletCompanionStatus
  let
    contractExists = flip followerContractExists contracts

    newContracts = filterKeys (not contractExists) companionAppState

    newContractsArray :: Array (Tuple MarloweParams MarloweData)
    newContractsArray = toUnfoldable newContracts
  when (walletCompanionStatus == WaitingToSync) $ assign
    _walletCompanionStatus
    WalletCompanionSynced
  flip parTraverse_ newContractsArray \(marloweParams /\ _) -> do
    ajaxFollow <- followContract wallet marloweParams
    case ajaxFollow of
      Left err -> globalError "Can't follow the contract" err
      _ -> pure unit

handleAction (ContractHistoryUpdated plutusAppId contractHistory) = do
  {- [UC-CONTRACT-1][3] Start a contract -}
  {- [UC-CONTRACT-3][1] Apply an input to a contract -}
  FollowerApp.onNewObservableState plutusAppId contractHistory
  {- [UC-CONTRACT-4][0] Redeem payments -}
  let
    marloweParams =
      view
        ( _ContractHistory
            <<< prop (Proxy :: Proxy "chParams")
        )
        $ contractHistory
    unspentPayouts = contractHistory
      ^. _ContractHistory <<< prop (Proxy :: Proxy "chUnspentPayouts")

  updateStore $ Store.NewPayoutsReceived marloweParams unspentPayouts

{- [UC-CONTRACT-4][1] Redeem payments
This action is triggered every time we receive a status update for a `MarloweFollower` app. The
handler looks, in the corresponding contract, for any payments to roles for which the current
wallet holds the token, and then calls the "redeem" endpoint of the wallet's `MarloweApp` for each
one, to make sure those funds reach the user's wallet (without the user having to do anything).
This is not very sophisticated, and results in the "redeem" endpoint being called more times than
necessary (we are not attempting to keep track of which payments have already been redeemed). Also,
we thought it would be more user-friendly for now to trigger this automatically - but when we
integrate with real wallets, I'm pretty sure we will need to provide a UI for the user to do it
manually (and sign the transaction). So this will almost certainly have to change.
-}
handleAction (RedeemPayments _) = pure unit

{-
  -- FIXME-3559 Fix redeem logic
handleAction { wallet } (RedeemPayments marloweParams) = do
 mStartedContract <- peruse $ _contracts <<< ix marloweParams <<< _Started
for_ mStartedContract \{ executionState, userParties } ->
  let
    payments = getAllPayments executionState
    { marloweParams } = executionState
    isToParty party (Payment _ payee _) = case payee of
      Party p -> p == party
      _ -> false
  in
    for (List.fromFoldable userParties) \party ->
      let
        paymentsToParty = List.filter (isToParty party) payments
      in
        for paymentsToParty \payment -> case payment of
          Payment _ (Party (Role tokenName)) _ -> void $ redeem wallet
            marloweParams
            tokenName
            _ -> pure unit
-}

{- [UC-CONTRACT-1][2] Starting a Marlowe contract
  After the PAB endpoint finishes creating the contract, it modifies
  its observable state with the MarloweParams of the newly created
  contract. We take this opportunity to create a FollowerContract
  that will give us updates on the state of the contract
-}
handleAction (MarloweContractCreated reqId marloweParams) = do
  bus <- asks $ view _createBus
  liftEffect $ EventBus.notify bus.listener reqId $ Right marloweParams

handleAction (InputsApplied reqId) = do
  bus <- asks $ view _applyInputBus
  liftEffect $ EventBus.notify bus.listener reqId $ Right unit

handleAction (PaymentRedeemed reqId) = do
  bus <- asks $ view _redeemBus
  liftEffect $ EventBus.notify bus.listener reqId $ Right unit

handleAction (CreateFailed reqId error) = void $ runMaybeT $ do
  bus <- asks $ view _createBus
  liftEffect $ EventBus.notify bus.listener reqId $ Left error

handleAction (ApplyInputsFailed reqId error) = void $ runMaybeT $ do
  bus <- asks $ view _applyInputBus
  liftEffect $ EventBus.notify bus.listener reqId $ Left error

handleAction (RedeemFailed reqId error) = void $ runMaybeT $ do
  bus <- asks $ view _redeemBus
  liftEffect $ EventBus.notify bus.listener reqId $ Left error

handleAction (NewActiveEndpoints plutusAppId activeEndpoints) = do
  -- TODO move into main directly and remove this from the PAB capability API
  onNewActiveEndpoints plutusAppId activeEndpoints

handleAction (MarloweAppClosed mVal) = void $ runMaybeT do
  reactivatePlutusScript MarloweApp mVal

handleAction (FollowerAppClosed _ marloweParams) = do
  wallet <- use _wallet
  result <- followContract wallet marloweParams
  case result of
    Left err ->
      globalError "Failed to start new contract follower" err
    Right _ -> do
      addToast $ successToast "Contract follower restarted."

handleAction (WalletCompanionAppClosed mVal) = void $ runMaybeT do
  reactivatePlutusScript WalletCompanion mVal

handleAction (UpdateWalletFunds { assets, sync }) = do
  updateStore $ Store.Wallet $ Wallet.OnAssetsChanged assets
  updateStore $ Store.Wallet $ Wallet.OnSyncStatusChanged sync

handleAction (SlotChanged slot) = do
  updateStore $ Store.SlotChanged slot

handleAction (NicknameUpdated id nickname) = do
  updateStore $ Store.ContractNicknameUpdated id nickname

handleAction (RestartFollower appId marloweParams) = do
  result <- stopFollower appId
  case result of
    Left err ->
      globalError "Failed to stop contract follower" err
    Right _ -> do
      addToast $ infoToast "Restarting contract follower..."
      handleAction $ FollowerAppClosed Nothing marloweParams

reactivatePlutusScript
  :: forall m
   . MonadState State m
  => MonadLogger StructuredLog m
  => MonadStore Store.Action Store.Store m
  => ManagePAB m
  => MarloweContract
  -> Maybe Json
  -> m Unit
reactivatePlutusScript contractType mVal = do
  walletId <- use (_wallet <<< _walletId)
  Logger.info
    ("Plutus script " <> show contractType <> " has stopped. Restarting now.")
    mVal
  result <- PAB.activateContract contractType walletId
  case result of
    Left err ->
      Logger.error
        ("Plutus script " <> show contractType <> " failed to start.")
        err
    Right instanceId -> do
      Logger.info
        ("Plutus script " <> show contractType <> " started.")
        (encodeJson instanceId)
      PAB.subscribeToPlutusApp instanceId
      updateStore
        $ Store.Wallet
        $ Wallet.OnPlutusScriptChanged contractType instanceId

subscribeToSources
  :: forall m
   . MonadAsk Env m
  => MonadUnliftAff m
  => MonadStore Store.Action Store.Store m
  => MonadLogger StructuredLog m
  => HalogenM m Unit
subscribeToSources =
  void <<< H.subscribe =<< H.lift actionsFromSources

actionsFromSources
  :: forall m
   . MonadAsk Env m
  => MonadUnliftAff m
  => MonadStore Store.Action Store.Store m
  => MonadLogger StructuredLog m
  => m (Emitter Action)
actionsFromSources = do
  { pabWebsocket, walletFunds } <- asks $ view _sources
  u <- askUnliftAff
  -- IMPORTANT: we can't use the applicative instance for Emitter here like
  -- this: actionFromWebsocket <$> wallet <*> pabWebsocket. Why? Because that
  -- will cause duplicate actions to be sent on every wallet change. In FRP
  -- terms, `wallet` functions as a `Behaviour` here, not an `Event`. But the
  -- applicative instance for `Emitters` does not work like that. It fires an
  -- event when either constituent `Emitter` fires, using the previous value of
  -- the other one each time.
  let
    websocketActions = makeEmitter \subscriber -> do
      canceller <- HS.subscribe pabWebsocket \websocketMsg -> launchAff_ do
        store <- unliftAff u $ getStore
        unliftAff u case websocketMsg of
          WS.ReceiveMessage (Left err) ->
            error "✘ Failed to parse websocket message" err
          _ -> pure unit
        liftEffect do
          let mWallet = store ^? Store._wallet <<< _connectedWallet
          for_ mWallet \wallet -> do
            let contractStore = store ^. Store._contracts
            traverse_ subscriber $ actionFromWebsocket
              contractStore
              wallet
              websocketMsg
      pure $ HS.unsubscribe canceller
  let
    walletUpdates =
      either (const $ OpenCard WalletNotFoundCard) UpdateWalletFunds <$>
        walletFunds
  -- Alt instance for Emitters "zips" them together. So the resulting Emitter
  -- is the union of events fired from the constituent emitters.
  pure $ websocketActions <|> walletUpdates

actionFromWebsocket
  :: ContractStore
  -> PABConnectedWallet
  -> FromSocket CombinedWSStreamToClient
  -> Maybe Action
actionFromWebsocket contractStore wallet = case _ of
  WS.ReceiveMessage (Left jsonDecodeError) ->
    Just $ notificationParseFailed "websocket message" jsonNull jsonDecodeError
  WS.ReceiveMessage (Right stream) ->
    actionFromStream contractStore wallet
      stream
  _ -> Nothing

actionFromStream
  :: ContractStore
  -> PABConnectedWallet
  -> PAB.CombinedWSStreamToClient
  -> Maybe Action
actionFromStream contractStore wallet = case _ of
  SlotChange { current, tip } -> do
    pure $ SlotChanged
      { current: fromPlutusSlot current, tip: fromPlutusSlot tip }
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
        case getMarloweParamsForFollower appId contractStore of
          Nothing -> Nothing
          Just marloweParams -> do
            Just $ FollowerAppClosed message marloweParams
  InstanceUpdate appId (PAB.NewObservableState state)
    | appId == companionAppId ->
        case D.decode (D.maybe D.value) state of
          Left error ->
            Just $ notificationParseFailed
              "wallet companion state"
              state
              error
          Right (Just companionAppState) ->
            Just $ CompanionAppStateUpdated companionAppState
          _ -> Nothing
    | appId == marloweAppId -> case D.decode (D.maybe D.value) state of
        Left error ->
          Just $ notificationParseFailed
            "marlowe app response"
            state
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
    | isFollowerContract appId contractStore ->
        case D.decode (D.maybe D.value) state of
          Left error ->
            Just $ notificationParseFailed
              "follower app response"
              state
              error
          Right (Just history) ->
            Just $ ContractHistoryUpdated appId history
          Right _ -> Nothing
    | otherwise -> Nothing
  where
  companionAppId = wallet ^. _companionAppId
  marloweAppId = wallet ^. _marloweAppId

notificationParseFailed
  :: String -> Json -> JsonDecodeError -> Action
notificationParseFailed whatFailed originalValue parsingError =
  NotificationParseFailed $ NotificationParseFailedError
    { whatFailed
    , originalValue
    , parsingError
    }
