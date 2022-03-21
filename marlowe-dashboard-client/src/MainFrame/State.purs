module MainFrame.State (mkMainFrame, handleAction) where

import Prologue

import API.Lenses
  ( _cicContract
  , _cicCurrentState
  , _cicDefinition
  , _cicStatus
  , _observableState
  )
import Capability.Marlowe (class ManageMarlowe)
import Capability.PAB (class ManagePAB, subscribeToPlutusApp)
import Capability.PAB (activateContract, getWalletContractInstances) as PAB
import Capability.PlutusApps.FollowerApp (class FollowerApp)
import Capability.Toast (class Toast, addToast)
import Clipboard (class MonadClipboard)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Fork.Class (class MonadKill)
import Control.Monad.Now (class MonadTime, makeClock, now, timezoneOffset)
import Control.Monad.Reader (class MonadAsk)
import Data.AddressBook as AddressBook
import Data.Argonaut (decodeJson)
import Data.Array (filter)
import Data.Array (filter, find) as Array
import Data.Either (hush)
import Data.Filterable (filterMap)
import Data.Foldable (for_, traverse_)
import Data.Lens (assign, lens, preview, set, use, view, (^.))
import Data.Lens.Extra (peruse)
import Data.Maybe (fromMaybe)
import Data.PABConnectedWallet (_syncStatus, connectWallet)
import Data.PABConnectedWallet as Connected
import Data.Set as Set
import Data.Time.Duration (Minutes(..), Seconds(..))
import Data.Wallet (SyncStatus(..), WalletDetails)
import Data.Wallet as Disconnected
import Data.WalletId (WalletId)
import Effect.Aff (Error, Fiber)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen as H
import Halogen.Extra (imapState)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import Language.Marlowe.Client (ContractHistory(..))
import MainFrame.Lenses
  ( _dashboardState
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
  , Query
  , Slice
  , State
  , WebSocketStatus(..)
  )
import MainFrame.View (render)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import MarloweContract (MarloweContract(..))
import Page.Welcome.State (handleAction, initialState) as Welcome
import Page.Welcome.Types (Action, State) as Welcome
import Plutus.PAB.Webserver.Types (ContractInstanceClientState)
import Store (_wallet)
import Store as Store
import Store.Wallet (WalletStore(..), _Connecting)
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Toast.Types (ajaxErrorToast, infoToast, successToast)
import Types (AjaxResponse)
import Wallet.Types (ContractActivityStatus(..))

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
  => MonadKill Error Fiber m
  => MonadLogger StructuredLog m
  => MonadAsk Env m
  => MonadStore Store.Action Store.Store m
  => MonadTime m
  => ManageMarlowe m
  => ManagePAB m
  => FollowerApp m
  => Toast m
  => MonadClipboard m
  => Component Query Unit Msg m
mkMainFrame =
  connect (selectEq \{ addressBook, wallet } -> { addressBook, wallet }) $
    mkComponent
      { initialState: deriveState emptyState
      , render
      , eval:
          mkEval defaultEval
            { handleAction = handleAction
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
      }
  }

deriveState :: State -> Connected Slice Unit -> State
deriveState state { context } = state
  { subState = case state.subState of
      Right ds -> Right ds
      Left ws -> Left ws
  , store = context
  }

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
  => ManagePAB m
  => ManageMarlowe m
  => FollowerApp m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => MonadClipboard m
  => Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction Tick = updateStore <<< Store.Tick =<< now

handleAction Init = do
  clock <- makeClock $ Seconds 1.0
  void $ H.subscribe $ Tick <$ clock
  assign _tzOffset =<< timezoneOffset
  wallet <- peruse $ _store <<< _wallet <<< _Connecting
  traverse_ enterDashboardState wallet

handleAction (Receive context) = do
  oldStore <- use _store
  { store: { wallet } } <-
    H.modify $ flip deriveState { context, input: unit }
  -- React to changes in the wallet store
  case oldStore.wallet, wallet of
    Disconnected, Connecting details -> enterDashboardState details
    Connecting _, Connected connectedWallet ->
      assign _subState $ Right connectedWallet
    Connected _, Disconnecting _ ->
      updateStore Store.Disconnect
    Disconnecting _, Disconnected ->
      assign _subState $ Left Welcome.initialState
    Connected oldWallet, Connected connectedWallet -> do
      let
        walletNickname = view Connected._walletNickname connectedWallet
        address = view Connected._address connectedWallet
      updateStore
        $ Store.ModifyAddressBook
        $ AddressBook.insert walletNickname address
      case oldWallet ^. _syncStatus, connectedWallet ^. _syncStatus of
        Synchronizing _, Synchronized -> addToast $ successToast
          "Wallet backend in sync."
        OutOfSync, Synchronizing _ -> addToast $ infoToast
          "Wallet backend synchronizing..."
        _, _ -> pure unit
    _, _ -> pure unit

handleAction (WelcomeAction wa) = do
  mWelcomeState <- peruse _welcomeState
  for_ mWelcomeState \ws -> toWelcome ws $ Welcome.handleAction wa

handleAction (NewWebSocketStatus status) = do
  assign _webSocketStatus status
  case status of
    WebSocketOpen -> do
      -- potentially renew websocket subscriptions
      mDashboardState <- peruse _dashboardState
      mWalletId <- peruse $ _store <<< _wallet <<< WalletStore._walletId
      let walletIdXDashboardState = Tuple <$> mWalletId <*> mDashboardState
      for_ walletIdXDashboardState \(Tuple walletId _dashboardState) -> do
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
      -- Get instances of the WalletCompanion and the MarloweApp control app.
      -- We try to reutilize an active plutus contract if possible,
      -- if not we activate new ones
      ajaxCompanionContracts <- activateOrRestorePlutusCompanionContracts
        walletId
        plutusApps
      case ajaxCompanionContracts of
        Left ajaxError -> addToast $ ajaxErrorToast
          "Failed to create the companion plutus contracts."
          ajaxError
        Right { companionAppId, marloweAppId } -> do
          let
            initialFollowers = Set.fromFoldable
              $ filterMap extractIdAndParams
              $ filter isActiveFollower plutusApps
          let
            connectedWallet = connectWallet
              { companionAppId, marloweAppId, initialFollowers }
              disconnectedWallet
          updateStore $ Store.Wallet $ Wallet.OnConnected connectedWallet

extractIdAndParams
  :: ContractInstanceClientState MarloweContract
  -> Maybe (Tuple MarloweParams PlutusAppId)
extractIdAndParams cic = do
  let instanceId = view _cicContract cic
  let observableState = view (_cicCurrentState <<< _observableState) cic
  mContractHistory <- hush $ decodeJson observableState
  ContractHistory { chParams } <- mContractHistory
  pure $ Tuple chParams instanceId

isActiveFollower :: ContractInstanceClientState MarloweContract -> Boolean
isActiveFollower cic =
  let
    definition = view _cicDefinition cic
    status = view _cicStatus cic
  in
    definition == MarloweFollower && status == Active

------------------------------------------------------------
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
