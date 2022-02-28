module Page.Dashboard.State
  ( handleAction
  , mkInitialState
  , updateTotalFunds
  ) where

import Prologue

import Bridge (toFront)
import Capability.MainFrameLoop (class MainFrameLoop)
import Capability.Marlowe (class ManageMarlowe, createContract, followContract)
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.Toast (class Toast, addToast)
import Capability.Wallet (class ManageWallet, getWalletTotalFunds)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.AddContact.Types as AddContact
import Component.Contacts.Lenses (_cardSection)
import Component.Contacts.State (handleAction, initialState) as Contacts
import Component.Contacts.Types (Action(..), State) as Contacts
import Component.Contacts.Types (CardSection(..))
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Component.Template.State (dummyState, handleAction, initialState) as Template
import Component.Template.State (instantiateExtendedContract)
import Component.Template.Types (Action(..), State(..)) as Template
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk)
import Data.ContractUserParties (contractUserParties)
import Data.DateTime.Instant (Instant)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use, (^.))
import Data.Map (filterKeys, toUnfoldable)
import Data.Maybe (fromMaybe)
import Data.PABConnectedWallet (PABConnectedWallet, _walletId)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.UserNamedActions (userNamedActions)
import Data.Wallet (SyncStatus, syncStatusFromNumber)
import Data.WalletId (WalletId)
import Effect.Aff.Class (class MonadAff, liftAff)
import Env (Env)
import Halogen (HalogenM, modify_, tell)
import Halogen.Extra (mapSubmodule)
import Halogen.Store.Monad (class MonadStore, updateStore)
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Execution.State (extractNamedActions)
import Marlowe.Execution.Types as Execution
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse(..))
import Marlowe.Semantics (MarloweData, MarloweParams)
import Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _closedContracts
  , _contactsState
  , _contractFilter
  , _menuOpen
  , _runningContracts
  , _selectedContractMarloweParams
  , _templateState
  , _walletCompanionStatus
  )
import Page.Dashboard.Types
  ( Action(..)
  , Card(..)
  , ContractFilter(..)
  , ContractState
  , Input
  , State
  , WalletCompanionStatus(..)
  )
import Store as Store
import Store.Contracts
  ( ContractStore
  , followerContractExists
  , getClosedContracts
  , getContract
  , getRunningContracts
  )
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Toast.Types (ajaxErrorToast, errorToast, successToast)

mkInitialState
  :: Instant -> PABConnectedWallet -> ContractStore -> State
mkInitialState currentTime wallet contracts =
  let
    runningContracts = deriveContractState currentTime wallet $
      getRunningContracts contracts
    closedContracts = deriveContractState currentTime wallet $
      getClosedContracts contracts
  in
    { contactsState: Contacts.initialState
    , walletCompanionStatus: WaitingToSync
    , menuOpen: false
    , card: Nothing
    , cardOpen: false
    , runningContracts
    , closedContracts
    , contractFilter: Running
    , selectedContractMarloweParams: Nothing
    , templateState: Template.dummyState
    }

deriveContractState
  :: Instant
  -> PABConnectedWallet
  -> Array Execution.State
  -> Array ContractState
deriveContractState currentTime wallet = map \executionState ->
  let
    { marloweParams, contract } = executionState
    userParties = contractUserParties wallet marloweParams contract
  in
    { executionState
    , contractUserParties: userParties
    , namedActions: userNamedActions userParties
        $ fromMaybe []
        $ hush
        $ extractNamedActions currentTime executionState
    }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarloweStorage m
  => ManageMarlowe m
  => MainFrameLoop m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => Input
  -> Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction { currentTime, wallet, contracts } Receive = do
  let
    runningContracts = deriveContractState currentTime wallet $
      getRunningContracts contracts
    closedContracts = deriveContractState currentTime wallet $
      getClosedContracts contracts
  modify_
    ( set _runningContracts runningContracts
        <<< set _closedContracts closedContracts
    )

{- [UC-WALLET-3][0] Disconnect a wallet -}
handleAction { wallet } DisconnectWallet = do
  updateStore $ Store.Wallet $ WalletStore.OnDisconnect wallet

handleAction _ (ContactsAction contactsAction) =
  case contactsAction of
    Contacts.OnAddContactMsg (Just _) AddContact.BackClicked -> do
      assign _card $ Just ContractTemplateCard
    _ -> do
      toContacts $ Contacts.handleAction contactsAction

handleAction _ ToggleMenu = modifying _menuOpen not

handleAction _ (ClipboardAction action) = do
  Clipboard.handleAction action
  addToast $ successToast "Copied to clipboard"

handleAction { wallet } (OpenCard card) = do
  -- Most cards requires to show the assets of the current wallet, so we update the funds
  -- before opening a card.
  -- See note [polling updateTotalFunds]
  let
    walletId = wallet ^. _walletId
  _ <- updateTotalFunds walletId
  -- Then we set the card and reset the contact and template card states to their first section
  -- (we could check the card and only reset if relevant, but it doesn't seem worth the bother)
  modify_
    $ set _card (Just card)
        <<< set (_contactsState <<< _cardSection) Home
        <<< set _templateState Template.Start
  -- then we set the card to open (and close the mobile menu) in a separate `modify_`, so that the
  -- CSS transition animation works
  modify_
    $ set _cardOpen true
        <<< set _menuOpen false

handleAction _ CloseCard = assign _cardOpen false

handleAction _ (SetContractFilter contractFilter) = assign _contractFilter
  contractFilter

handleAction _ (SelectContract marloweParams) = assign
  _selectedContractMarloweParams
  marloweParams

{- [UC-CONTRACT-2][1] Receive a role token for a marlowe contract -}
handleAction { wallet, contracts } (UpdateFollowerApps companionAppState) = do
  walletCompanionStatus <- use _walletCompanionStatus
  let
    contractExists marloweParams = followerContractExists marloweParams
      contracts

    newContracts = filterKeys (not contractExists) companionAppState

    newContractsArray :: Array (Tuple MarloweParams MarloweData)
    newContractsArray = toUnfoldable newContracts
  when (walletCompanionStatus == WaitingToSync) $ assign
    _walletCompanionStatus
    WalletCompanionSynced
  for_ newContractsArray \(marloweParams /\ _) ->
    followContract wallet marloweParams

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
handleAction _ (RedeemPayments _) = pure unit
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

handleAction input@{ wallet } (TemplateAction templateAction) =
  case templateAction of
    Template.OpenCreateWalletCard tokenName -> do
      modify_
        $ set _card (Just $ ContactsCard)
            <<< set (_contactsState <<< _cardSection)
              (NewWallet $ Just tokenName)
    {- [UC-CONTRACT-1][0] Start a new marlowe contract
     The flow of creating a new marlowe contract starts when we submit the
     Template form. In here we apply the contract parameters to the Marlowe
     Extended contract to receive a Marlowe Core contract, and we call the
     PAB endpoint to create and distribute the role tokens. We also create
     a placeholder so the user can see that that the contract is being created
    -}
    Template.OnStartContract template params -> do
      let { nickname, roles } = params
      currentInstant <- now
      case instantiateExtendedContract currentInstant template params of
        Nothing -> do
          void $ tell _submitButtonSlot "action-pay-and-start" $ SubmitResult
            (Milliseconds 600.0)
            (Left "Error")
          addToast $ errorToast "Failed to instantiate contract." $ Just
            "Something went wrong when trying to instantiate a contract from this template using the parameters you specified."
        Just contract -> do
          ajaxCreateContract <- createContract wallet roles contract
          case ajaxCreateContract of
            -- TODO: make this error message more informative
            Left ajaxError -> do
              void $ tell _submitButtonSlot "action-pay-and-start" $
                SubmitResult (Milliseconds 600.0) (Left "Error")
              addToast $ ajaxErrorToast "Failed to initialise contract."
                ajaxError
            Right (reqId /\ mMarloweParams) -> do
              let metaData = template.metaData
              -- We save in the store the request of a created contract with
              -- the information relevant to show a placeholder of a starting contract.
              updateStore
                $ Store.AddStartingContract
                $ reqId /\ nickname /\ metaData
              handleAction input CloseCard
              void $ tell _submitButtonSlot "action-pay-and-start" $
                SubmitResult (Milliseconds 600.0) (Right "")
              addToast $ successToast
                "The request to initialise this contract has been submitted."
              assign _templateState Template.initialState
              marloweParams <- liftAff mMarloweParams
              ajaxFollow <- followContract wallet marloweParams
              case ajaxFollow of
                Left _ ->
                  addToast $ errorToast "Can't follow the contract" Nothing
                Right (_ /\ _) -> do
                  -- TODO: SCP-3487 swap store contract from new to running
                  addToast $ successToast "Contract initialised."
    _ -> do
      toTemplate $ Template.handleAction templateAction

-- FIXME: this all feels like a horrible hack that should be refactored.
-- This action is a bridge from the Contacts to the Template modules. It is used to create a
-- contract for a specific role during contract setup.
handleAction _ (SetContactForRole _ _) = do
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
  input@{ contracts, wallet }
  (OnAskContractActionConfirmation marloweParams action chosenNum) = do
  let
    mExecutionState = getContract marloweParams contracts
  for_ mExecutionState \executionState ->
    handleAction input
      $ OpenCard
      $ ContractActionConfirmationCard
          { action, chosenNum, executionState, wallet }

updateTotalFunds
  :: forall m
   . MonadAff m
  => ManageWallet m
  => MonadStore Store.Action Store.Store m
  => WalletId
  -> m (Maybe SyncStatus)
updateTotalFunds walletId = do
  response <- getWalletTotalFunds walletId
  hush <$> for response \(GetTotalFundsResponse { assets, sync }) -> do
    updateStore $ Store.Wallet $ Wallet.OnAssetsChanged $ toFront assets
    let syncStatus = syncStatusFromNumber sync
    updateStore $ Store.Wallet $ Wallet.OnSyncStatusChanged syncStatus
    pure syncStatus

toContacts
  :: forall m msg slots
   . Functor m
  => HalogenM Contacts.State Contacts.Action slots msg m Unit
  -> HalogenM State Action slots msg m Unit
toContacts = mapSubmodule _contactsState ContactsAction

toTemplate
  :: forall m msg slots
   . Functor m
  => HalogenM Template.State Template.Action slots msg m Unit
  -> HalogenM State Action slots msg m Unit
toTemplate = mapSubmodule _templateState TemplateAction
