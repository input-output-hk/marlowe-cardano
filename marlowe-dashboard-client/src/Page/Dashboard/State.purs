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
import Control.Monad.Reader (class MonadAsk)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use, (^.))
import Data.Map (filterKeys, toUnfoldable)
import Data.PABConnectedWallet (_walletId)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Data.Wallet (SyncStatus, syncStatusFromNumber)
import Data.WalletId (WalletId)
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, modify_, tell)
import Halogen.Extra (mapSubmodule)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse(..))
import Marlowe.Semantics (MarloweData, MarloweParams)
import Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _contactsState
  , _contractFilter
  , _menuOpen
  , _selectedContractMarloweParams
  , _templateState
  , _walletCompanionStatus
  )
import Page.Dashboard.Types
  ( Action(..)
  , Card(..)
  , ContractFilter(..)
  , Input
  , State
  , WalletCompanionStatus(..)
  )
import Store as Store
import Store.Contracts (ContractStore, followerContractExists)
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Toast.Types (ajaxErrorToast, errorToast, successToast)

mkInitialState
  :: ContractStore -> State
mkInitialState contractStore =
  { contactsState: Contacts.initialState
  , walletCompanionStatus: WaitingToSync
  , menuOpen: false
  , card: Nothing
  , cardOpen: false
  , contractStore
  , contractFilter: Running
  , selectedContractMarloweParams: Nothing
  , templateState: Template.dummyState
  }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarloweStorage m
  => ManageMarlowe m
  => MainFrameLoop m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => Input
  -> Action
  -> HalogenM State Action ChildSlots Msg m Unit
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

{- [UC-CONTRACT-1][2] Start a Marlowe contract
-- FIXME-3208 redo comments
After starting a new Marlowe contract, we should receive a WebSocket notification informing us of
the `MarloweParams` and initial `MarloweData` for that contract (via a status update for our
`WalletCompanion` app). We now need to start following that contract with a `MarloweFollower` app.
If we started the contract ourselves, we already created a `MarloweFollower` app as a placeholder.
So here we need to check whether there is an existing `MarloweFollower` app with the right metadata
(and no `MarloweParams`) - and potentially use that instead of creating a new one.
If someone else started the contract, and gave us a role, we will have no placeholder
`MarloweFollower` app, and so we simply create a new one and start following immediately.
-}
{- [UC-CONTRACT-2][1] Receive a role token for a marlowe contract -}
handleAction { wallet } (UpdateFollowerApps companionAppState) = do
  walletCompanionStatus <- use _walletCompanionStatus
  -- FIXME-3208: We should use Input here instead of getStore
  contracts <- _.contracts <$> getStore
  let
    contractExists marloweParams = followerContractExists marloweParams
      contracts

    newContracts = filterKeys (not contractExists) companionAppState

    newContractsArray :: Array (Tuple MarloweParams MarloweData)
    newContractsArray = toUnfoldable newContracts
  when (walletCompanionStatus == WaitingToSync) $ assign
    _walletCompanionStatus
    WalletCompanionSynced
  -- for_ newContractsArray \(marloweParams /\ marloweData) ->
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
  -- FIXME-3208
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

handleAction _ AdvanceTimedoutSteps = pure unit
{- FIXME-3208
handleAction input@{ currentSlot } AdvanceTimedoutSteps = do
  pure unit
  modifying
    ( _contracts
        <<< traversed
        <<< _Started
        <<< filtered
          ( \contract ->
              contract.executionState.mNextTimeout /= Nothing
                && contract.executionState.mNextTimeout
                  <= Just currentSlot
          )
    )
    (applyTimeout currentSlot)
  selectedContractFollowerAppId <- use _selectedContractMarloweParams
  for_ selectedContractFollowerAppId \followerAppId -> do
-- If the modification changed the currently selected step, that means the screen for the
-- contract that was changed is currently open, so we need to realign the step cards. We also
-- call the CancelConfirmation action - because if the user had the action confirmation card
-- open for an action in the current step, we want to close it (otherwise they could confirm an
-- action that is no longer possible).

when (selectedStep /= selectedStep') do
  for_ selectedStep'
    ( handleAction input <<< ContractAction followerAppId <<<
        Contract.MoveToStep
    )
  handleAction input $ ContractAction followerAppId $
    Contract.CancelConfirmation
    -}

handleAction
  input@{ currentSlot, wallet }
  (TemplateAction templateAction) =
  case templateAction of
    Template.OpenCreateWalletCard tokenName -> do
      modify_
        $ set _card (Just $ ContactsCard)
            <<< set (_contactsState <<< _cardSection)
              (NewWallet $ Just tokenName)
    {- [UC-CONTRACT-1][0] Starting a Marlowe contract
     The flow of creating a new marlowe contract starts when we submit the
     Template form. In here we apply the contract parameters to the Marlowe
     Extended contract to receive a Marlowe Core contract, and we call the
     PAB endpoint to create and distribute the role tokens. We also create
     a placeholder so the user can see that that the contract is being created
    -}
    Template.OnStartContract template params -> do
      let { nickname, roles } = params
      case instantiateExtendedContract currentSlot template params of
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
            Right reqId -> do
              -- FIXME-3208
              -- insertIntoContractNicknames followerAppId nickname
              let metaData = template.metaData
              -- We save in the store the request of a created contract with
              -- the information relevant to show a placeholder of a starting contract.
              updateStore $ Store.AddStartingContract $ reqId
                /\ nickname
                /\
                  metaData
              handleAction input CloseCard
              void $ tell _submitButtonSlot "action-pay-and-start" $
                SubmitResult (Milliseconds 600.0) (Right "")
              addToast $ successToast
                "The request to initialise this contract has been submitted."
              assign _templateState Template.initialState
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

handleAction _ (ContractAction _ _) =
  pure unit

{-
  FIXME-3208
handleAction
  input@{ wallet, currentSlot, tzOffset }
  (ContractAction marloweParams contractAction) = do
  pure unit

startedState <- peruse
  $ _contracts
      <<< at marloweParams
      <<< _Just
      <<< _Started
let
  contractInput = { currentSlot, wallet, marloweParams, tzOffset }
mContractState <- peruse $ _contract marloweParams
case contractAction of
  Contract.AskConfirmation action ->
    for_ startedState \contractState ->
      handleAction input $ OpenCard
        $ ContractActionConfirmationCard
            marloweParams
            { action
            , contractState
            , currentSlot
            , transactionFeeQuote: transactionFee
            , userNickname: wallet ^. _walletNickname
            , walletBalance: getAda $ wallet ^. _assets
            }
  Contract.CancelConfirmation -> handleAction input CloseCard
  _ -> for_ mContractState \s -> toContract
    marloweParams
    s
    (Contract.handleAction contractInput contractAction)
-}
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
