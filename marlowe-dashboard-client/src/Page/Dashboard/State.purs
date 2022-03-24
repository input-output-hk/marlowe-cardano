module Page.Dashboard.State
  ( handleAction
  , mkInitialState
  , updateTotalFunds
  ) where

import Prologue

import Bridge (toFront)
import Capability.Marlowe (class ManageMarlowe, initializeContract)
import Capability.PlutusApps.FollowerApp
  ( class FollowerApp
  , ensureFollowerContract
  , followNewContract
  )
import Capability.Toast (class Toast, addToast)
import Capability.Wallet (class ManageWallet, getWalletTotalFunds)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Types as Contacts
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Component.Template.State (dummyState, handleAction, initialState) as Template
import Component.Template.Types (Action(..), State(..)) as Template
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Logger.Structured as Logger
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk)
import Data.ContractStatus (ContractStatus(..))
import Data.ContractUserParties (contractUserParties)
import Data.DateTime.Instant (Instant)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (assign, modifying, set, use)
import Data.Map (filterKeys, toUnfoldable)
import Data.NewContract (NewContract(..))
import Data.PABConnectedWallet (PABConnectedWallet)
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
  , _contractFilter
  , _menuOpen
  , _newContracts
  , _runningContracts
  , _selectedContractIndex
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
  , getNewContracts
  , getRunningContracts
  )
import Store.Wallet as Wallet
import Store.Wallet as WalletStore
import Toast.Types (explainableErrorToast, successToast)

mkInitialState
  :: Instant -> PABConnectedWallet -> ContractStore -> State
mkInitialState currentTime wallet contracts =
  let
    runningContracts =
      deriveContractState currentTime wallet <$> getRunningContracts contracts
    closedContracts =
      deriveContractState currentTime wallet <$> getClosedContracts contracts
  in
    { walletCompanionStatus: WaitingToSync
    , menuOpen: false
    , card: Nothing
    , cardOpen: false
    , newContracts: getNewContracts contracts
    , runningContracts
    , closedContracts
    , contractFilter: Running
    , selectedContractIndex: Nothing
    , templateState: Template.dummyState
    }

deriveContractState
  :: Instant
  -> PABConnectedWallet
  -> Execution.State
  -> ContractState
deriveContractState currentTime wallet executionState =
  let
    { marloweParams, contract } = executionState
    userParties = contractUserParties wallet marloweParams contract
  in
    { executionState
    , contractUserParties: userParties
    , namedActions: userNamedActions userParties
        $ extractNamedActions currentTime executionState
    }

handleAction
  :: forall m
   . MonadAff m
  => MonadLogger StructuredLog m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => FollowerApp m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => Input
  -> Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction _ (OnContactsMsg Contacts.Closed) =
  assign _card Nothing

handleAction { currentTime, wallet, contracts } Receive = do
  let
    runningContracts = deriveContractState currentTime wallet <$>
      getRunningContracts contracts
    closedContracts = deriveContractState currentTime wallet <$>
      getClosedContracts contracts
    newContracts = getNewContracts contracts
  modify_
    ( set _runningContracts runningContracts
        <<< set _closedContracts closedContracts
        <<< set _newContracts newContracts
    )

{- [UC-WALLET-3][0] Disconnect a wallet -}
handleAction { wallet } DisconnectWallet = do
  updateStore $ Store.Wallet $ WalletStore.OnDisconnect wallet

handleAction _ ToggleMenu = modifying _menuOpen not

handleAction _ (ClipboardAction action) = do
  Clipboard.handleAction action
  addToast $ successToast "Copied to clipboard"

handleAction _ (OpenCard card) = do
  -- We set the card and reset the contact and template card states to their first section
  -- (we could check the card and only reset if relevant, but it doesn't seem worth the bother)
  modify_
    $ set _card (Just card)
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
  _selectedContractIndex
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
    ensureFollowerContract wallet marloweParams

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
    Template.OpenCreateWalletCard _ -> do
      modify_ $ set _card (Just $ ContactsCard)
    {- [UC-CONTRACT-1][0] Start a new marlowe contract
     The flow of creating a new marlowe contract starts when we submit the
     Template form. In here we apply the contract parameters to the Marlowe
     Extended contract to receive a Marlowe Core contract, and we call the
     PAB endpoint to create and distribute the role tokens. We also create
     a placeholder so the user can see that that the contract is being created
    -}
    Template.OnStartContract template params -> do
      currentInstant <- now
      instantiateResponse <- initializeContract currentInstant template params
        wallet
      case instantiateResponse of
        Left error -> do
          void $ tell _submitButtonSlot "action-pay-and-start" $
            SubmitResult (Milliseconds 600.0) (Left "Error")
          addToast $ explainableErrorToast "Failed to initialize contract."
            error
          Logger.error "Failed to initialize contract." error
        Right (newContract /\ awaitContractCreation) -> do
          handleAction input CloseCard
          void $ tell _submitButtonSlot "action-pay-and-start" $
            SubmitResult (Milliseconds 600.0) (Right "")
          addToast $ successToast
            "The request to initialize this contract has been submitted."
          -- We reset the template component (TODO: this wont be needed after the SCP-3464 refactor)
          assign _templateState Template.initialState
          marloweParams <- liftAff awaitContractCreation
          awaitContractFollow <- followNewContract wallet marloweParams
          ajaxFollow <- liftAff awaitContractFollow

          case ajaxFollow of
            Left err -> do
              addToast $ explainableErrorToast "Can't follow the contract"
                err
              Logger.error "Can't follow the contract: " err
            Right (followerId /\ history) -> do
              -- TODO: Ideally I want this updateStore to live under the FollowerApp capability
              --       or the ManageMarlowe capability, but in order to do that I need to see how
              --       feasible is to add a Co-routine.
              --       After we submit a new contract from the capability, we should yield the flow
              --       to the UI here so we can show the toast message, then we should give back control
              --       to the capability via the awaitContractCreation which should do this updateStore
              --       and then yield back here so we can do the second toast
              updateStore $
                Store.SwapStartingToStartedContract
                  newContract
                  currentInstant
                  followerId
                  history
              addToast $ successToast "Contract initialized."
              -- If the UI is showing the Starting contract we change the index to
              -- show the newly Started contract
              let
                NewContract newContractUUID _ _ = newContract
              mSelectedContract <- use _selectedContractIndex
              when (mSelectedContract == (Just $ Starting newContractUUID))
                do
                  assign _selectedContractIndex
                    $ Just
                    $ Started marloweParams
    _ -> do
      toTemplate $ Template.handleAction templateAction

-- FIXME: SCP-3468
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

toTemplate
  :: forall m msg slots
   . Functor m
  => HalogenM Template.State Template.Action slots msg m Unit
  -> HalogenM State Action slots msg m Unit
toTemplate = mapSubmodule _templateState TemplateAction
