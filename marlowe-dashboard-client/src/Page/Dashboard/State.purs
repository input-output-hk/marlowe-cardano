module Page.Dashboard.State
  ( handleAction
  , mkInitialState
  , updateTotalFunds
  ) where

import Prologue

import Bridge (toFront)
import Capability.MainFrameLoop (class MainFrameLoop, callMainFrameAction)
import Capability.Marlowe
  ( class ManageMarlowe
  , createContract
  , followContract
  , followContractWithPendingFollowerApp
  , redeem
  , subscribeToPlutusApp
  )
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , insertIntoContractNicknames
  )
import Capability.Toast (class Toast, addToast)
import Capability.Wallet (class ManageWallet, getWalletTotalFunds)
import Clipboard (class MonadClipboard)
import Clipboard (handleAction) as Clipboard
import Component.Contacts.Lenses
  ( _assets
  , _cardSection
  , _walletId
  , _walletInfo
  , _walletNickname
  )
import Component.Contacts.State (getAda)
import Component.Contacts.State (handleAction, initialState) as Contacts
import Component.Contacts.Types (Action(..), State) as Contacts
import Component.Contacts.Types (CardSection(..), WalletDetails)
import Component.ContractSetupForm (ContractParams(..))
import Component.LoadingSubmitButton.Types (Query(..), _submitButtonSlot)
import Component.Template.State (dummyState, handleAction, initialState) as Template
import Component.Template.State (instantiateExtendedContract)
import Component.Template.Types (Action(..), State(..)) as Template
import Control.Monad.Reader (class MonadAsk)
import Data.Address as A
import Data.Array (null)
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as ContractNickname
import Data.Either (hush)
import Data.Filterable (filterMap)
import Data.Foldable (for_)
import Data.Lens
  ( _Just
  , assign
  , elemOf
  , filtered
  , lens
  , modifying
  , preview
  , set
  , use
  , view
  , (^.)
  )
import Data.Lens.At (at)
import Data.Lens.Extra (peruse)
import Data.Lens.Index (ix)
import Data.Lens.Traversal (traversed)
import Data.List (filter, fromFoldable) as List
import Data.Map
  ( Map
  , filterKeys
  , findMin
  , insert
  , lookup
  , mapMaybe
  , mapMaybeWithKey
  , toUnfoldable
  )
import Data.Maybe (fromMaybe)
import Data.Set (delete, fromFoldable, isEmpty) as Set
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen (HalogenM, modify_, tell)
import Halogen.Extra (imapState, mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import Halogen.Store.Monad (class MonadStore, updateStore)
import MainFrame.Types (Action(..)) as MainFrame
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Client (ContractHistory, _chHistory, _chParams)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.Execution.State (getAllPayments)
import Marlowe.PAB (PlutusAppId, transactionFee)
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse(..))
import Marlowe.Semantics
  ( MarloweData
  , MarloweParams
  , Party(..)
  , Payee(..)
  , Payment(..)
  , Slot
  , _marloweContract
  )
import Page.Contract.Lenses (_Started, _marloweParams, _selectedStep)
import Page.Contract.State (applyTimeout)
import Page.Contract.State
  ( handleAction
  , mkInitialState
  , mkPlaceholderState
  , updateState
  ) as Contract
import Page.Contract.Types (Action(..), StartingState, State(..)) as Contract
import Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _contactsState
  , _contract
  , _contractFilter
  , _contracts
  , _menuOpen
  , _selectedContract
  , _selectedContractFollowerAppId
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
import Toast.Types
  ( ajaxErrorToast
  , decodedAjaxErrorToast
  , errorToast
  , successToast
  )

{- [Workflow 2][4] Connect a wallet
When we connect a wallet, it has this initial state. Notable is the walletCompanionStatus of
`FirstUpdatePending`. Follow the trail of worflow comments to see what happens next.
-}
mkInitialState
  :: WalletDetails
  -> Map PlutusAppId ContractHistory
  -- FIXME-3208: Contract nicknames should be indexed by MarloweParams
  -> Map PlutusAppId ContractNickname
  -> Slot
  -> State
mkInitialState walletDetails contracts contractNicknames currentSlot =
  let
    mkInitialContractState followerAppId contractHistory =
      let
        nickname = fromMaybe ContractNickname.unknown $ lookup followerAppId
          contractNicknames
      in
        Contract.mkInitialState walletDetails currentSlot nickname
          contractHistory
  in
    { contactsState: Contacts.initialState
    , walletCompanionStatus: FirstUpdatePending
    , menuOpen: false
    , card: Nothing
    , cardOpen: false
    , contracts: mapMaybeWithKey mkInitialContractState contracts
    , contractFilter: Running
    , selectedContractFollowerAppId: Nothing
    , templateState: Template.dummyState
    }

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MainFrameLoop m
  => ManageMarloweStorage m
  => ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => MonadClipboard m
  => Input
  -> Action
  -> HalogenM State Action ChildSlots Msg m Unit
{- [UC-WALLET-3][0] Disconnect a wallet -}
handleAction { walletDetails } DisconnectWallet = do
  contracts <- use _contracts
  callMainFrameAction $ MainFrame.EnterWelcomeState walletDetails contracts

handleAction _ (ContactsAction contactsAction) =
  case contactsAction of
    Contacts.CancelNewContactForRole -> assign _card $ Just ContractTemplateCard
    _ -> toContacts $ Contacts.handleAction contactsAction

handleAction _ ToggleMenu = modifying _menuOpen not

handleAction _ (ClipboardAction action) = do
  Clipboard.handleAction action
  addToast $ successToast "Copied to clipboard"

handleAction { walletDetails } (OpenCard card) = do
  -- Most cards requires to show the assets of the current wallet, so we update the funds
  -- before opening a card.
  -- See note [polling updateTotalFunds]
  updateTotalFunds walletDetails
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

handleAction _ (SelectContract mFollowerAppId) = assign
  _selectedContractFollowerAppId
  mFollowerAppId

{- [Workflow 2][6] Connect a wallet
If we have just connected a wallet (and the walletCompanionStatus is still `FirstUpdatePending`),
then we know the `MarloweFollower` apps that are running in this wallet, and have just been given
the current state of its `WalletCompanion` app for the first time since connecting. If the status
of the `WalletCompanion` app contains a record of `MarloweParams` for any _new_ Marlowe contracts
(created since the last time we connected this wallet, and for which we therefore have no
corresponding `MarloweFollowr` apps), we now need to create `MarloweFollower` apps for those new
contracts.
In this case, we change the walletCompanionStatus to `LoadingNewContracts`, and subscribe to every
new `MarloweFollower`. We'll know the loading is finished when we've received the first status
update for each of these `MarloweFollower` apps through the WebSocket.
-}
{- [UC-CONTRACT-1][2] Start a Marlowe contract
After starting a new Marlowe contract, we should receive a WebSocket notification informing us of
the `MarloweParams` and initial `MarloweData` for that contract (via a status update for our
`WalletCompanion` app). We now need to start following that contract with a `MarloweFollower` app.
If we started the contract ourselves, we already created a `MarloweFollower` app as a placeholder.
So here we need to check whether there is an existing `MarloweFollower` app with the right metadata
(and no `MarloweParams`) - and potentially use that instead of creating a new one.
If someone else started the contract, and gave us a role, we will have no placeholder
`MarloweFollower` app, and so we simply create a new one and start following immediately.
-}
{- [UC-CONTRACT-2][X] Receive a role token for a marlowe contract -}
handleAction { walletDetails } (UpdateFollowerApps companionAppState) = do
  walletCompanionStatus <- use _walletCompanionStatus
  existingContracts <- use _contracts
  let
    contractExists marloweParams =
      elemOf
        (traversed <<< _Started <<< _marloweParams)
        marloweParams
        existingContracts

    newContracts = filterKeys (not contractExists) companionAppState

    newContractsArray :: Array (Tuple MarloweParams MarloweData)
    newContractsArray = toUnfoldable newContracts
  if
    ( walletCompanionStatus /= FirstUpdateComplete &&
        (not $ null newContractsArray)
    ) then
    assign _walletCompanionStatus $ LoadingNewContracts $ Set.fromFoldable $ map
      fst
      newContractsArray
  else
    assign _walletCompanionStatus FirstUpdateComplete
  void
    $ for newContractsArray \(marloweParams /\ marloweData) -> do
        let
          mTemplate = findTemplate $ view _marloweContract marloweData

          isStartingAndMetadataMatches
            :: Contract.State -> Maybe Contract.StartingState
          isStartingAndMetadataMatches = case _, mTemplate of
            Contract.Starting starting@{ metadata }, Just template
              | template.metaData == metadata -> Just starting
            _, _ -> Nothing

          mPendingContract = findMin $ mapMaybe isStartingAndMetadataMatches
            existingContracts
        ajaxFollowerApp <- case mPendingContract of
          Just { key: followerAppId } -> followContractWithPendingFollowerApp
            walletDetails
            marloweParams
            followerAppId
          Nothing -> followContract walletDetails marloweParams
        case ajaxFollowerApp of
          Left decodedAjaxError -> addToast $ decodedAjaxErrorToast
            "Failed to load new contract."
            decodedAjaxError
          Right (followerAppId /\ _) -> subscribeToPlutusApp followerAppId

{- [Workflow 2][8] Connect a wallet
If this is the first update we are receiving from a new `MarloweFollower` app that was created
after we connected the wallet, but _before_ the walletCompanionStatus was set to
`FirstUpdateComplete`, we need to remove the app from the `LoadingNewContracts` set. And if it's
the last element of that set, we can set the walletCompanionStatus to `FirstUpdateComplete`. This
(finally!) completes the workflow of connecting a wallet.
-}
{- [UC-CONTRACT-1][4] Start a contract
   [UC-CONTRACT-2][X] Receive a role token for a marlowe contract
If we started a contract (or someone else started one and gave us a role in it), we will have
created a `MarloweFollower` app for that contract, and started following the contract with that
`MarloweFollower` app. Since we will also be subscribed to that app, we will receive an update
about its initial state through the WebSocket. We potentially use that to change the corresponding
`Contract.State` from `Starting` to `Started`.
-}
{- [UC-CONTRACT-3][2] Apply an input to a contract -}
handleAction
  input@{ currentSlot, walletDetails }
  (UpdateContract followerAppId contractHistory) = do
  let
    chParams = view _chParams contractHistory
  -- Before we update the contract state we need to update the total funds to see if there has
  -- been a change in the role tokens.
  -- There is a chance that the data is not fully synced
  -- See note [polling updateTotalFunds]
  updateTotalFunds walletDetails
  -- if the chParams have not yet been set, we can't do anything; that's fine though, we'll get
  -- another notification through the websocket as soon as they are set
  for_ chParams \(marloweParams /\ marloweData) -> do
    contracts <- use _contracts
    case lookup followerAppId contracts of
      Just contractState -> do
        let
          chHistory = view _chHistory contractHistory
        selectedStep <- peruse $ _selectedContract <<< _Started <<<
          _selectedStep
        modifying _contracts $ insert followerAppId $ Contract.updateState
          walletDetails
          marloweParams
          marloweData
          currentSlot
          chHistory
          contractState
        -- if the modification changed the currently selected step, that means the card for the contract
        -- that was changed is currently open, so we need to realign the step cards
        selectedStep' <- peruse $ _selectedContract <<< _Started <<<
          _selectedStep
        when (selectedStep /= selectedStep')
          $ for_ selectedStep'
              ( handleAction input <<< ContractAction followerAppId <<<
                  Contract.MoveToStep
              )
      Nothing -> for_
        ( Contract.mkInitialState walletDetails currentSlot
            ContractNickname.unknown
            contractHistory
        )
        (modifying _contracts <<< insert followerAppId)
    -- if we're currently loading the first bunch of contracts, we can report that this one has now been loaded
    walletCompanionStatus <- use _walletCompanionStatus
    case walletCompanionStatus of
      LoadingNewContracts pendingMarloweParams -> do
        let
          updatedPendingMarloweParams = Set.delete marloweParams
            pendingMarloweParams
        if Set.isEmpty updatedPendingMarloweParams then
          assign _walletCompanionStatus FirstUpdateComplete
        else
          assign _walletCompanionStatus $ LoadingNewContracts
            updatedPendingMarloweParams
      _ -> pure unit

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
handleAction { walletDetails } (RedeemPayments followerAppId) = do
  mStartedContract <- peruse $ _contracts <<< ix followerAppId <<< _Started
  for_ mStartedContract \{ executionState, marloweParams, userParties } ->
    let
      payments = getAllPayments executionState

      isToParty party (Payment _ payee _) = case payee of
        Party p -> p == party
        _ -> false
    in
      for (List.fromFoldable userParties) \party ->
        let
          paymentsToParty = List.filter (isToParty party) payments
        in
          for paymentsToParty \payment -> case payment of
            Payment _ (Party (Role tokenName)) _ -> void $ redeem walletDetails
              marloweParams
              tokenName
            _ -> pure unit

handleAction input@{ currentSlot } AdvanceTimedoutSteps = do
  selectedStep <- peruse $ _selectedContract <<< _Started <<< _selectedStep
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
  selectedContractFollowerAppId <- use _selectedContractFollowerAppId
  for_ selectedContractFollowerAppId \followerAppId -> do
    -- If the modification changed the currently selected step, that means the screen for the
    -- contract that was changed is currently open, so we need to realign the step cards. We also
    -- call the CancelConfirmation action - because if the user had the action confirmation card
    -- open for an action in the current step, we want to close it (otherwise they could confirm an
    -- action that is no longer possible).
    selectedStep' <- peruse $ _selectedContract <<< _Started <<< _selectedStep
    when (selectedStep /= selectedStep') do
      for_ selectedStep'
        ( handleAction input <<< ContractAction followerAppId <<<
            Contract.MoveToStep
        )
      handleAction input $ ContractAction followerAppId $
        Contract.CancelConfirmation

handleAction
  input@{ currentSlot, walletDetails }
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
      let ContractParams nickname roles _ _ = params
      case instantiateExtendedContract currentSlot template params of
        Nothing -> do
          void $ tell _submitButtonSlot "action-pay-and-start" $ SubmitResult
            (Milliseconds 600.0)
            (Left "Error")
          addToast $ errorToast "Failed to instantiate contract." $ Just
            "Something went wrong when trying to instantiate a contract from this template using the parameters you specified."
        Just contract -> do
          ajaxCreateContract <-
            createContract walletDetails (A.toPubKeyHash <$> roles) contract
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

handleAction
  input@{ walletDetails, currentSlot, tzOffset }
  (ContractAction followerAppId contractAction) = do
  startedState <- peruse
    $ _contracts
        <<< at followerAppId
        <<< _Just
        <<< _Started
  let
    contractInput = { currentSlot, walletDetails, followerAppId, tzOffset }
  mContractState <- peruse $ _contract followerAppId
  case contractAction of
    Contract.AskConfirmation action ->
      for_ startedState \contractState ->
        handleAction input $ OpenCard
          $ ContractActionConfirmationCard
              followerAppId
              { action
              , contractState
              , currentSlot
              , transactionFeeQuote: transactionFee
              , userNickname: walletDetails ^. _walletNickname
              , walletBalance: getAda $ walletDetails ^. _assets
              }
    Contract.CancelConfirmation -> handleAction input CloseCard
    _ -> for_ mContractState \s -> toContract
      followerAppId
      s
      (Contract.handleAction contractInput contractAction)

updateTotalFunds
  :: forall m
   . MonadAff m
  => ManageWallet m
  => MonadStore Store.Action Store.Store m
  => WalletDetails
  -> m Unit
updateTotalFunds walletDetails = do
  let walletId = walletDetails ^. _walletInfo <<< _walletId
  response <- getWalletTotalFunds walletId
  for_ response \(GetTotalFundsResponse { assets }) ->
    updateStore $ Store.UpdateAssets $ toFront assets

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

-- see note [dummyState] in MainFrame.State
toContract
  :: forall m msg slots
   . Functor m
  => PlutusAppId
  -> Contract.State
  -> HalogenM Contract.State Contract.Action slots msg m Unit
  -> HalogenM State Action slots msg m Unit
toContract followerAppId s =
  mapAction (ContractAction followerAppId)
    <<< imapState (lens (fromMaybe s <<< preview trav) (flip (set trav)))
  where
  trav = _contract followerAppId
