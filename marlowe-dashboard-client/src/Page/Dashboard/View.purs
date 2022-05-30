module Page.Dashboard.View (render, appTemplate, appTemplateHeader) where

import Prologue hiding (Either(..), div)

import Capability.Marlowe (class ManageMarlowe)
import Capability.Toast (class Toast)
import Clipboard (class MonadClipboard)
import Clipboard (Action(..)) as Clipboard
import Component.Address.View (defaultInput, render) as Address
import Component.ConfirmContractActionDialog.State as ConfirmContractActionDialog
import Component.ConfirmContractActionDialog.Types
  ( Msg(..)
  , _confirmActionDialog
  )
import Component.Contacts.State as Contacts
import Component.Contacts.Types (_contacts)
import Component.ContractPreview.View
  ( contractPreviewCard
  , contractStartingPreviewCard
  )
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Component.Popper (Placement(..))
import Component.Template.State as Template
import Component.Tooltip.State (tooltip)
import Component.Tooltip.Types (ReferenceId(..))
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Fork.Class (class MonadKill)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadAsk)
import Css as Css
import Data.Address as A
import Data.Array (concat)
import Data.Array as Array
import Data.Compactable (compact)
import Data.ContractNickname as CN
import Data.ContractStatus (ContractStatus(..))
import Data.DateTime.Instant (Instant)
import Data.Int (round)
import Data.Lens (folded, toArrayOf, view, (^.))
import Data.Map as Map
import Data.Maybe (isJust)
import Data.NewContract (getContractNickname)
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _address
  , _assets
  , _syncStatus
  , _walletNickname
  )
import Data.String (take)
import Data.UUID.Argonaut as UUID
import Data.Wallet (SyncStatus(..))
import Data.WalletNickname as WN
import Effect.Aff (Error, Fiber)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Exception.Unsafe (unsafeThrow)
import Env (Env)
import Halogen (RefLabel(..))
import Halogen.Css (applyWhen, classNames)
import Halogen.HTML
  ( HTML
  , a
  , button
  , div
  , div_
  , footer
  , h2
  , h3
  , h4
  , header
  , img
  , main
  , nav
  , p
  , p_
  , slot
  , span
  , span_
  , text
  , ul
  )
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (href, id, ref, src, title)
import Halogen.HTML.Properties.ARIA (labelledBy, role)
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Store.Monad (class MonadStore)
import Humanize (humanizeValue)
import Images (marloweRunNavLogo, marloweRunNavLogoDark)
import Marlowe.Semantics (PubKey, _rolesCurrency, adaToken, getAda)
import Page.Contract.State as ContractPage
import Page.Contract.Types (Msg(..), _contractPage)
import Page.Dashboard.Lenses
  ( _card
  , _cardOpen
  , _closedContracts
  , _contractFilter
  , _contracts
  , _currentTime
  , _menuOpen
  , _newContracts
  , _runningContracts
  , _selectedContractIndex
  , _wallet
  , _walletCompanionStatus
  )
import Page.Dashboard.Types
  ( Action(..)
  , Card(..)
  , ChildSlots
  , ContractFilter(..)
  , State
  , WalletCompanionStatus(..)
  , _template
  )
import Store as Store

type ComponentHTML m = HH.ComponentHTML Action ChildSlots m

render
  :: forall m
   . MonadUnliftAff m
  => MonadKill Error Fiber m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => MonadLogger StructuredLog m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => MonadClipboard m
  => State
  -> ComponentHTML m
render state = HH.div [ classNames [ "h-full" ] ]
  [ dashboardScreen state
  , dashboardCard state
  ]

appTemplate :: forall w i. Boolean -> HH.HTML w i -> HH.HTML w i -> HH.HTML w i
appTemplate cardOpen header body =
  div
    [ classNames
        $
          [ "h-full"
          , "grid"
          , "grid-rows-auto-1fr-auto"
          , "transition-all"
          , "duration-500"
          , "overflow-x-hidden"
          ]
            <> applyWhen cardOpen [ "lg:mr-sidebar" ]
    ]
    [ header, body, dashboardFooter ]

dashboardScreen
  :: forall m
   . MonadAff m
  => MonadKill Error Fiber m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => MonadClipboard m
  => State
  -> ComponentHTML m
dashboardScreen state =
  let
    currentTime = state ^. _currentTime
    wallet = state ^. _wallet
    walletNickname = wallet ^. _walletNickname
    menuOpen = state ^. _menuOpen
    cardOpen = state ^. _cardOpen
    mSelectedContractIndex = state ^. _selectedContractIndex
    mSelectedContractStringId = do
      contractIndex <- mSelectedContractIndex
      pure case contractIndex of
        Starting reqId ->
          let
            newContracts = state ^. _newContracts
            mContract = Map.lookup reqId newContracts
          in
            case getContractNickname <$> mContract of
              Just nickname -> CN.toString nickname
              Nothing -> UUID.toString reqId
        Started marloweParams ->
          let
            contracts = state ^. _contracts
            mContract = Map.lookup marloweParams contracts
          in
            case _.nickname =<< mContract of
              Just nickname -> CN.toString nickname
              Nothing -> marloweParams ^. _rolesCurrency
  in
    appTemplate cardOpen (dashboardHeader (WN.toString walletNickname) menuOpen)
      $ div
          [ classNames [ "relative" ] ] -- this wrapper is relative because the mobile menu is absolutely positioned inside it
          [ mobileMenu menuOpen
          , div [ classNames [ "h-full", "grid", "grid-rows-auto-1fr" ] ]
              [ dashboardBreadcrumb mSelectedContractStringId
              , main
                  [ classNames [ "relative" ] ]
                  case mSelectedContractIndex of
                    Just contractIndex ->
                      [ slot
                          _contractPage
                          unit
                          ContractPage.component
                          { contractIndex }
                          ( \(AskConfirmation namedAction num) ->
                              case contractIndex of
                                -- This should never happen (famous last words)
                                Starting _ -> unsafeThrow
                                  "Cant fire a confirmation dialog to take an action for a starting contract"
                                Started marloweParams ->
                                  OnAskContractActionConfirmation
                                    marloweParams
                                    namedAction
                                    num
                          )
                      ]
                    _ -> [ contractsScreen currentTime state ]
              ]
          ]

dashboardCard
  :: forall m
   . MonadClipboard m
  => MonadUnliftAff m
  => MonadKill Error Fiber m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => MonadLogger StructuredLog m
  => MonadClipboard m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML m
dashboardCard state =
  let
    wallet = state ^. _wallet
    cardOpen = state ^. _cardOpen
    templateInput = wallet
  in
    div
      [ ref $ RefLabel "card", classNames $ Css.sidebarCardOverlay cardOpen ]
      [ div
          [ classNames $ Css.sidebarCard cardOpen, role "dialog" ]
          $ concat
              [ case state ^. _card of
                  Just WalletNotFoundCard -> []
                  _ ->
                    [ a
                        [ classNames [ "absolute", "top-4", "right-4" ]
                        , onClick_ CloseCard
                        ]
                        [ icon_ Icon.Close ]
                    ]
              , pure case state ^. _card of
                  Just TutorialsCard -> tutorialsCard
                  Just CurrentWalletCard -> currentWalletCard wallet
                  Just ContactsCard -> HH.slot
                    _contacts
                    unit
                    Contacts.component
                    wallet
                    OnContactsMsg
                  Just ContractTemplateCard -> slot
                    _template
                    unit
                    Template.component
                    templateInput
                    OnTemplateMsg
                  Just (ContractActionConfirmationCard input) ->
                    slot
                      _confirmActionDialog
                      unit
                      ConfirmContractActionDialog.component
                      input
                      (\DialogClosed -> CloseCard)
                  Just WalletNotFoundCard -> walletNotFoundCard
                  Nothing -> HH.text ""
              ]
      ]

------------------------------------------------------------
appTemplateHeader
  :: forall w i. Maybe i -> Boolean -> Array (HH.HTML w i) -> HH.HTML w i
appTemplateHeader onLogoClick menuOpen navItems =
  header
    [ classNames
        $ [ "relative", "border-gray", "transition-colors", "duration-200" ]
            <>
              if menuOpen then
                [ "border-0"
                , "bg-black"
                , "text-white"
                , "md:border-b"
                , "md:bg-transparent"
                , "md:text-black"
                ]
              else [ "border-b", "text-black" ]
    -- ^ in case the menu is open when the user makes their window wider, we make sure the menuOpen styles only apply on small screens ...
    ]
    [ div
        [ classNames $ Css.maxWidthContainer <>
            [ "flex"
            , "justify-between"
            , "items-center"
            , "leading-none"
            , "py-3"
            , "md:py-1"
            ]
        ]
        [ a
            (compact [ onClick_ <$> onLogoClick ])
            [ img
                [ classNames [ "w-16", "md:hidden" ]
                , src
                    if menuOpen then marloweRunNavLogoDark
                    else marloweRunNavLogo
                ]
            -- ... and provide an alternative logo for wider screens that always has black text
            , img
                [ classNames [ "w-16", "hidden", "md:inline" ]
                , src marloweRunNavLogo
                ]
            ]
        , nav [ classNames [ "flex", "items-center" ] ] navItems
        ]
    ]

dashboardHeader
  :: forall m
   . MonadAff m
  => PubKey
  -> Boolean
  -> ComponentHTML m
dashboardHeader walletNickname menuOpen =
  appTemplateHeader (Just $ SelectContract Nothing) menuOpen
    [ navigation
        (OpenCard ContactsCard)
        Icon.Contacts
        "contactsHeader"
        "Contacts"
    , navigation
        (OpenCard TutorialsCard)
        Icon.Tutorials
        "tutorialsHeader"
        "Tutorials"
    , a
        [ classNames [ "ml-6", "font-bold", "text-sm" ]
        , id "currentWalletHeader"
        , onClick_ $ OpenCard CurrentWalletCard
        , href "#"
        , ARIA.label "My wallet"
        ]
        [ span
            [ classNames $
                [ "flex"
                , "items-baseline"
                , "gap-2"
                , "bg-white"
                , "rounded-lg"
                , "p-4"
                , "leading-none"
                ]
            ]
            [ span
                [ classNames $
                    [ "-m-1"
                    , "rounded-full"
                    , "text-white"
                    , "w-5"
                    , "h-5"
                    , "flex"
                    , "justify-center"
                    , "items-center"
                    , "uppercase"
                    , "font-semibold"
                    ] <> Css.bgBlueGradient
                ]
                [ text $ take 1 walletNickname ]
            , span
                [ classNames
                    [ "hidden", "md:inline", "truncate", "max-w-16" ]
                ]
                [ text walletNickname ]
            ]
        ]
    , tooltip "My wallet" (RefId "currentWalletHeader") Bottom
    , a
        [ classNames [ "ml-4", "md:hidden" ]
        , onClick_ ToggleMenu
        ]
        [ if menuOpen then icon_ Icon.Close else icon_ Icon.Menu ]
    ]
  where
  navigation action icon' refId label =
    div_
      [ a
          [ classNames [ "ml-6", "font-bold", "text-sm" ]
          , id refId
          , href "#"
          , onClick_ action
          , ARIA.label label
          ]
          [ icon_ icon' ]
      , tooltip label (RefId refId) Bottom
      ]

mobileMenu :: forall p. Boolean -> HTML p Action
mobileMenu menuOpen =
  nav
    [ classNames
        $
          [ "md:hidden"
          , "absolute"
          , "inset-0"
          , "z-30"
          , "bg-black"
          , "text-white"
          , "text-lg"
          , "overflow-auto"
          , "flex"
          , "flex-col"
          , "justify-between"
          , "pt-8"
          , "pb-4"
          , "transition-all"
          , "duration-200"
          ]
            <>
              if menuOpen then [ "opacity-100" ]
              else [ "opacity-0", "pointer-events-none" ]
    ]
    [ div
        [ classNames [ "flex", "flex-col" ] ]
        dashboardLinks
    , div
        [ classNames [ "flex", "flex-col" ] ]
        iohkLinks
    ]

dashboardBreadcrumb :: forall m. MonadAff m => Maybe String -> ComponentHTML m
dashboardBreadcrumb mSelectedContractStringId =
  div [ classNames [ "border-b", "border-gray" ] ]
    [ nav [ classNames $ Css.maxWidthContainer <> [ "flex", "gap-2", "py-2" ] ]
        $
          [ a
              [ id "goToDashboard"
              , onClick $ const $ SelectContract Nothing
              , href "#"
              , classNames
                  if (isJust mSelectedContractStringId) then
                    [ "text-lightpurple", "font-bold" ]
                  else
                    [ "cursor-default" ]
              ]
              [ text "Dashboard" ]
          ]
            <> case mSelectedContractStringId of
              Just nickname ->
                [ icon_ Icon.Next
                , tooltip "Go to dashboard" (RefId "goToDashboard") Bottom
                , span_
                    [ text nickname
                    ]
                ]
              Nothing -> []
    ]

dashboardFooter :: forall w i. HTML w i
dashboardFooter =
  footer
    [ classNames [ "hidden", "md:block", "border-t", "border-gray" ] ]
    [ div
        [ classNames $ Css.maxWidthContainer <>
            [ "flex", "justify-between", "py-2", "text-sm" ]
        ]
        [ nav
            [ classNames [ "flex", "-ml-4" ] ] -- -ml-4 to offset the padding of the first link
            dashboardLinks
        , nav
            [ classNames [ "flex", "-mr-4" ] ] -- -mr-4 to offset the padding of the last link
            iohkLinks
        ]
    ]

dashboardLinks :: forall w i. Array (HTML w i)
dashboardLinks =
  -- FIXME: SCP-2589 Add link to Docs
  [ link "Docs" ""
  , link "marlowe-finance.io" "https://marlowe-finance.io"
  , link "play.marlowe-finance.io" "https://play.marlowe-finance.io"
  {- disabled for phase 1, link "Market" ""
  , link "Support" "" -}
  ]

iohkLinks :: forall w i. Array (HTML w i)
iohkLinks =
  [ link "cardano.org" "https://cardano.org"
  , link "iohk.io" "https://iohk.io"
  ]

link :: forall w i. String -> String -> HTML w i
link label url =
  a
    [ classNames [ "px-4", "py-2", "font-bold", "cursor-pointer" ]
    , href url
    ]
    [ text label ]

------------------------------------------------------------
contractsScreen
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Instant
  -> State
  -> ComponentHTML m
contractsScreen currentTime state =
  let
    contractFilter = view _contractFilter state
  in
    -- This convoluted combination of absolute and relative elements is the only way I could find
    -- to get the contract navigation element to be have a fixed position relative to a max-width
    -- container, at the same time as having the vertical scroll appear to be on the whole div.
    -- If not for the scroll, it could have just had position absolute (with a relative parent).
    -- And if not for the max width container, it could have just had position fixed.
    div
      [ classNames [ "h-full", "relative" ] ]
      [ div
          -- overflow-x here can occur when the sidebar is open
          [ classNames
              [ "absolute"
              , "z-10"
              , "inset-0"
              , "h-full"
              , "overflow-y-auto"
              , "overflow-x-hidden"
              ]
          ]
          [ div
              [ classNames $ Css.maxWidthContainer <> [ "relative", "h-full" ] ]
              [ contractCards currentTime state ]
          ]
      , div
          [ classNames [ "absolute", "inset-0" ] ]
          [ div
              [ classNames $ Css.maxWidthContainer <> [ "relative", "h-full" ] ]
              [ contractNavigation contractFilter ]
          ]
      ]

contractNavigation :: forall m. MonadAff m => ContractFilter -> ComponentHTML m
contractNavigation contractFilter =
  let
    navClasses =
      [ "inline-flex"
      , "gap-4"
      , "overflow-hidden"
      , "px-3"
      , "lg:px-0"
      , "lg:py-3"
      , "lg:flex-col"
      , "bg-white"
      , "rounded"
      , "shadow"
      ]

    navItemClasses active =
      [ "leading-none"
      , "pt-2+2px"
      , "pb-2"
      , "border-b-2"
      , "lg:py-0"
      , "lg:pr-2+2px"
      , "lg:pl-2"
      , "lg:border-b-0"
      , "lg:border-l-2"
      , "border-transparent"
      ] <> applyWhen active [ "border-black" ]
  in
    div
      [ classNames
          [ "absolute"
          , "z-20"
          , "left-4"
          , "bottom-4"
          , "right-4"
          , "lg:right-auto"
          , "lg:top-0"
          , "grid"
          , "grid-cols-1fr-auto-1fr"
          , "lg:grid-cols-none"
          , "lg:grid-rows-1fr-auto-1fr"
          ]
      ]
      [ div
          [ classNames
              [ "row-start-1"
              , "col-start-2"
              , "lg:row-start-2"
              , "lg:col-start-1"
              ]
          ]
          [ nav
              [ classNames navClasses ]
              [ a
                  [ classNames $ navItemClasses $ contractFilter == Running
                  , onClick_ $ SetContractFilter Running
                  , id "runningContractsFilter"
                  , href "#"
                  , ARIA.label "Running contracts"
                  ]
                  [ icon_ Icon.Running ]
              , tooltip "Running contracts" (RefId "runningContractsFilter")
                  Right
              , a
                  [ classNames $ navItemClasses $ contractFilter == Completed
                  , onClick_ $ SetContractFilter Completed
                  , id "completedContractsFilter"
                  , href "#"
                  , ARIA.label "Completed contracts"
                  ]
                  [ icon_ Icon.History ]
              , tooltip "Completed contracts" (RefId "completedContractsFilter")
                  Right
              , a
                  [ classNames $ navItemClasses false
                  , onClick_ $ OpenCard ContractTemplateCard
                  , href "#"
                  , ARIA.label "Create a new contract"
                  , id "newContractButton"
                  , href "#"
                  ]
                  [ icon Icon.AddBox [ "text-purple" ] ]
              , tooltip "Create a new contract" (RefId "newContractButton")
                  Right
              ]
          ]
      , div
          [ classNames
              [ "row-start-1", "col-start-1", "lg:row-start-3", "lg:self-end" ]
          ]
          [ nav
              [ classNames navClasses ]
              [ a
                  [ classNames $ navItemClasses false
                  , onClick_ $ OpenCard TutorialsCard
                  , id "tutorialsButton"
                  ]
                  [ icon Icon.Help [ "text-purple" ] ]
              , tooltip "Tutorials" (RefId "tutorialsButton") Right
              ]
          ]
      ]

contractCards
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Instant
  -> State
  -> ComponentHTML m
contractCards currentTime state =
  let
    walletCompanionStatus = state ^. _walletCompanionStatus
    contractFilter = state ^. _contractFilter
  in
    case walletCompanionStatus of
      WalletCompanionSynced ->
        case contractFilter of
          Running -> contractGridRunning currentTime state
          Completed -> contractGridCompleted currentTime state
      WaitingToSync ->
        div
          [ classNames
              [ "h-full", "flex", "flex-col", "justify-center", "items-center" ]
          ]
          [ icon Icon.Contract [ "text-big-icon", "text-gray" ]
          , p
              [ classNames [ "flex", "items-center", "gap-2" ] ]
              [ icon Icon.Sync [ "animate-spin" ]
              , text "Checking for new contracts..."
              ]
          ]

noContractsMessage :: forall p. ContractFilter -> HTML p Action
noContractsMessage contractFilter =
  div
    [ classNames
        [ "h-full", "flex", "flex-col", "justify-center", "items-center" ]
    ]
    $ [ icon Icon.Contract [ "text-big-icon", "text-gray" ] ]
        <> case contractFilter of
          Running ->
            [ p
                [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
                [ text "You have no running contracts." ]
            , p
                [ classNames [ "text-lg", "mb-4" ] ]
                [ text "Choose a template to begin." ]
            , button
                [ classNames Css.primaryButton
                , onClick_ $ OpenCard ContractTemplateCard
                ]
                [ text "Choose a template" ]
            ]
          Completed ->
            [ p
                [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
                [ text "You have no completed contracts." ]
            ]

contractGridClasses :: Array String
contractGridClasses =
  [ "grid"
  , "pt-4"
  , "pb-20"
  , "lg:pb-4"
  , "gap-8"
  , "auto-rows-min"
  , "mx-auto"
  , "max-w-contracts-grid-sm"
  , "md:max-w-none"
  , "md:w-contracts-grid-md"
  , "md:grid-cols-2"
  , "lg:w-contracts-grid-lg"
  , "lg:grid-cols-3"
  ]

contractGridRunning
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Instant
  -> State
  -> ComponentHTML m
contractGridRunning currentTime state =
  if Array.null runningContracts && Array.null newContracts then
    noContractsMessage Running
  else
    ul
      [ classNames contractGridClasses ]
      $ [ newContractCard ]
          <> (contractStartingPreviewCard <$> newContracts)
          <> (contractPreviewCard currentTime <$> runningContracts)
  where
  runningContracts = toArrayOf _runningContracts state
  newContracts = toArrayOf (_newContracts <<< folded) state
  newContractCard =
    a
      [ classNames
          [ "hidden"
          , "md:flex"
          , "flex-col"
          , "justify-center"
          , "items-center"
          , "rounded"
          , "border-2"
          , "border-darkgray"
          , "border-dashed"
          , "p-4"
          ]
      , onClick_ $ OpenCard ContractTemplateCard
      ]
      [ icon_ Icon.AddCircle
      , span_ [ text "New smart contract from template" ]
      ]

contractGridCompleted
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Instant
  -> State
  -> ComponentHTML m
contractGridCompleted currentTime state =
  let
    closedContracts = toArrayOf _closedContracts state
  in
    if Array.null closedContracts then noContractsMessage Completed
    else
      ul
        [ classNames contractGridClasses ]
        (contractPreviewCard currentTime <$> closedContracts)

currentWalletCard :: forall p. PABConnectedWallet -> HTML p Action
currentWalletCard wallet =
  let
    walletNickname = view _walletNickname wallet

    address = view _address wallet

    assets = view _assets wallet

    syncStatus = view _syncStatus wallet

    copyAddress = ClipboardAction <<< Clipboard.CopyToClipboard <<< A.toString
  in
    div
      [ classNames
          [ "h-full"
          , "grid"
          , "grid-rows-auto-1fr-auto"
          , "divide-y"
          , "divide-gray"
          ]
      ]
      [ h2
          [ classNames Css.cardHeader ]
          [ text "My wallet" ]
      , div
          [ role "table"
          , classNames
              [ "p-4", "overflow-y-auto", "overflow-x-hidden", "space-y-4" ]
          ]
          [ h3
              [ classNames [ "font-semibold", "text-lg" ], title "wallet-name" ]
              [ text $ WN.toString walletNickname ]
          , copyAddress <$> Address.render
              (Address.defaultInput address)
          , div [ role "row" ]
              [ h4
                  [ id "mw-balance", classNames [ "font-semibold" ] ]
                  [ text "Balance:" ]
              , p
                  [ role "cell", labelledBy "mw-balance", classNames Css.funds ]
                  [ text $ humanizeValue adaToken $ getAda assets ]
              ]
          , div [ role "row", classNames [ "space-y-2" ] ]
              [ h4
                  [ id "mw-status", classNames [ "font-semibold" ] ]
                  [ text "Status:" ]
              , case syncStatus of
                  OutOfSync ->
                    p
                      [ role "cell"
                      , labelledBy "mw-status"
                      , classNames [ "text-red" ]
                      ]
                      [ text "Out of sync" ]
                  Synchronizing progress ->
                    p [ role "cell", labelledBy "mw-status" ]
                      [ text "Synchronizing ("
                      , text $ show $ round $ progress * 100.0
                      , text "%)"
                      ]
                  Synchronized ->
                    p
                      [ role "cell"
                      , labelledBy "mw-status"
                      , classNames [ "text-green" ]
                      ]
                      [ text "Synchronized" ]
              ]
          ]
      , div
          [ classNames [ "p-4", "flex", "gap-4" ] ]
          [ button
              [ classNames $ Css.secondaryButton <> [ "flex-1" ]
              , onClick_ CloseCard
              ]
              [ text "Cancel" ]
          , button
              [ classNames $ Css.primaryButton <> [ "flex-1" ]
              , onClick_ (DisconnectWallet Nothing)
              ]
              [ text "Drop wallet" ]
          ]
      ]

-- FIXME: add a proper tutorials card (possibly a whole tutorials module)
tutorialsCard :: forall p. HTML p Action
tutorialsCard =
  div
    [ classNames [ "p-4" ] ]
    [ h2
        [ classNames [ "font-semibold", "text-lg", "mb-4" ] ]
        [ text "Tutorials" ]
    ]

walletNotFoundCard :: forall p. HTML p Action
walletNotFoundCard =
  div
    [ classNames
        [ "h-full"
        , "grid"
        , "grid-rows-auto-1fr-auto"
        , "divide-y"
        , "divide-gray"
        ]
    ]
    [ h2
        [ classNames Css.cardHeader ]
        [ text "Wallet backend disconnected" ]
    , div
        [ classNames
            [ "p-4", "overflow-y-auto", "overflow-x-hidden", "space-y-4" ]
        ]
        [ p_
            [ text
                "Connection to the wallet backend has been lost, and your wallet will need to be restored again."
            ]
        , p_
            [ text
                "For our centralized deployment, for performance purposes, we periodically restart the wallet backend and clear its database of wallets."
            ]
        ]
    , div
        [ classNames [ "p-4", "flex", "gap-4" ] ]
        [ button
            [ classNames $ Css.primaryButton <> [ "flex-1" ]
            , onClick_ (DisconnectWallet Nothing)
            ]
            [ text "Drop wallet" ]
        ]
    ]
