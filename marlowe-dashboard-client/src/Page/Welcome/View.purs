module Page.Welcome.View
  ( welcomeScreen
  , welcomeCard
  ) where

import Prologue hiding (div)

import Capability.Marlowe (class ManageMarlowe)
import Capability.Toast (class Toast)
import Capability.Wallet (class ManageWallet)
import Component.Button.Types (Variant(..)) as B
import Component.Button.View (button) as B
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Css as Css
import Data.Lens ((^.))
import Data.List (foldMap)
import Data.MnemonicPhrase (toString) as MnemonicPhrase
import Effect.Aff.Class (class MonadAff)
import Halogen.Css (classNames)
import Halogen.HTML
  ( ComponentHTML
  , HTML
  , a
  , br_
  , div
  , div_
  , h2
  , hr
  , iframe
  , img
  , main
  , mark
  , p
  , p_
  , section
  , slot
  , text
  )
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (href, src, title)
import Halogen.HTML.Properties.ARIA (role)
import Halogen.HTML.Properties.ARIA as ARIA
import Halogen.Store.Monad (class MonadStore)
import Images (marloweRunLogo)
import MainFrame.Types (ChildSlots)
import Page.Welcome.ConfirmMnemonic (_confirmMnemonic)
import Page.Welcome.ConfirmMnemonic as ConfirmMnemonic
import Page.Welcome.CreateWallet (_createWallet)
import Page.Welcome.CreateWallet as CreateWallet
import Page.Welcome.Lenses (_card, _cardOpen)
import Page.Welcome.RestoreWallet (_restoreWallet)
import Page.Welcome.RestoreWallet as RestoreWallet
import Page.Welcome.Types (Action(..), Card(..), CreateWalletStep(..), State)
import Store as Store

welcomeScreen :: forall p. State -> HTML p Action
welcomeScreen _ =
  main
    [ classNames
        [ "h-full"
        , "overflow-x-hidden"
        , "grid"
        , "gap-8"
        , "grid-rows-1fr-auto-auto-1fr"
        , "lg:grid-rows-1fr-auto-1fr"
        , "lg:grid-cols-1fr-auto-auto-1fr"
        , "bg-background-shape"
        , "bg-right-top"
        , "bg-no-repeat"
        , "p-4"
        ]
    ]
    [ useWalletBox
    , gettingStartedBox
    ]

welcomeCard
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => Toast m
  => State
  -> ComponentHTML Action ChildSlots m
welcomeCard state =
  let
    card = state ^. _card

    cardOpen = state ^. _cardOpen

    cardClasses =
      if card == Just GetStartedHelpCard then Css.videoCard else Css.card
  in
    div
      [ classNames $ Css.cardOverlay cardOpen, role "dialog" ]
      [ div
          [ classNames $ cardClasses cardOpen ]
          $ (flip foldMap card) \cardType -> case cardType of
              GetStartedHelpCard -> getStartedHelpCard
              CreateWalletHelpCard -> createWalletHelpCard
              CreateWalletCard res -> createWalletCard res
              RestoreWalletCard -> restoreWalletCard
      ]

------------------------------------------------------------
useWalletBox :: forall p. HTML p Action
useWalletBox =
  section
    [ classNames
        [ "row-start-2"
        , "lg:col-start-2"
        , "bg-white"
        , "rounded-lg"
        , "shadow-lg"
        , "p-8"
        , "lg:p-12"
        , "max-w-sm"
        , "mx-auto"
        , "lg:max-w-none"
        , "lg:w-welcome-box"
        , "space-y-4"
        ]
    ]
    [ div [ classNames [ "p-2 pt-0" ] ]
        [ img
            [ classNames [ "mx-auto", "text-center" ]
            , src marloweRunLogo
            ]
        ]
    , p
        [ classNames [ "text-center" ] ]
        [ text
            "To begin using the Marlowe Run demo, generate o restore a testnet wallet."
        ]
    , B.button
        B.Primary
        (Just $ OnCreateWallet)
        [ "w-full", "text-center" ]
        [ text "Generate testnet wallet" ]
    , B.button
        B.Secondary
        (Just $ OnRestoreWallet)
        [ "w-full", "text-center" ]
        [ text "Restore testnet wallet" ]
    , a
        [ classNames [ "block", "text-purple", "text-center", "font-semibold" ]
        , onClick_ OnCreateWalletHelp
        ]
        [ text "Why do I need to do this?" ]
    , hr [ classNames [ "max-w-xs", "mx-auto" ] ]
    , div
        [ classNames [ "pt-2", "flex", "justify-between" ] ]
        [ a
            [ classNames [ "flex", "font-bold" ]
            , href "https://marlowe-finance.io"
            ]
            [ icon_ Icon.Previous
            , text "Back to home page"
            ]
        , a
            [ classNames [ "font-bold" ]
            -- FIXME: add link to documentation
            , href ""
            ]
            [ text "Docs" ]
        ]
    ]

gettingStartedBox :: forall p. HTML p Action
gettingStartedBox =
  section
    [ classNames
        [ "row-start-3"
        , "lg:row-start-2"
        , "lg:col-start-3"
        , "max-w-sm"
        , "mx-auto"
        , "lg:max-w-none"
        , "lg:w-welcome-box"
        , "flex"
        , "flex-col"
        , "justify-center"
        ]
    ]
    [ a
        [ classNames [ "text-purple", "text-center", "lg:hidden" ]
        , onClick_ OnGetStartedHelp
        ]
        [ icon Icon.Play $ Css.bgBlueGradient <>
            [ "text-3xl", "text-white", "rounded-full" ]
        , br_
        , text "Watch our get started tutorial"
        ]
    , div
        [ classNames [ "hidden", "lg:block", "space-y-6" ] ]
        [ a
            [ classNames
                [ "block"
                , "relative"
                , "rounded-lg"
                , "shadow-lg"
                , "bg-get-started-thumbnail"
                , "bg-cover"
                , "w-full"
                , "h-welcome-box"
                ]
            , onClick_ OnGetStartedHelp
            ]
            [ icon Icon.Play $ Css.bgBlueGradient <>
                [ "absolute"
                , "bottom-4"
                , "right-4"
                , "text-3xl"
                , "text-white"
                , "rounded-full"
                ]
            ]
        , div_
            [ p
                [ classNames [ "font-semibold", "text-lg", "text-center" ] ]
                [ text "New to Marlowe Run?" ]
            , p
                [ classNames [ "text-lg", "text-center" ] ]
                [ text "Watch our get started tutorial" ]
            ]
        ]
    ]

------------------------------------------------------------
getStartedHelpCard :: forall p. Array (HTML p Action)
getStartedHelpCard =
  [ a
      [ classNames [ "absolute", "-top-10", "right-0", "lg:-right-10" ]
      , onClick_ CloseCard
      ]
      [ icon Icon.Close [ "text-lg", "rounded-full", "bg-white", "p-2" ] ]
  , div
      [ classNames $ Css.embeddedVideoContainer <>
          [ "rounded", "overflow-hidden" ]
      ]
      [ iframe
          [ classNames Css.embeddedVideo
          , src "https://www.youtube.com/embed/PJLtKJJMH0U"
          , title "Get started video"
          ]
      ]
  ]

createWalletHelpCard :: forall p. Array (HTML p Action)
createWalletHelpCard =
  [ div
      [ classNames [ "p-5", "pb-6", "lg:pb-8", "space-y-4" ] ]
      [ h2
          [ classNames [ "font-semibold" ] ]
          [ text "Why generate a demo wallet?" ]
      , p_
          [ text
              "We use a centralized server connected to a Testnet so you can play around with the app and all its incredible features without using your own tokens from your real wallet. Do not restore a wallet using a mnemonic phrase from a real wallet."
          ]
      , div
          [ classNames [ "flex" ] ]
          [ B.button
              B.Primary
              (Just CloseCard)
              [ "flex-1" ]
              [ text "Got it" ]
          ]
      ]
  ]

createWalletCard
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageMarlowe m
  => Toast m
  => CreateWalletStep
  -> Array (ComponentHTML Action ChildSlots m)
createWalletCard = case _ of
  CreateWalletSetWalletName -> do
    [ a
        [ classNames [ "absolute", "top-4", "right-4" ]
        , onClick_ CloseCard
        ]
        [ icon_ Icon.Close ]
    , slot
        _createWallet
        unit
        CreateWallet.component
        {}
        OnCreateWalletMsg
    ]
  CreateWalletPresentMnemonic r@{ mnemonic } ->
    [ div
        [ classNames [ "p-5", "lg:p-6", "space-y-2" ] ]
        [ div_
            [ h2
                [ classNames [ "font-bold" ] ]
                [ text "Testnet wallet mnemonic phrase" ]
            , p_
                [ text
                    "Please save this mnemonic phrase if you want to preserve accesses to your testnet account:"
                ]
            , mark
                [ ARIA.role "mark", classNames [ "font-bold", "m-2", "mb-6" ] ]
                [ text $ MnemonicPhrase.toString mnemonic ]
            , B.button
                B.Primary
                ( Just $ OnAcknowledgeMnemonic r
                )
                [ "flex-1", "w-full" ]
                [ text "Ok" ]
            ]
        ]
    ]
  CreateWalletConfirmMnemonic newWalletDetails ->
    [ slot
        _confirmMnemonic
        unit
        ConfirmMnemonic.component
        { newWalletDetails }
        OnConfirmMnemonicMsg
    ]

restoreWalletCard
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => ManageWallet m
  => Toast m
  => Array (ComponentHTML Action ChildSlots m)
restoreWalletCard =
  [ a
      [ classNames [ "absolute", "top-4", "right-4" ]
      , onClick_ CloseCard
      ]
      [ icon_ Icon.Close ]
  , slot
      _restoreWallet
      unit
      RestoreWallet.component
      {}
      OnRestoreWalletMsg
  ]
