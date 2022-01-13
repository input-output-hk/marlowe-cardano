module Page.Welcome.View
  ( welcomeScreen
  , welcomeCard
  ) where

import Prologue hiding (div)

import Capability.Marlowe (class ManageMarlowe)
import Componenet.RestoreWalletForm (Msg(..))
import Componenet.RestoreWalletForm as RestoreWalletForm
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Css as Css
import Data.Lens ((^.))
import Data.List (foldMap)
import Data.Map as Map
import Data.MnemonicPhrase (class CheckMnemonic)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Halogen.Css (classNames)
import Halogen.HTML
  ( ComponentHTML
  , HTML
  , a
  , br_
  , button
  , div
  , div_
  , h2
  , hr
  , iframe
  , img
  , main
  , p
  , p_
  , section
  , slot
  , span_
  , text
  )
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (href, src, title)
import Images (marloweRunLogo)
import MainFrame.Types (ChildSlots)
import Page.Welcome.Lenses (_addressBook, _card, _cardOpen)
import Page.Welcome.Types (Action(..), Card(..), State)
import Type.Proxy (Proxy(..))

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
  => CheckMnemonic m
  => ManageMarlowe m
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
      [ classNames $ Css.cardOverlay cardOpen ]
      [ div
          [ classNames $ cardClasses cardOpen ]
          $ (flip foldMap card) \cardType -> case cardType of
              GetStartedHelpCard -> getStartedHelpCard
              GenerateWalletHelpCard -> generateWalletHelpCard
              UseNewWalletCard -> [] --useNewWalletCard state
              UseWalletCard -> [] --useWalletCard state
              RestoreTestnetWalletCard -> restoreTestnetWalletCard state
              LocalWalletMissingCard -> localWalletMissingCard
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
    , button
        [ classNames $ Css.primaryButton <> [ "w-full", "text-center" ]
        , onClick_ GenerateWallet
        ]
        [ text "Generate testnet wallet" ]
    , button
        [ classNames $ Css.secondaryButton <> [ "w-full", "text-center" ]
        , onClick_ $ OpenCard RestoreTestnetWalletCard
        ]
        [ text "Restore testnet wallet" ]
    , a
        [ classNames [ "block", "text-purple", "text-center", "font-semibold" ]
        , onClick_ $ OpenCard GenerateWalletHelpCard
        ]
        [ text "Why do I need to do this?" ]
    , hr [ classNames [ "max-w-xs", "mx-auto" ] ]
    , div
        [ classNames [ "pt-2", "flex", "justify-between" ] ]
        [ a
            [ classNames [ "flex", "font-bold" ]
            , href "https://staging.marlowe-web.iohkdev.io"
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
        , onClick_ $ OpenCard GetStartedHelpCard
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
            , onClick_ $ OpenCard GetStartedHelpCard
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

generateWalletHelpCard :: forall p. Array (HTML p Action)
generateWalletHelpCard =
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
          [ button
              [ classNames $ Css.primaryButton <> [ "flex-1" ]
              , onClick_ CloseCard
              ]
              [ text "Got it" ]
          ]
      ]
  ]

restoreTestnetWalletCard
  :: forall m
   . MonadAff m
  => CheckMnemonic m
  => ManageMarlowe m
  => State
  -> Array (ComponentHTML Action ChildSlots m)
restoreTestnetWalletCard state =
  let
    nicknames = WN.fromFoldable $ Map.keys $ state ^. _addressBook
  in
    [ a
        [ classNames [ "absolute", "top-4", "right-4" ]
        , onClick_ CloseCard
        ]
        [ icon_ Icon.Close ]
    , slot
        (Proxy :: _ "restoreWalletForm")
        unit
        RestoreWalletForm.component
        nicknames
        case _ of
          Closed -> CloseCard
          Restored nickname walletDetails ->
            ConnectWallet nickname walletDetails
    ]

-- TODO: Most likely remove or adapt all [Workflow 2][X] functionality (SCP-3218)
-- useNewWalletCard :: forall p. State -> Array (HTML p Action)
-- useNewWalletCard state =
--   let
--     enteringDashboardState = state ^. _enteringDashboardState

--     remoteWalletDetails = state ^. _remoteWalletDetails

--     walletNicknameInput = state ^. _walletNicknameInput

--     walletNickname = walletNicknameInput ^. _value
--   -- walletId = state ^. _walletId
--   in
--     [ a
--         [ classNames [ "absolute", "top-4", "right-4" ]
--         , onClick_ CloseCard
--         ]
--         [ icon_ Icon.Close ]
--     , div [ classNames [ "p-5", "lg:p-6", "space-y-4" ] ]
--         [ h2
--             [ classNames [ "font-bold" ] ]
--             [ text $ "Demo wallet generated" ]
--         , WalletNicknameInputAction
--             <$> renderInput
--               (walletNicknameInputDisplayOptions false)
--               walletNicknameInput
--         -- While removing information from the other contacts we have (part of task SCP-3174) I noticed
--         -- that we don't have the right data to show renderAddress in here, which is pointing to the direction
--         -- of removing this view altogether. Once the phase 1 is complete and we are sure we don use this, we can
--         -- remove the view and most likely the walletId and maybe the remoteWalletDetails from the state
--         -- , renderAddress walletId
--         , div
--             [ classNames [ "flex", "gap-4" ] ]
--             [ button
--                 [ classNames $ Css.secondaryButton <> [ "flex-1" ]
--                 , onClick_ CloseCard
--                 ]
--                 [ text "Cancel" ]
--             , button
--                 [ classNames $ Css.primaryButton <> [ "flex-1" ]
--                 , disabled $ isJust (validate walletNicknameInput)
--                     || enteringDashboardState
--                     || not isSuccess remoteWalletDetails
--                 , onClick_ $ ConnectWallet walletNickname
--                 ]
--                 [ text
--                     if enteringDashboardState then "Connecting..."
--                     else "Connect Wallet"
--                 ]
--             ]
--         ]
--     ]

-- TODO: Probably remove as part of SCP-3218
-- renderAddress :: forall p. PubKeyHash -> HTML p Action
-- renderAddress address =
--   let
--     copyAddress = ClipboardAction <<< Clipboard.CopyToClipboard
--   in
--     div
--       [ classNames [] ]
--       [ copyAddress
--           <$> Address.render
--             Address.defaultInput
--               { label = "Demo wallet ID", value = address }
--       , walletIdTip
--       ]
-- TODO: Most likely remove or adapt all [Workflow 2][X] functionality (SCP-3218)
-- useWalletCard :: forall p. State -> Array (HTML p Action)
-- useWalletCard state =
--   let
--     enteringDashboardState = state ^. _enteringDashboardState

--     remoteWalletDetails = state ^. _remoteWalletDetails

--     walletNicknameInput = state ^. _walletNicknameInput

--     walletNickname = walletNicknameInput ^. _value
--   -- walletId = state ^. _walletId
--   in
--     [ a
--         [ classNames [ "absolute", "top-4", "right-4" ]
--         , onClick_ CloseCard
--         ]
--         [ icon_ Icon.Close ]
--     , div [ classNames [ "p-5", "lg:p-6", "space-y-4" ] ]
--         [ h2
--             [ classNames [ "font-bold", "truncate", "w-11/12" ] ]
--             [ text $ "Demo wallet " <> walletNickname ]
--         , WalletNicknameInputAction
--             <$> renderInput
--               (walletNicknameInputDisplayOptions true)
--               walletNicknameInput
--         -- same note that in useNewWalletCard
--         -- , renderAddress walletId
--         , div
--             [ classNames [ "flex", "gap-4" ] ]
--             [ button
--                 [ classNames $ Css.secondaryButton <> [ "flex-1" ]
--                 , onClick_ CloseCard
--                 ]
--                 [ text "Cancel" ]
--             , button
--                 [ classNames $ Css.primaryButton <> [ "flex-1" ]
--                 , onClick_ $ ConnectWallet walletNickname
--                 , disabled $ enteringDashboardState || not isSuccess
--                     remoteWalletDetails
--                 ]
--                 [ text
--                     if enteringDashboardState then "Connecting..."
--                     else "Connect Wallet"
--                 ]
--             ]
--         ]
--     ]

localWalletMissingCard :: forall p. Array (HTML p Action)
localWalletMissingCard =
  [ div
      [ classNames $ Css.card true ]
      [ a
          [ classNames [ "absolute", "top-4", "right-4" ]
          , onClick_ CloseCard
          ]
          [ icon_ Icon.Close ]
      , div
          [ classNames
              [ "flex", "font-semibold", "gap-2", "px-5", "py-4", "bg-gray" ]
          ]
          [ icon Icon.ErrorOutline []
          , span_ [ text "Wallet not found" ]
          ]
      , div
          [ classNames [ "p-5", "pb-6", "lg:pb-8", "space-y-4" ] ]
          [ p_
              [ text
                  "A wallet that you have previously used is no longer available in our demo server. This is probably because the demo server has been updated. (Note that this demo is in continuous development, and data is not preserved between updates.) We recommend that you use the button below to clear your browser's cache for this site and start again."
              ]
          , div
              [ classNames [ "flex", "justify-center" ] ]
              [ button
                  [ classNames Css.primaryButton
                  , onClick_ ClearLocalStorage
                  ]
                  [ text "Clear Cache" ]
              ]
          ]
      ]
  ]
