module Component.Contacts.View
  ( contactsCard
  , walletIdTip
  ) where

import Prelude hiding (div)

import Clipboard (Action(..)) as Clipboard
import Component.Address.View as Address
import Component.Contacts.Lenses
  ( _addressBook
  , _addressInput
  , _cardSection
  , _walletNickname
  , _walletNicknameInput
  )
import Component.Contacts.Types
  ( Action(..)
  , AddressBook
  , AddressError
  , CardSection(..)
  , State
  , WalletDetails
  , WalletNickname
  , WalletNicknameError
  )
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon_)
import Component.InputField.State (validate)
import Component.InputField.Types (State) as InputField
import Component.InputField.View (renderInput)
import Component.Label.View as Label
import Css as Css
import Data.Lens ((^.))
import Data.Map (isEmpty, toUnfoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Halogen.Css (classNames)
import Halogen.HTML (HTML, a, button, div, h2, h3, li, p, span, span_, text, ul)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (disabled)
import Marlowe.Semantics (PubKeyHash)

contactsCard :: forall p. WalletDetails -> State -> HTML p Action
contactsCard currentWallet state =
  let
    addressBook = state ^. _addressBook

    cardSection = state ^. _cardSection

    walletNicknameInput = state ^. _walletNicknameInput

    addressInput = state ^. _addressInput
  in
    div
      [ classNames
          [ "h-full"
          , "grid"
          , "grid-rows-auto-auto-1fr-auto"
          , "divide-y"
          , "divide-gray"
          ]
      ]
      $
        [ h2
            [ classNames Css.cardHeader ]
            [ text "Contacts" ]
        , contactsBreadcrumb cardSection
        ]
          <> case cardSection of
            Home -> addressBookCard addressBook
            ViewWallet nickname address ->
              contactDetailsCard currentWallet nickname address
            NewWallet mTokenName ->
              newWalletCard walletNicknameInput addressInput mTokenName

contactsBreadcrumb :: forall p. CardSection -> HTML p Action
contactsBreadcrumb cardSection =
  div
    [ classNames
        [ "overflow-x-auto"
        , "flex"
        , "align-baseline"
        , "px-4"
        , "gap-1"
        , "text-xs"
        ]
    ]
    case cardSection of
      Home -> [ activeItem "Home" ]
      ViewWallet nickname _ ->
        [ previousItem "Home" Home
        , arrow
        , activeItem nickname
        ]
      NewWallet mTokenName ->
        [ previousItem "Home" Home
        , arrow
        , case mTokenName of
            Nothing -> activeItem "New Contact"
            Just tokenName -> activeItem $ "New Contact for " <> tokenName <>
              " Role"
        ]
  where
  activeItem itemText =
    span
      [ classNames
          [ "whitespace-nowrap"
          , "py-2.5"
          , "border-black"
          , "border-b-2"
          , "font-semibold"
          ]
      ]
      [ text itemText ]

  previousItem itemText stage =
    a
      [ classNames
          [ "whitespace-nowrap"
          , "py-2.5"
          , "text-purple"
          , "border-transparent"
          , "border-b-2"
          , "hover:border-purple"
          , "font-semibold"
          ]
      , onClick_ $ SetCardSection stage
      ]
      [ text itemText ]

  arrow = span [ classNames [ "mt-2" ] ] [ icon_ Icon.Next ]

addressBookCard :: forall p. AddressBook -> Array (HTML p Action)
addressBookCard addressBook =
  [ if isEmpty addressBook then
      -- If you're here, the addressBook can't be empty, because at least your own wallet will
      -- be in there. But that might change when we have real wallet integration, and it's easy
      -- to forget cases like these, so it seems sensible to code for it in case.
      p [ classNames [ "p-4" ] ] [ text "You do not have any contacts." ]
    else
      ul [ classNames [ "divide-y", "divide-gray" ] ]
        $ contactLi <$> toUnfoldable addressBook
  , button
      [ classNames $ Css.primaryButton <> Css.withIcon Icon.NewContact <>
          Css.fixedBottomRight
      , onClick_ $ SetCardSection $ NewWallet Nothing
      ]
      [ text "New contact" ]
  ]
  where
  contactLi (nickname /\ address) =
    li
      [ classNames
          [ "px-4", "py-2", "hover:cursor-pointer", "hover:text-purple" ]
      , onClick_ $ SetCardSection $ ViewWallet nickname address
      ]
      [ text nickname ]

contactDetailsCard
  :: forall p
   . WalletDetails
  -> WalletNickname
  -> PubKeyHash
  -> Array (HTML p Action)
contactDetailsCard currentWallet walletNickname address =
  let
    isCurrentWallet = walletNickname == currentWallet ^. _walletNickname

    copyAddress = ClipboardAction <<< Clipboard.CopyToClipboard
  in
    [ div [ classNames [ "space-y-4", "p-4" ] ]
        [ h3
            [ classNames [ "text-lg", "font-semibold" ] ]
            [ text walletNickname ]
        , copyAddress
            <$> Address.render
              Address.defaultInput
                { label = "Wallet Address"
                , value = address
                }
        , walletIdTip
        ]
    , div
        [ classNames [ "flex", "gap-4", "p-4" ] ]
        [ a
            [ classNames $ Css.button <> [ "text-center" ]
            , onClick_ $ SetCardSection Home
            ]
            [ text "Back" ]
        , if isCurrentWallet then
            span
              [ classNames $ Css.button <>
                  [ "flex-1"
                  , "text-center"
                  , "border-2"
                  , "border-green"
                  , "text-green"
                  ]
              ]
              [ text "Using this wallet" ]
          else
            span_ []
        ]
    ]

newWalletCard
  :: forall p
   . InputField.State WalletNicknameError
  -> InputField.State AddressError
  -> Maybe String
  -> Array (HTML p Action)
newWalletCard walletNicknameInput walletIdInput mTokenName =
  let
    walletNicknameInputDisplayOptions =
      { additionalCss: mempty
      , id_: "newWalletNickname"
      , placeholder: "Nickname"
      , readOnly: false
      , numberFormat: Nothing
      , valueOptions: mempty
      , after: Nothing
      , before:
          Just
            $ Label.render
                Label.defaultInput
                  { for = "newWalletNickname", text = "Wallet nickname" }
      }

    addressInputDisplayOptions =
      { additionalCss: mempty
      , id_: "newAddress"
      , placeholder: "Address"
      , readOnly: false
      , numberFormat: Nothing
      , valueOptions: mempty
      , after: Nothing
      , before:
          Just
            $ Label.render
                Label.defaultInput
                  { for = "newAddress", text = "Wallet address" }
      }
  in
    [ div [ classNames [ "space-y-4", "p-4" ] ]
        [ WalletNicknameInputAction <$> renderInput
            walletNicknameInputDisplayOptions
            walletNicknameInput
        , AddressInputAction <$> renderInput addressInputDisplayOptions
            walletIdInput
        ]
    , div
        [ classNames [ "flex", "gap-4", "p-4" ] ]
        [ a
            [ classNames $ Css.button <> [ "flex-1", "text-center" ]
            , onClick_ case mTokenName of
                Just _ -> CancelNewContactForRole
                Nothing -> SetCardSection Home
            ]
            [ text "Back" ]
        , button
            [ classNames $ Css.primaryButton <> [ "flex-1" ]
            , disabled $ isJust (validate walletNicknameInput) || isJust
                (validate walletIdInput)
            , onClick_ $ SaveWallet mTokenName
            ]
            [ text "Save" ]
        ]
    ]

walletIdTip :: forall p a. HTML p a
walletIdTip =
  p
    [ classNames [ "text-xs", "font-semibold" ] ]
    [ text
        "Tip: Copy and share your address with others so they can add you to their contracts"
    ]
