module Component.Contacts.View
  ( contactsCard
  , walletIdTip
  ) where

import Prelude hiding (div)

import Clipboard (Action(..)) as Clipboard
import Component.AddContactForm (component) as AddContactForm
import Component.Address.View as Address
import Component.Contacts.Lenses (_cardSection, _walletNickname)
import Component.Contacts.Types
  ( Action(..)
  , CardSection(..)
  , State
  , WalletDetails
  )
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon_)
import Control.Monad.Rec.Class (class MonadRec)
import Css as Css
import Data.Address (Address)
import Data.Address as A
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Array (singleton) as Array
import Data.Lens ((^.))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (classNames)
import Halogen.HTML
  ( HTML
  , a
  , button
  , div
  , h2
  , h3
  , li
  , p
  , slot
  , span
  , span_
  , text
  , ul
  )
import Halogen.HTML.Events.Extra (onClick_)
import MainFrame.Types (ChildSlots)
import Type.Prelude (Proxy(..))

contactsCard
  :: forall m
   . MonadAff m
  => MonadRec m
  => AddressBook
  -> WalletDetails
  -> State
  -> ComponentHTML Action ChildSlots m
contactsCard addressBook currentWallet state =
  let
    cardSection = state ^. _cardSection
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
            NewWallet mTokenName -> Array.singleton $ slot
              (Proxy :: Proxy "addContactForm")
              unit
              AddContactForm.component
              { addressBook, mTokenName }
              identity

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
        , activeItem $ WN.toString nickname
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
  [ if AddressBook.isEmpty addressBook then
      -- If you're here, the addressBook can't be empty, because at least your own wallet will
      -- be in there. But that might change when we have real wallet integration, and it's easy
      -- to forget cases like these, so it seems sensible to code for it in case.
      p [ classNames [ "p-4" ] ] [ text "You do not have any contacts." ]
    else
      ul [ classNames [ "divide-y", "divide-gray" ] ]
        $ contactLi <$> AddressBook.toUnfoldable addressBook
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
      [ text $ WN.toString nickname ]

contactDetailsCard
  :: forall p
   . WalletDetails
  -> WalletNickname
  -> Address
  -> Array (HTML p Action)
contactDetailsCard currentWallet walletNickname address =
  let
    isCurrentWallet = walletNickname == currentWallet ^. _walletNickname

    copyAddress = ClipboardAction <<< Clipboard.CopyToClipboard <<< A.toString
  in
    [ div [ classNames [ "space-y-4", "p-4" ] ]
        [ h3
            [ classNames [ "text-lg", "font-semibold" ] ]
            [ text $ WN.toString walletNickname ]
        , copyAddress <$> Address.render (Address.defaultInput address)
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

walletIdTip :: forall p a. HTML p a
walletIdTip =
  p
    [ classNames [ "text-xs", "font-semibold" ] ]
    [ text
        "Tip: Copy and share your address with others so they can add you to their contracts"
    ]
