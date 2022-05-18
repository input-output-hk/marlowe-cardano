module Component.Toast.View (render) where

import Prologue hiding (div)

import Component.Icons (Icon(..), icon, icon_)
import Component.Toast.Types (Action(..), State, ToastEntry, indexRef)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.String.Extra (repeat)
import Data.Tuple.Nested ((/\))
import Halogen.Css (classNames)
import Halogen.HTML (HTML, a, br_, div, div_, pre_, span, span_, text)
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (id)
import Halogen.HTML.Properties.ARIA (describedBy, labelledBy, role)
import Text.Pretty (Doc(..))

render
  :: forall p
   . State
  -> HTML p Action
render state = case state.toasts of
  Nil -> div_ []
  toasts ->
    let
      renderedToasts = List.toUnfoldable toasts <#>
        \entry ->
          let
            toastElem =
              if entry.expanded then
                renderExpandedToast entry
              else
                renderCollapsedToast entry
          in
            -- See note on HK.div
            (indexRef "toast-message" entry.index) /\ toastElem
    in
      -- We used the Keyed version of this div to tell Halogen to reutilize
      -- the same nodes when they are moved around in the array. If we don't
      -- do this there is some problem with animations (as the elements come and go)
      -- and with the ref property
      -- check issue https://github.com/purescript-halogen/purescript-halogen/issues/423
      HK.div
        [ classNames
            [ "fixed"
            , "bottom-3"
            , "left-0"
            , "right-0"
            , "flex"
            , "flex-col"
            , "items-center"
            , "z-50"
            , "pointer-events-none"
            ]
        ]
        renderedToasts

renderCollapsedToast
  :: forall p
   . ToastEntry
  -> HTML p Action
renderCollapsedToast { index, message: toast } =
  let
    readMore = case toast.longDescription of
      Nothing -> div_ []
      Just _ ->
        div
          [ classNames [ "ml-4", "font-semibold", "underline", "flex-shrink-0" ]
          ]
          [ a
              [ onClick_ $ ToggleExpanded index ]
              [ text "Read more" ]
          ]
    shortMessageId = indexRef "toast-short-message" index

  in
    div
      [ classNames
          [ "px-4"
          , "py-2"
          , "mb-3"
          , "md:mb-6"
          , "rounded"
          , "shadow-lg"
          , "min-w-90pc"
          , "max-w-90pc"
          , "sm:min-w-sm"
          , "sm:max-w-sm"
          , "flex"
          , "justify-between"
          , "pointer-events-auto"
          , toast.bgColor
          , toast.textColor
          ]
      , id $ indexRef "toast-message" index
      , role $ show toast.role
      , labelledBy shortMessageId
      ]
      [ div [ classNames [ "flex-grow", "flex", "overflow-hidden" ] ]
          [ icon toast.icon [ "mr-2", toast.iconColor ]
          , span
              [ classNames
                  [ "font-semibold"
                  , "overflow-ellipsis"
                  , "whitespace-nowrap"
                  , "overflow-hidden"
                  ]
              , id shortMessageId
              ]
              [ text toast.shortDescription ]
          ]
      , readMore
      , a
          [ classNames [ "ml-2", "leading-none", toast.textColor ]
          , onClick_ $ AnimateCloseToast index
          ]
          [ icon_ Close ]
      ]

renderExpandedToast
  :: forall p
   . ToastEntry
  -> HTML p Action
renderExpandedToast { index, message: toast } =
  let
    readLess =
      div
        [ classNames [ "ml-4", "font-semibold", "underline", "flex-shrink-0" ]
        ]
        [ a
            [ onClick_ $ ToggleExpanded index ]
            [ text "Read less" ]
        ]
    shortMessageId = indexRef "toast-short-message" index
    longMessageId = indexRef "toast-long-message" index
    renderDoc = case _ of
      Empty -> [ span_ [] ]
      Text str -> [ span_ [ text str ] ]
      Newline n -> [ br_, (pre_ [ text $ repeat n " " ]) ]
      Cat a b -> renderDoc a <> renderDoc b

    header = div
      [ classNames
          [ "px-4"
          , "py-2"
          , "flex"
          , "justify-between"
          , toast.bgColor
          , toast.textColor
          ]
      , role $ show toast.role
      , labelledBy shortMessageId
      , describedBy longMessageId
      ]
      [ div [ classNames [ "flex-grow", "flex", "overflow-hidden" ] ]
          [ icon toast.icon [ "mr-2", toast.iconColor ]
          , span
              [ classNames
                  [ "font-semibold"
                  , "overflow-ellipsis"
                  , "whitespace-nowrap"
                  , "overflow-hidden"
                  ]
              , id shortMessageId
              ]
              [ text toast.shortDescription ]
          ]
      , readLess
      , a
          [ classNames [ "ml-2", "leading-none", toast.textColor ]
          , onClick_ $ AnimateCloseToast index
          ]
          [ icon_ Close ]
      ]
    body = div
      [ classNames [ "px-5", "pb-6", "pt-3", "md:pb-8", "bg-white" ]
      ]
      (renderDoc $ fromMaybe Empty toast.longDescription)
  in
    div
      [ classNames
          [ "rounded"
          , "overflow-hidden"
          , "mb-3"
          , "md:mb-6"
          , "shadow-lg"
          , "min-w-90pc"
          , "max-w-90pc"
          , "sm:min-w-sm"
          , "sm:max-w-sm"
          , "flex"
          , "flex-col"
          , "pointer-events-auto"
          ]
      , id $ indexRef "toast-message" index
      ]
      [ header
      , body
      ]
