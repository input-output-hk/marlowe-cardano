module Component.Toast.View (renderToast) where

import Prologue hiding (div)

import Component.Icons (Icon(..), icon, icon_)
import Component.Toast.Lenses (_expanded, _toastMessage)
import Component.Toast.Types (Action(..), State, ToastMessage)
import Css as Css
import Data.Lens (preview)
import Data.Maybe (fromMaybe)
import Halogen (RefLabel(..))
import Halogen.Css (classNames)
import Halogen.HTML (HTML, a, div, div_, span, text)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (id, ref)
import Halogen.HTML.Properties.ARIA (describedBy, labelledBy, role)

renderToast
  :: forall p
   . State
  -> HTML p Action
renderToast state = doRender (preview _toastMessage state)
  (fromMaybe false $ preview _expanded state)
  where
  doRender Nothing _ = div_ []

  doRender (Just toast) true = renderExpanded toast

  doRender (Just toast) false = renderCollapsed toast

renderExpanded
  :: forall p
   . ToastMessage
  -> HTML p Action
renderExpanded toast =
  div
    [ classNames $ Css.cardOverlay true ]
    [ div
        [ classNames (Css.card true)
        , role $ show toast.role
        , labelledBy "toast-short-message"
        , describedBy "toast-long-message"
        ]
        [ a
            [ classNames [ "absolute", "top-4", "right-4", toast.textColor ]
            , onClick_ CloseToast
            ]
            [ icon_ Close ]
        , div
            [ classNames
                [ "flex"
                , "font-semibold"
                , "px-5"
                , "py-4"
                , toast.bgColor
                , toast.textColor
                ]
            ]
            [ icon toast.icon [ "mr-2", toast.iconColor ]
            , span [ id "toast-short-message" ] [ text toast.shortDescription ]
            ]
        , div
            [ classNames [ "px-5", "pb-6", "pt-3", "md:pb-8" ]
            , id "toast-long-message"
            ]
            [ text $ fromMaybe "" toast.longDescription
            ]
        ]
    ]

renderCollapsed
  :: forall p
   . ToastMessage
  -> HTML p Action
renderCollapsed toast =
  let
    readMore = case toast.longDescription of
      Nothing -> div_ []
      Just _ ->
        div
          [ classNames [ "ml-4", "font-semibold", "underline", "flex-shrink-0" ]
          ]
          [ a
              [ onClick_ ExpandToast ]
              [ text "Read more" ]
          ]
  in
    div
      [ classNames
          [ "fixed"
          , "bottom-6"
          , "md:bottom-10"
          , "left-0"
          , "right-0"
          , "flex"
          , "justify-center"
          , "z-50"
          ]
      ]
      [ div
          [ classNames
              [ "px-4"
              , "py-2"
              , "rounded"
              , "shadow-lg"
              , "min-w-90pc"
              , "max-w-90pc"
              , "sm:min-w-sm"
              , "flex"
              , "justify-between"
              , "animate-from-below"
              , toast.bgColor
              , toast.textColor
              ]
          , ref $ RefLabel "collapsed-toast"
          , role $ show toast.role
          , labelledBy "toast-short-message"
          ]
          [ div [ classNames [ "flex", "overflow-hidden" ] ]
              [ icon toast.icon [ "mr-2", toast.iconColor ]
              , span
                  [ classNames
                      [ "font-semibold"
                      , "overflow-ellipsis"
                      , "whitespace-nowrap"
                      , "overflow-hidden"
                      ]
                  , id "toast-short-message"
                  ]
                  [ text toast.shortDescription ]
              ]
          , readMore
          ]
      ]
