module Component.Form where

import Prologue

import Component.Hint.State (hint)
import Component.Popper (Placement(..))
import Control.Alternative (guard)
import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import Data.Compactable (compact)
import Data.Maybe (fromMaybe, isJust, maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Language.Marlowe.Extended.V1.Metadata (NumberFormat(..))

type HasHintSlot slots =
  (hintSlot :: forall query. H.Slot query Void String | slots)

renderLabel
  :: forall action slots m
   . MonadAff m
  => String
  -> String
  -> Maybe PlainHTML
  -> HH.ComponentHTML action (HasHintSlot slots) m
renderLabel id label hintHtml =
  HH.div [ classNames Css.labelBox ] $ compact
    [ pure $ HH.label [ classNames Css.labelText, HP.for id ] [ HH.text label ]
    , hint [] ("hint-" <> label) Auto <$> hintHtml
    ]

renderErrorLabel :: forall w i. Boolean -> Maybe String -> HH.HTML w i
renderErrorLabel isWarning error = HH.span
  [ classNames $ Css.inputError isWarning <> maybe [ "invisible" ] (const [])
      error
  , HPA.role "alert"
  ]
  [ HH.text $ fromMaybe "Valid" error ]

renderInputBox
  :: forall w error i
   . Boolean
  -> Maybe error
  -> Array String
  -> Array (HH.HTML w i)
  -> HH.HTML w i
renderInputBox isWarning error classes =
  HH.div
    [ classNames $ Css.inputBox (guard (not isWarning) *> error) <> classes ]

renderInput
  :: forall w i
   . String
  -> Array (HP.IProp HTMLinput i)
  -> HH.HTML w i
renderInput id props = HH.input
  ( props <> [ HP.id id, classNames Css.inputText ]
  )

renderTextInput
  :: forall error output action slots m
   . MonadAff m
  => String
  -> String
  -> Maybe PlainHTML
  -> Maybe output
  -> Maybe error
  -> Array (HP.IProp HTMLinput action)
  -> (error -> String)
  -> HH.ComponentHTML action (HasHintSlot slots) m
renderTextInput id label hintHtml output error props renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderInputBox (isJust output) error [] [ renderInput id props ]
    , renderLabel id label hintHtml
    , renderErrorLabel (isJust output) $ renderError <$> error
    ]

renderNumberInput
  :: forall error output action slots m
   . MonadAff m
  => NumberFormat
  -> String
  -> String
  -> Maybe PlainHTML
  -> Maybe output
  -> Maybe error
  -> Array (HP.IProp HTMLinput action)
  -> (error -> String)
  -> HH.ComponentHTML action (HasHintSlot slots) m
renderNumberInput format id label hintHtml output error props renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label hintHtml
    , renderInputBox (isJust output) error [] $ join
        [ case format of
            DecimalFormat _ symbol -> [ HH.span_ [ HH.text symbol ] ]
            _ -> []
        , [ renderInput id $ props <> [ HP.type_ HP.InputNumber ] ]
        , case format of
            TimeFormat -> [ HH.span_ [ HH.text "minutes" ] ]
            _ -> []
        ]
    , renderErrorLabel (isJust output) $ renderError <$> error
    ]
