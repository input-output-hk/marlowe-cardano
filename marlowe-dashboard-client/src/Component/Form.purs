module Component.Form where

import Prologue

import Control.Alternative (guard)
import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (fromMaybe, isJust, maybe)
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Marlowe.Extended.Metadata (NumberFormat(..))

renderLabel :: forall w i. String -> String -> HH.HTML w i
renderLabel id label =
  HH.label [ classNames $ Css.labelBox <> Css.labelText, HP.for id ]
    [ HH.text label ]

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
  :: forall error output w i
   . String
  -> String
  -> Maybe output
  -> Maybe error
  -> Array (HP.IProp HTMLinput i)
  -> (error -> String)
  -> HH.HTML w i
renderTextInput id label output error props renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label
    , renderInputBox (isJust output) error [] [ renderInput id props ]
    , renderErrorLabel (isJust output) $ renderError <$> error
    ]

renderNumberInput
  :: forall error output w i
   . NumberFormat
  -> String
  -> String
  -> Maybe output
  -> Maybe error
  -> Array (HP.IProp HTMLinput i)
  -> (error -> String)
  -> HH.HTML w i
renderNumberInput format id label output error props renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label
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
