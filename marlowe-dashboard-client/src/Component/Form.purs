module Component.Form where

import Prologue

import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (fromMaybe, maybe)
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Marlowe.Extended.Metadata (NumberFormat(..))

renderLabel :: forall w i. String -> String -> HH.HTML w i
renderLabel id label =
  HH.label [ classNames $ Css.labelBox <> Css.labelText, HP.for id ]
    [ HH.text label ]

renderErrorLabel :: forall w i. Maybe String -> HH.HTML w i
renderErrorLabel error = HH.span
  [ classNames $ Css.inputError <> maybe [ "invisible" ] (const []) error
  , HPA.role "alert"
  ]
  [ HH.text $ fromMaybe "Valid" error ]

renderInputBox
  :: forall w error i
   . Maybe error
  -> Array String
  -> Array (HH.HTML w i)
  -> HH.HTML w i
renderInputBox error classes =
  HH.div [ classNames $ Css.inputBox error <> classes ]

renderInput
  :: forall w i
   . String
  -> Array (HP.IProp HTMLinput i)
  -> HH.HTML w i
renderInput id props = HH.input
  ( props <> [ HP.id id, classNames Css.inputText ]
  )

renderTextInput
  :: forall error w i
   . String
  -> String
  -> Maybe error
  -> Array (HP.IProp HTMLinput i)
  -> (error -> String)
  -> HH.HTML w i
renderTextInput id label error props renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label
    , renderInputBox error [] [ renderInput id props ]
    , renderErrorLabel $ renderError <$> error
    ]

renderNumberInput
  :: forall error w i
   . NumberFormat
  -> String
  -> String
  -> Maybe error
  -> Array (HP.IProp HTMLinput i)
  -> (error -> String)
  -> HH.HTML w i
renderNumberInput format id label error props renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label
    , renderInputBox error [] $ join
        [ case format of
            DecimalFormat _ symbol -> [ HH.span_ [ HH.text symbol ] ]
            _ -> []
        , [ renderInput id $ props <> [ HP.type_ HP.InputNumber ] ]
        , case format of
            TimeFormat -> [ HH.span_ [ HH.text "minutes" ] ]
            _ -> []
        ]
    , renderErrorLabel $ renderError <$> error
    ]
