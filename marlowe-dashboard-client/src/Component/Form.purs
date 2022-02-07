module Component.Form where

import Prologue

import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import Data.Maybe (fromMaybe, maybe)
import Halogen.Css (classNames)
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Marlowe.Extended.Metadata (NumberFormat(..))
import Marlowe.Semantics (CurrencySymbol)

renderLabel :: forall w i. String -> String -> HH.HTML w i
renderLabel id label =
  HH.label [ classNames $ Css.labelBox <> Css.labelText, HP.for id ]
    [ HH.text label ]

renderErrorLabel :: forall w i. String -> Maybe String -> HH.HTML w i
renderErrorLabel id error = HH.label
  [ classNames $ Css.inputError <> maybe [ "invisible" ] (const []) error
  , HP.for id
  ]
  [ HH.text $ fromMaybe "Valid" error ]

renderInputBox
  :: forall w error i. Maybe error -> Array (HH.HTML w i) -> HH.HTML w i
renderInputBox error =
  HH.div [ classNames $ Css.inputBox error ]

renderInput
  :: forall pa error output slots m
   . String
  -> String
  -> Array (HP.IProp HTMLinput (Input.Action pa error output slots m))
  -> Input.ComponentHTML pa error output slots m
renderInput id value props = HH.input
  ( Input.setInputProps value $ props <> [ HP.id id, classNames Css.inputText ]
  )

renderTextInput
  :: forall pa error output slots m
   . String
  -> String
  -> Input.State error output
  -> (error -> String)
  -> Input.ComponentHTML pa error output slots m
renderTextInput id label state renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label
    , renderInputBox state.error [ renderInput id state.value [] ]
    , renderErrorLabel id $ renderError <$> state.error
    ]

renderNumberInput
  :: forall pa error output slots m
   . NumberFormat
  -> String
  -> String
  -> Input.State error output
  -> (error -> String)
  -> Input.ComponentHTML pa error output slots m
renderNumberInput format id label state renderError =
  HH.div [ classNames [ "relative" ] ]
    [ renderLabel id label
    , renderInputBox state.error $ join
        [ case format of
            DecimalFormat _ symbol -> [ HH.span_ [ HH.text symbol ] ]
            _ -> []
        , [ renderInput id state.value [ HP.type_ HP.InputNumber ] ]
        , case format of
            TimeFormat -> [ HH.span_ [ HH.text "minutes" ] ]
            _ -> []
        ]
    , renderErrorLabel id $ renderError <$> state.error
    ]
