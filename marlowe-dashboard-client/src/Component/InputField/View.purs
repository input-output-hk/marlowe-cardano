module Component.InputField.View (renderInput) where

import Prologue hiding (div, min)
import Component.Input.Types (InputType(..)) as Input
import Component.Input.View (renderWithChildren) as Input
import Component.InputField.Lenses
  ( _after
  , _before
  , _dropdownLocked
  , _dropdownOpen
  , _id_
  , _numberFormat
  , _placeholder
  , _pristine
  , _readOnly
  , _value
  , _valueOptions
  )
import Component.InputField.State (validate)
import Component.InputField.Types
  ( class InputFieldError
  , Action(..)
  , InputDisplayOptions
  , State
  , inputErrorToString
  )
import Control.Alt ((<|>))
import Control.MonadPlus (guard)
import Css as Css
import Data.Array (null)
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Lens ((^.))
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.String (Pattern(..), contains, toLower)
import Halogen.Css (classNames)
import Halogen.HTML (HTML, a, div, span_, text)
import Halogen.HTML.Events (onMouseEnter, onMouseLeave)
import Halogen.HTML.Events.Extra (onClick_)
import Marlowe.Extended.Metadata (NumberFormat(..))

renderInput
  :: forall p e
   . InputFieldError e
  => InputDisplayOptions p (Action e)
  -> State e
  -> HTML p (Action e)
renderInput options state =
  let
    pristine = state ^. _pristine

    error = inputErrorToString <$> validate state <* guard (not pristine)

    numberFormat = options ^. _numberFormat

    valueOptions = options ^. _valueOptions

    dropdownOpen = state ^. _dropdownOpen

    value = state ^. _value
  in
    div
      [ classNames [ "relative" ] ]
      $ compact
          [ Just
              $ Input.renderWithChildren
                  { inputType: maybe Input.Text (const Input.Numeric)
                      numberFormat
                  , autocomplete: false
                  , onBlur:
                      (FormatValue <$> numberFormat)
                        <|>
                          ( guard (not $ state ^. _dropdownLocked) $>
                              SetDropdownOpen false
                          )
                  , onFocus: Just $ SetDropdownOpen true
                  , noHighlight: not $ null valueOptions
                  , id: options ^. _id_
                  , onChange: guard (not $ options ^. _readOnly) $> SetValue
                  , invalid: isJust error
                  , value
                  , placeholder: options ^. _placeholder
                  }
              $ \input ->
                  compact
                    [ options ^. _before <|> stringToSpan <$>
                        (decimalFormatLabel =<< numberFormat)
                    , Just input
                    , options ^. _after <|>
                        filter (_ == TimeFormat) numberFormat $> stringToSpan
                          "minutes"
                    ]
          , guard (not $ null valueOptions)
              $>
                let
                  matchingValueOptions = filter
                    (contains (Pattern $ toLower value) <<< toLower)
                    valueOptions
                in
                  div
                    [ classNames $ Css.pseudoDropdown
                        (dropdownOpen && not null matchingValueOptions)
                    , onMouseEnter $ const $ SetDropdownLocked true
                    , onMouseLeave $ const $ SetDropdownLocked false
                    ]
                    ( matchingValueOptions
                        <#> \option ->
                          a
                            [ classNames
                                [ "block"
                                , "p-4"
                                , "hover:bg-black"
                                , "hover:text-white"
                                ]
                            , onClick_ $ SetValueFromDropdown option
                            ]
                            [ text option ]
                    )
          , Just
              $ div
                  [ classNames Css.inputError ]
                  [ text $ fromMaybe "" error ]
          ]
  where
  decimalFormatLabel (DecimalFormat _ label) = Just label

  decimalFormatLabel _ = Nothing

  stringToSpan s = span_ [ text s ]
