module Forms where

import Prologue hiding (div)

import Component.Label.View as Label
import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import DOM.HTML.Indexed.InputType (InputType)
import Data.Address (Address)
import Data.Address (AddressError(..), validator) as A
import Data.Array ((:))
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Filterable (filter)
import Data.Int as Int
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.Set (Set)
import Data.String (Pattern(..), trim)
import Data.String as String
import Data.String.Extra (leftPadTo, rightPadTo)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (Form, FormHTML)
import Halogen.Form as Form
import Halogen.Form.FormM (FormM)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (usePutState)
import NSelect as Select
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))

type InputSlots slots =
  (input :: forall query. H.Slot query String String | slots)

_input :: Proxy "input"
_input = Proxy

type AutocompleteSlots parentAction slots =
  ( autocomplete ::
      forall query
       . H.Slot
           query
           (Form.Action parentAction AutocompleteInput)
           String
  | slots
  )

_autocomplete :: Proxy "autocomplete"
_autocomplete = Proxy

_select :: Proxy "select"
_select = Proxy

renderInput
  :: forall w action
   . Array (HH.IProp HTMLinput action)
  -> { after :: Array (HH.HTML w action)
     , before :: Array (HH.HTML w action)
     , error :: Maybe String
     , id :: String
     , inputType :: InputType
     , label :: String
     , value :: String
     }
  -> HH.HTML w action
renderInput props inp =
  HH.div [ classNames containerStyles ]
    $ labelEl : before <> [ inputEl ] <> after
  where
  { after, before, error, id, inputType, label, value } = inp
  labelEl = Label.render Label.defaultInput
    { for = id
    , text = label
    }
  inputEl = HH.input $ props <>
    [ classNames inputStyles
    , HP.id id
    , HP.value value
    , HP.type_ inputType
    ]
  containerStyles =
    [ "border-2"
    , "duration-200"
    , "flex"
    , "gap-1"
    , "items-baseline"
    , "p-4"
    , "relative"
    , "rounded-sm"
    , "transition-all"
    , "w-full"
    , "focus:border-transparent"
    , "focus:ring-2"
    , "focus-within:border-transparent"
    , "focus-within:ring-2"
    ] <>
      if isJust error then
        [ "border-red", "ring-red" ]
      else
        [ "border-gray", "ring-purple" ]

  inputStyles =
    [ "border-0"
    , "duration-200"
    , "flex-1"
    , "focus:outline-none"
    , "focus:ring-0"
    , "leading-none"
    , "outline-none"
    , "p-0"
    , "ring-0"
    , "text-black"
    , "transition-all"
    ]

inputComponent
  :: forall q m
   . H.Component
       q
       { after ::
           Array (HH.ComponentHTML (Hooks.HookM m Unit) (InputSlots ()) m)
       , before ::
           Array (HH.ComponentHTML (Hooks.HookM m Unit) (InputSlots ()) m)
       , error :: Maybe String
       , id :: String
       , label :: String
       , value :: String
       , format :: String -> String
       , inputType :: InputType
       }
       String
       m
inputComponent =
  Hooks.component
    \{ outputToken }
     { inputType, after, before, value, id, label, format, error } ->
      Hooks.do
        Tuple pristine putPristine <- usePutState true
        Tuple visited putVisited <- usePutState false
        Tuple focused putFocused <- usePutState false
        let error' = filter (\_ -> not pristine && visited) error
        Hooks.pure do
          HH.div [ classNames [ "relative" ] ]
            [ renderInput
                [ HE.onValueChange \value' -> do
                    putPristine false
                    if focused then
                      Hooks.raise outputToken value'
                    else
                      Hooks.raise outputToken $ format value'
                , HE.onFocus \_ -> putFocused true
                , HE.onBlur \_ -> do
                    Hooks.raise outputToken $ format value
                    putVisited true
                    putFocused false
                ]
                { before
                , after
                , value: value
                , error: error'
                , id: id
                , inputType: inputType
                , label
                }
            , HH.label
                [ classNames
                    $ Css.inputError <> maybe [ "invisible" ] (const []) error'
                , HP.for id
                ]
                [ HH.text $ fromMaybe "Valid" error ]
            ]

type AutocompleteInput =
  { options :: Array String
  , value :: String
  }

autocompleteComponent
  :: forall parentAction q slots m
   . MonadAff m
  => H.Component
       q
       { after ::
           Array (HH.ComponentHTML (Select.Action parentAction slots m) slots m)
       , before ::
           Array (HH.ComponentHTML (Select.Action parentAction slots m) slots m)
       , error :: Maybe String
       , id :: String
       , label :: String
       , value :: AutocompleteInput
       }
       (Form.Action parentAction AutocompleteInput)
       m
autocompleteComponent = Hooks.component \{ outputToken } inp -> Hooks.do
  Tuple pristine putPristine <- usePutState true
  let { after, before, label, id, value } = inp
  filtered <- Hooks.captures { value } Hooks.useMemo \_ ->
    Array.filter (String.contains (String.Pattern value.value)) value.options
  let error = filter (\_ -> not pristine) inp.error
  let
    selectProps =
      { itemCount: Array.length filtered
      , render: render
          { after
          , before
          , error
          , id
          , label
          , options: filtered
          , value: value.value
          }
      }
  Hooks.pure do
    HH.slot _select id Select.component selectProps case _ of
      Select.Selected index -> do
        putPristine false
        Hooks.raise outputToken $ Form.update value
          { value = fromMaybe value.value $ Array.index value.options index
          }
      Select.InputValueChanged value' -> do
        putPristine false
        Hooks.raise outputToken $ Form.update value { value = value' }
      Select.Emit parentAction ->
        Hooks.raise outputToken $ Form.raise parentAction
      _ -> pure unit
  where
  render { error, id, label, before, after, value, options } state =
    HH.div
      (Select.setRootProps [ classNames [ "relative", "inline-block" ] ])
      [ renderInput (Select.setInputProps [])
          { before
          , after
          , value
          , error
          , id: id
          , inputType: HP.InputText
          , label
          }
      , if state.isOpen then
          HH.div []
            [ HH.div (Select.setMenuProps [])
                $ options # Array.mapWithIndex \index item ->
                    HH.div (Select.setItemProps index [])
                      [ HH.text item
                      ]
            ]
        else
          HH.label
            [ classNames
                $ Css.inputError <> maybe [ "invisible" ] (const []) error
            , HP.for id
            ]
            [ HH.text $ fromMaybe "Valid" error ]
      ]

autocomplete
  :: forall parentAction s m e a
   . MonadAff m
  => String
  -> String
  -> (e -> String)
  -> AutocompleteInput
  -> Either e a
  -> FormM
       AutocompleteInput
       m
       ( FormHTML
           parentAction
           AutocompleteInput
           (AutocompleteSlots parentAction s)
           m
       )
autocomplete id label renderError value = case _ of
  Left e ->
    render $ Just $ renderError e
  Right _ ->
    render Nothing
  where
  props error =
    { after: []
    , before: []
    , error
    , id
    , label
    , value
    }
  render error = pure
    [ HH.slot _autocomplete id autocompleteComponent (props error) identity
    ]

inputAsync
  :: forall parentAction s m e a
   . Monad m
  => String
  -> String
  -> (e -> String)
  -> String
  -> RemoteData e a
  -> FormM String m (FormHTML parentAction String (InputSlots s) m)
inputAsync id label renderError value = case _ of
  Loading ->
    render $ Just "Checking..."
  Failure e ->
    render $ Just $ renderError e
  NotAsked ->
    render Nothing
  Success _ ->
    render Nothing
  where
  render error = pure
    [ HH.slot
        _input
        id
        inputComponent
        { inputType: HP.InputText
        , after: []
        , before: []
        , value
        , id
        , label
        , error
        , format: identity
        }
        Form.update
    ]

intInput
  :: forall parentAction s m e a
   . Monad m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML parentAction String (InputSlots s) m)
intInput id label renderError value = case _ of
  Left e ->
    render $ Just $ renderError e
  Right _ ->
    render Nothing
  where
  render error = pure
    [ HH.slot
        _input
        id
        inputComponent
        { inputType: HP.InputNumber
        , after: []
        , before: []
        , value
        , id
        , label
        , error
        , format: formatInt
        }
        Form.update
    ]

formatInt :: String -> String
formatInt s = case BigInt.fromString $ trim s of
  Nothing -> s
  Just n -> BigInt.toString n

adaInput
  :: forall parentAction s m e a
   . Monad m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML parentAction String (InputSlots s) m)
adaInput id label renderError value = case _ of
  Left e ->
    render $ Just $ renderError e
  Right _ ->
    render Nothing
  where
  render error = pure
    [ HH.slot
        _input
        id
        inputComponent
        { inputType: HP.InputNumber
        , after: []
        , before: [ HH.span_ [ HH.text "₳" ] ]
        , value
        , id
        , label
        , error
        , format: formatCurrency 6
        }
        Form.update
    ]

formatCurrency :: Int -> String -> String
formatCurrency decimals s =
  let
    { isNegative, absoluteValue } =
      if String.take 1 s == "-" then
        { isNegative: true, absoluteValue: String.drop 1 s }
      else
        { isNegative: false, absoluteValue: s }

    valueBits = Array.take 2 $ String.split (Pattern ".") absoluteValue

    decimalString =
      if absoluteValue == "" then "0" else fromMaybe "0" $ Array.head valueBits

    fractionalString =
      if Array.length valueBits < 2 then "0"
      else fromMaybe "0" $ Array.last valueBits

    -- if zeros have been deleted from the end of the string, the fractional part will be wrong
    correctedFractionalString = String.take decimals $ rightPadTo decimals "0"
      fractionalString

    multiplier = BigInt.fromInt $ Int.pow 10 decimals

    dec = fromMaybe zero $ BigInt.fromString decimalString

    frac = fromMaybe zero $ BigInt.fromString $ String.take decimals $
      correctedFractionalString
    value =
      if isNegative then
        -((dec * multiplier) + frac)
      else
        (dec * multiplier) + frac
    string =
      if value < zero then
        "-" <>
          (leftPadTo decimals "0" $ String.drop 1 $ BigInt.toString value)
      else
        leftPadTo decimals "0" $ BigInt.toString value

    len = String.length string

    { after: fractionalString, before } = String.splitAt (len - decimals)
      string

    decimalString = case before of
      "" -> "0"
      "-" -> "-0"
      _ -> before
  in
    if decimals == 0 then
      decimalString
    else
      decimalString <> "." <> fractionalString

input
  :: forall parentAction s m e a
   . Monad m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML parentAction String (InputSlots s) m)
input id label renderError value = case _ of
  Left e ->
    render $ Just $ renderError e
  Right _ ->
    render Nothing
  where
  render error = pure
    [ HH.slot
        _input
        id
        inputComponent
        { inputType: HP.InputText
        , after: []
        , before: []
        , value
        , id
        , label
        , error
        , format: identity
        }
        Form.update
    ]

walletNickname
  :: forall parentAction s m
   . Monad m
  => Set WalletNickname
  -> Form parentAction (InputSlots s) m String WalletNickname
walletNickname used =
  Form.mkForm
    { validator: WN.validatorExclusive used
    , render: input "wallet-nickname" "Wallet nickname" case _ of
        WN.Empty -> "Required."
        WN.Exists -> "Already exists."
        WN.DoesNotExist -> "Not found."
        WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."
    }

-- type MnemonicPhraseInput = Input String MnemonicPhraseError MnemonicPhrase

mnemonicPhrase
  :: forall parentAction s m
   . Monad m
  => Form parentAction (InputSlots s) m String MnemonicPhrase
mnemonicPhrase =
  Form.mkForm
    { validator: MP.validator
    , render: input "wallet-mnemonic" "Mnemonic phrase" case _ of
        MP.Empty -> "Required."
        MP.WrongWordCount -> "24 words required."
        MP.ContainsInvalidWords -> "Mnemonic phrase contains invalid words."
    }

address
  :: forall parentAction s m
   . Monad m
  => Set Address
  -> Form parentAction (InputSlots s) m String Address
address used =
  Form.mkForm
    { validator: A.validator used
    , render: input "address" "Address" case _ of
        A.Empty -> "Required."
        A.Invalid -> "Provided address is not a valid pubkeyhash."
        A.Exists -> "Already exists."
    }
