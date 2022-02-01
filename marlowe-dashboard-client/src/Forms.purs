module Forms where

import Prologue hiding (div)

import Component.FormInput as Input
import Control.Alternative (guard)
import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import Data.Address (Address)
import Data.Address (AddressError(..), validator) as A
import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Int as Int
import Data.Maybe (fromMaybe, maybe)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.Set (Set)
import Data.String (Pattern(..), trim)
import Data.String as String
import Data.String.Extra (leftPadTo, rightPadTo)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Class (class MonadEffect)
import Halogen.Css (classNames)
import Halogen.Form (Form, FormHTML)
import Halogen.Form as Form
import Halogen.Form.FormM (FormM)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))

type InputSlots pa slots =
  (input :: Input.Slot pa String | slots)

_input :: Proxy "input"
_input = Proxy

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
  :: forall w pa slots m
   . String
  -> Input.State
  -> Array (HP.IProp HTMLinput (Input.Action pa slots m))
  -> HH.HTML w (Input.Action pa slots m)
renderInput id { value } props = HH.input
  ( Input.setInputProps value $ props <> [ HP.id id, classNames Css.inputText ]
  )

mountInput
  :: forall pa slots m
   . MonadEffect m
  => { format :: String -> String
     , id :: String
     , value :: String
     }
  -> (Input.State -> Input.ComponentHTML pa slots m)
  -> Form.ComponentHTML pa String (InputSlots pa slots) m
mountInput { id, value, format } render =
  HH.slot _input id Input.component { value, render, format } $
    case _ of
      Input.Updated newValue -> Form.update newValue
      Input.Emit pa -> Form.raise pa
      _ -> Form.ignore

inputAsync
  :: forall pa s m e a
   . MonadEffect m
  => String
  -> String
  -> (e -> String)
  -> String
  -> RemoteData e a
  -> FormM String m (FormHTML pa String (InputSlots pa s) m)
inputAsync id label renderError value remote = pure
  [ mountInput { id, value, format: identity } \state ->
      let
        error = guard (not state.pristine && state.visited) *> case remote of
          Loading ->
            Just "Checking..."
          Failure e ->
            Just $ renderError e
          NotAsked ->
            Nothing
          Success _ ->
            Nothing
      in
        HH.div [ classNames [ "relative" ] ]
          [ renderLabel id label
          , renderInputBox error [ renderInput id state [] ]
          , renderErrorLabel id error
          ]
  ]

intInput
  :: forall pa s m e a
   . MonadEffect m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML pa String (InputSlots pa s) m)
intInput id label renderError value result = pure
  [ mountInput { id, value, format: formatInt } \state ->
      let
        error = guard (not state.pristine && state.visited) *> case result of
          Left e ->
            Just $ renderError e
          Right _ ->
            Nothing
      in
        HH.div [ classNames [ "relative" ] ]
          [ renderLabel id label
          , renderInputBox error
              [ renderInput id state [ HP.type_ HP.InputNumber ]
              ]
          , renderErrorLabel id error
          ]
  ]

formatInt :: String -> String
formatInt s = case BigInt.fromString $ trim s of
  Nothing -> s
  Just n -> BigInt.toString n

adaInput
  :: forall pa s m e a
   . MonadEffect m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML pa String (InputSlots pa s) m)
adaInput id label renderError value result = pure
  [ mountInput { id, value, format: formatCurrency 6 } \state ->
      let
        error = guard (not state.pristine && state.visited) *> case result of
          Left e ->
            Just $ renderError e
          Right _ ->
            Nothing
      in
        HH.div [ classNames [ "relative" ] ]
          [ renderLabel id label
          , renderInputBox error
              [ HH.span_ [ HH.text "₳" ]
              , renderInput id state [ HP.type_ HP.InputNumber ]
              ]
          , renderErrorLabel id error
          ]
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
  :: forall pa s m e a
   . MonadEffect m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML pa String (InputSlots pa s) m)
input id label renderError value result = pure
  [ mountInput { id, value, format: identity } \state ->
      let
        error = guard (not state.pristine && state.visited) *> case result of
          Left e ->
            Just $ renderError e
          Right _ ->
            Nothing
      in
        HH.div [ classNames [ "relative" ] ]
          [ renderLabel id label
          , renderInputBox error [ renderInput id state [] ]
          , renderErrorLabel id error
          ]
  ]

walletNickname
  :: forall pa s m
   . MonadEffect m
  => Set WalletNickname
  -> Form pa (InputSlots pa s) m String WalletNickname
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
  :: forall pa s m
   . MonadEffect m
  => Form pa (InputSlots pa s) m String MnemonicPhrase
mnemonicPhrase =
  Form.mkForm
    { validator: MP.validator
    , render: input "wallet-mnemonic" "Mnemonic phrase" case _ of
        MP.Empty -> "Required."
        MP.WrongWordCount -> "24 words required."
        MP.ContainsInvalidWords -> "Mnemonic phrase contains invalid words."
    }

address
  :: forall pa s m
   . MonadEffect m
  => Set Address
  -> Form pa (InputSlots pa s) m String Address
address used =
  Form.mkForm
    { validator: A.validator used
    , render: input "address" "Address" case _ of
        A.Empty -> "Required."
        A.Invalid -> "Provided address is not a valid pubkeyhash."
        A.Exists -> "Already exists."
    }
