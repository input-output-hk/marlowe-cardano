module Toast.Types
  ( ToastMessage
  , Action(..)
  , ToastState
  , State
  , infoToast
  , successToast
  , errorToast
  , ajaxErrorToast
  , decodingErrorToast
  ) where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.Icons (Icon(..))
import Data.Argonaut.Decode (JsonDecodeError)
import Halogen (SubscriptionId)
import Types (JsonAjaxError)

type ToastMessage =
  { shortDescription :: String
  , longDescription :: Maybe String
  , icon :: Icon
  , iconColor :: String
  , textColor :: String
  , bgColor :: String
  , timeout :: Number
  }

data Action
  = Receive (Maybe ToastMessage)
  | ExpandToast
  | CloseToast
  | ToastTimeout

defaultEvent :: String -> Event
defaultEvent s = A.defaultEvent $ "Toast." <> s

instance actionIsEvent :: IsEvent Action where
  toEvent (Receive _) = Just $ defaultEvent "Receive"
  toEvent ExpandToast = Just $ defaultEvent "ExpandToast"
  toEvent CloseToast = Just $ defaultEvent "CloseToast"
  toEvent ToastTimeout = Just $ defaultEvent "ToastTimeout"

-- TODO: For now the state and actions can only represent a single toast. If you open a new toast
--       it will replace the current one. We could later on extend this to have multiple messages
type ToastState =
  { message :: ToastMessage
  , expanded :: Boolean
  }

type State =
  { mToast :: Maybe ToastState
  , timeoutSubscription :: Maybe SubscriptionId
  }

successToast :: String -> ToastMessage
successToast shortDescription =
  { shortDescription
  , longDescription: Nothing
  , icon: DoneWithCircle
  , iconColor: "text-lightgreen"
  , textColor: "text-white"
  , bgColor: "bg-black"
  , timeout: 2500.0
  }

infoToast :: String -> ToastMessage
infoToast shortDescription =
  { shortDescription
  , longDescription: Nothing
  , icon: Info
  , iconColor: "text-lightpurple"
  , textColor: "text-white"
  , bgColor: "bg-black"
  , timeout: 2500.0
  }

errorToast :: String -> Maybe String -> ToastMessage
errorToast shortDescription longDescription =
  { shortDescription
  , longDescription: map (\m -> m <> " " <> contactSupportMessage)
      longDescription
  , icon: ErrorOutline
  , iconColor: "text-white"
  , textColor: "text-white"
  , bgColor: "bg-red"
  , timeout: 5000.0
  }

ajaxErrorToast :: String -> JsonAjaxError -> ToastMessage
ajaxErrorToast shortDescription _ = errorToast shortDescription $ Just
  "A request was made to the server, but the expected response was not returned."

decodingErrorToast :: String -> JsonDecodeError -> ToastMessage
decodingErrorToast shortDescription _ = errorToast shortDescription $ Just
  "Some data was received from the server, but the browser was unable to parse it."

contactSupportMessage :: String
contactSupportMessage = "Please contact support if the problem persists."
