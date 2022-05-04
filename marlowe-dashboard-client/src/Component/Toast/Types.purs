module Component.Toast.Types
  ( Action(..)
  , State
  , ToastMessage
  , ToastState
  , errorToast
  , explainableErrorToast
  , infoToast
  , successToast
  ) where

import Prologue

import Analytics (class IsEvent, Event)
import Analytics as A
import Component.Icons (Icon(..))
import Errors.Explain (class Explain, explainString)
import Halogen (SubscriptionId)
import Web.ARIA (ARIARole(..))

type ToastMessage =
  { shortDescription :: String
  , longDescription :: Maybe String
  , icon :: Icon
  , iconColor :: String
  , textColor :: String
  , bgColor :: String
  , timeout :: Number
  , role :: ARIARole
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
  , role: Status
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
  , role: Status
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
  , role: Alert
  }

explainableErrorToast :: forall a. Explain a => String -> a -> ToastMessage
explainableErrorToast shortDescription error = errorToast shortDescription
  $ Just
  $ explainString error

contactSupportMessage :: String
contactSupportMessage = "Please contact support if the problem persists."
