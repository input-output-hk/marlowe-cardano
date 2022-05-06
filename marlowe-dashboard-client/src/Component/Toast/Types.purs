module Component.Toast.Types
  ( Action(..)
  , State
  , ToastMessage
  , ToastEntry
  , ToastIndex(..)
  , errorToast
  , explainableErrorToast
  , infoToast
  , successToast
  ) where

import Prologue

import Component.Icons (Icon(..))
import Data.List (List)
import Data.Map (Map)
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

-- Because we can show multiple Toast at the same time we use
-- an index to identify the different messages.
newtype ToastIndex = ToastIndex Int

derive newtype instance Semiring ToastIndex
derive newtype instance Eq ToastIndex
derive newtype instance Ord ToastIndex

type ToastEntry =
  { index :: ToastIndex
  , message :: ToastMessage
  , expanded :: Boolean
  }

data Action
  = Receive (List ToastEntry)
  | ExpandToast ToastIndex
  | CloseToast ToastIndex
  | ToastTimeout ToastIndex

type State =
  { toasts :: List ToastEntry
  , timeoutSubscriptions :: Map ToastIndex SubscriptionId
  }

successToast :: String -> ToastMessage
successToast shortDescription =
  { shortDescription
  , longDescription: Nothing
  , icon: DoneWithCircle
  , iconColor: "text-lightgreen"
  , textColor: "text-white"
  , bgColor: "bg-black"
  , timeout: 25000.0
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
  , timeout: 50000.0
  , role: Alert
  }

explainableErrorToast :: forall a. Explain a => String -> a -> ToastMessage
explainableErrorToast shortDescription error = errorToast shortDescription
  $ Just
  $ explainString error

contactSupportMessage :: String
contactSupportMessage = "Please contact support if the problem persists."
