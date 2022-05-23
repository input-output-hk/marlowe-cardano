module Component.Toast.Types
  ( Action(..)
  , State
  , ToastEntry
  , ToastIndex(..)
  , ToastMessage
  , errorToast
  , explainableErrorToast
  , indexRef
  , infoToast
  , successToast
  ) where

import Prologue

import Component.Icons (Icon(..))
import Data.List (List)
import Data.Map (Map)
import Data.Time.Duration (Milliseconds(..))
import Errors.Explain (class Explain, explain)
import Halogen (SubscriptionId)
import Text.Pretty (Doc(..), newline)
import Web.ARIA (ARIARole(..))

type ToastMessage =
  { shortDescription :: String
  -- TODO SCP-3962 use dodo-printer instead of Text.Pretty
  -- for this.
  , longDescription :: Maybe Doc
  , icon :: Icon
  , iconColor :: String
  , textColor :: String
  , bgColor :: String
  , timeout :: Maybe Milliseconds
  , role :: ARIARole
  }

-- Because we can show multiple Toast at the same time we use
-- an index to identify the different messages.
newtype ToastIndex = ToastIndex Int

derive newtype instance Semiring ToastIndex
derive newtype instance Eq ToastIndex
derive newtype instance Ord ToastIndex

indexRef :: String -> ToastIndex -> String
indexRef pre (ToastIndex n) = pre <> "-" <> show n

type ToastEntry =
  { index :: ToastIndex
  , message :: ToastMessage
  , expanded :: Boolean
  }

data Action
  = Receive (List ToastEntry)
  | ToggleExpanded ToastIndex
  | CloseToast ToastIndex
  | AnimateCloseToast ToastIndex

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
  , timeout: Just $ Milliseconds 5000.0
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
  , timeout: Just $ Milliseconds 5000.0
  , role: Status
  }

errorToast :: String -> Maybe Doc -> ToastMessage
errorToast shortDescription longDescription =
  { shortDescription
  , longDescription: map
      (\m -> m <> Newline 0 <> Newline 0 <> Text contactSupportMessage)
      longDescription
  , icon: ErrorOutline
  , iconColor: "text-white"
  , textColor: "text-white"
  , bgColor: "bg-red"
  , timeout: Nothing
  , role: Alert
  }

explainableErrorToast :: forall a. Explain a => String -> a -> ToastMessage
explainableErrorToast shortDescription error = errorToast shortDescription
  $ Just
  $ Text (shortDescription <> ":") <> newline <> newline <> explain error

contactSupportMessage :: String
contactSupportMessage = "Please contact support if the problem persists."
