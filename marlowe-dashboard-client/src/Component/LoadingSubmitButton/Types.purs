module Component.LoadingSubmitButton.Types where

import Prologue
import Type.Proxy (Proxy(..))
import Data.Time.Duration (Milliseconds)
import Halogen (RefLabel(..))
import Network.RemoteData (RemoteData)

_submitButtonSlot :: Proxy "submitButtonSlot"
_submitButtonSlot = Proxy

type State
  =
  { caption :: String
  , styles :: Array String
  , enabled :: Boolean
  , status :: RemoteData String String
  , buttonHeight :: Number
  }

type Input =
  { caption :: String
  , styles :: Array String
  , enabled :: Boolean
  }

data Query a
  = SubmitResult Milliseconds (Either String String) a

data Action
  = Submit
  | OnNewInput Input

buttonRef :: RefLabel
buttonRef = RefLabel "button"
