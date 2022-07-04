module Gists.View
  ( idPublishGist
  , idLoadGist
  ) where

import Prologue hiding (div)

import Auth (AuthRole(..), AuthStatus, authStatusAuthRole)
import Bootstrap
  ( btn
  , btnDanger
  , btnSecondary
  , btnSmall
  , empty
  , formControl
  , formGroup
  , isInvalid
  , isValid
  , nbsp
  )
import Component.ErrorPane (closeableErrorPane)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Lens (view)
import Data.Maybe (fromMaybe)
import Gist (Gist, gistHtmlUrl)
import Gists.Extra (GistId)
import Gists.Types (GistAction(..), parseGistUrl)
import Halogen.HTML
  ( ClassName(ClassName)
  , HTML
  , IProp
  , a
  , button
  , div
  , input
  , label
  , text
  )
import Halogen.HTML.Events (onClick, onValueInput)
import Halogen.HTML.Properties
  ( class_
  , classes
  , disabled
  , for
  , href
  , id
  , target
  , type_
  , value
  )
import Icons (Icon(..), icon)
import Network.RemoteData (RemoteData(NotAsked, Loading, Failure, Success))
import Servant.PureScript (printAjaxError)
import Types (WebData)

idPublishGist :: forall r i. IProp (id :: String | r) i
idPublishGist = id "publish-gist"

idLoadGist :: forall r i. IProp (id :: String | r) i
idLoadGist = id "load-gist"

