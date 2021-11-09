module Component.LoadingSubmitButton.Lenses where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))

_caption :: forall s a. Lens' { caption :: a | s } a
_caption = prop (Proxy :: _ "caption")

_buttonHeight :: forall s a. Lens' { buttonHeight :: a | s } a
_buttonHeight = prop (Proxy :: _ "buttonHeight")

_styles :: forall s a. Lens' { styles :: a | s } a
_styles = prop (Proxy :: _ "styles")

_status :: forall s a. Lens' { status :: a | s } a
_status = prop (Proxy :: _ "status")

_enabled :: forall s a. Lens' { enabled :: a | s } a
_enabled = prop (Proxy :: _ "enabled")
