module Component.InputField.Lenses
  ( _value
  , _pristine
  , _validator
  , _dropdownOpen
  , _dropdownLocked
  , _additionalCss
  , _id_
  , _placeholder
  , _readOnly
  , _valueOptions
  , _numberFormat
  , _after
  , _before
  ) where

import Prologue
import Component.InputField.Types (InputDisplayOptions, State)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Halogen.HTML (HTML)
import Marlowe.Extended.Metadata (NumberFormat)

_value :: forall e. Lens' (State e) String
_value = prop (Proxy :: _ "value")

_pristine :: forall e. Lens' (State e) Boolean
_pristine = prop (Proxy :: _ "pristine")

_validator :: forall e. Lens' (State e) (String -> Maybe e)
_validator = prop (Proxy :: _ "validator")

_dropdownOpen :: forall e. Lens' (State e) Boolean
_dropdownOpen = prop (Proxy :: _ "dropdownOpen")

_dropdownLocked :: forall e. Lens' (State e) Boolean
_dropdownLocked = prop (Proxy :: _ "dropdownLocked")

------------------------------------------------------------
_additionalCss :: forall w i. Lens' (InputDisplayOptions w i) (Array String)
_additionalCss = prop (Proxy :: _ "additionalCss")

_id_ :: forall w i. Lens' (InputDisplayOptions w i) String
_id_ = prop (Proxy :: _ "id_")

_placeholder :: forall w i. Lens' (InputDisplayOptions w i) String
_placeholder = prop (Proxy :: _ "placeholder")

_readOnly :: forall w i. Lens' (InputDisplayOptions w i) Boolean
_readOnly = prop (Proxy :: _ "readOnly")

_valueOptions :: forall w i. Lens' (InputDisplayOptions w i) (Array String)
_valueOptions = prop (Proxy :: _ "valueOptions")

_numberFormat
  :: forall w i. Lens' (InputDisplayOptions w i) (Maybe NumberFormat)
_numberFormat = prop (Proxy :: _ "numberFormat")

_after :: forall w i. Lens' (InputDisplayOptions w i) (Maybe (HTML w i))
_after = prop (Proxy :: _ "after")

_before :: forall w i. Lens' (InputDisplayOptions w i) (Maybe (HTML w i))
_before = prop (Proxy :: _ "before")
