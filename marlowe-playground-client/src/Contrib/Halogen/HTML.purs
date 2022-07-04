module Contrib.Halogen.HTML where

import Data.Void (Void)
import Halogen.HTML (HTML)
import Unsafe.Coerce (unsafeCoerce)

-- | Like `PlainHTML` but tagged with actions.
type PlainHTML' i = HTML Void i

fromPlainHTML' :: forall i w. PlainHTML' i -> HTML w i
fromPlainHTML' = unsafeCoerce
