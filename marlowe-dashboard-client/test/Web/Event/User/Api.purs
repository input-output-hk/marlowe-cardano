module Test.Web.Event.User.Api where

import Prelude

import Control.Promise (Promise)
import Data.Undefinable (Undefinable)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.File.File (File)
import Web.HTML (HTMLElement)
import Web.HTML.Event.DataTransfer (DataTransfer)

data ClipboardData

class IsClipboardData a where
  toClipboardData :: a -> ClipboardData

instance IsClipboardData DataTransfer where
  toClipboardData = unsafeCoerce

instance IsClipboardData String where
  toClipboardData = unsafeCoerce

data PointerActionInput
data PointerInput

-- TODO define the rest of the pointer action inputs
-- https://github.com/testing-library/user-event/blob/beta/src/pointer/index.ts

class IsPointerActionInput a where
  toPointerActionInput :: a -> PointerActionInput

instance IsPointerActionInput String where
  toPointerActionInput = unsafeCoerce

class IsPointerInput a where
  toPointerInput :: a -> PointerInput

instance IsPointerActionInput a => IsPointerInput (Array a) where
  toPointerInput = unsafeCoerce
else instance IsPointerActionInput a => IsPointerInput a where
  toPointerInput = unsafeCoerce

data SelectOptions

class IsSelectOptions a where
  toSelectOptions :: a -> SelectOptions

instance IsSelectOptions HTMLElement where
  toSelectOptions = unsafeCoerce

instance IsSelectOptions (Array HTMLElement) where
  toSelectOptions = unsafeCoerce

instance IsSelectOptions String where
  toSelectOptions = unsafeCoerce

instance IsSelectOptions (Array String) where
  toSelectOptions = unsafeCoerce

data FileOrFiles

class IsFileOrFiles a where
  toFileOrFiles :: a -> FileOrFiles

instance IsFileOrFiles File where
  toFileOrFiles = unsafeCoerce

instance IsFileOrFiles (Array File) where
  toFileOrFiles = unsafeCoerce

type TypeOptions =
  { skipClick :: Boolean
  , skipAutoClose :: Boolean
  , initialSelectionStart :: Undefinable Int
  , initialSelectionEnd :: Undefinable Int
  }

type UserApi =
  { click :: Element -> Effect (Promise Unit)
  , dblClick :: Element -> Effect (Promise Unit)
  , tripleClick :: Element -> Effect (Promise Unit)
  , hover :: Element -> Effect (Promise Unit)
  , unhover :: Element -> Effect (Promise Unit)
  , tab :: { shift :: Boolean } -> Effect (Promise Unit)
  , keyboard :: String -> Effect (Promise Unit)
  , copy :: Effect (Promise Unit)
  , cut :: Effect (Promise Unit)
  , paste :: ClipboardData -> Effect (Promise Unit)
  , pointer :: PointerInput -> Effect (Promise Unit)
  , clear :: Element -> Effect (Promise Unit)
  , deselectOptions :: Element -> SelectOptions -> Effect (Promise Unit)
  , selectOptions :: Element -> SelectOptions -> Effect (Promise Unit)
  , type ::
      Element -> String -> Undefinable TypeOptions -> Effect (Promise Unit)
  , upload :: HTMLElement -> FileOrFiles -> Effect (Promise Unit)
  }
