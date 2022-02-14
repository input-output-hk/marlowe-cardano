module Test.Web.Event.User.Api where

import Prelude

import Control.Promise (Promise)
import Data.Undefinable (Undefinable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3)
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
  { click :: EffectFn1 Element (Promise Unit)
  , dblClick :: EffectFn1 Element (Promise Unit)
  , tripleClick :: EffectFn1 Element (Promise Unit)
  , hover :: EffectFn1 Element (Promise Unit)
  , unhover :: EffectFn1 Element (Promise Unit)
  , tab :: EffectFn1 { shift :: Boolean } (Promise Unit)
  , keyboard :: EffectFn1 String (Promise Unit)
  , copy :: Effect (Promise Unit)
  , cut :: Effect (Promise Unit)
  , paste :: EffectFn1 ClipboardData (Promise Unit)
  , pointer :: EffectFn1 PointerInput (Promise Unit)
  , clear :: EffectFn1 Element (Promise Unit)
  , deselectOptions :: EffectFn2 Element SelectOptions (Promise Unit)
  , selectOptions :: EffectFn2 Element SelectOptions (Promise Unit)
  , type :: EffectFn3 Element String (Undefinable TypeOptions) (Promise Unit)
  , upload :: EffectFn2 HTMLElement FileOrFiles (Promise Unit)
  }
