module Store.OverlayState where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Variant (Variant)
import Data.Variant as Variant
import Halogen.Store.Monad (class MonadStore, updateStore)
import Prim.Row as Row
import Record as Record
import Type.Prelude (Proxy(..))

_overlayStateP = Proxy :: Proxy "overlayState"

_overlayState :: forall r. Lens' (State r) OverlayState
_overlayState = prop _overlayStateP

-- | Should we generalize this to something like `refCounter` component + helpers?
type OverlayState =
  { backdropCounter :: Int
  , overlayCounter :: Int
  }

insertInitialOverlayState
  :: forall r. Row.Lacks "overlayState" r => { | r } -> { | StateRow r }
insertInitialOverlayState = Record.insert _overlayStateP
  { backdropCounter: 0, overlayCounter: 0 }

data OverlayStateAction
  = UseBackdrop
  | ReleaseBackdrop
  | UseOverlay
  | ReleaseOverlay

-- | Overalay is a resource which can be asquired by multiple
-- | components so we implement "reference counting" here.
-- | If loadingOverlayCounter != 0 then we should display an overlay.
-- | If loadingOverlayCounter == 0 && backdropCounter > 0 then we
-- | should display a backdrop.
type StateRow r = (overlayState :: OverlayState | r)
type State r = { | StateRow r }

type ActionRow r = (overlayState :: OverlayStateAction | r)
type Action r = Variant (ActionRow r)

action :: forall acc. OverlayStateAction -> Action acc
action = Variant.inj _overlayStateP

modify :: forall st. (OverlayState -> OverlayState) -> State st -> State st
modify = Record.modify _overlayStateP

withBackdrop
  :: forall a acc m st. Monad m => MonadStore (Action acc) st m => m a -> m Unit
withBackdrop operation = do
  updateStore (action UseBackdrop)
  void $ operation
  updateStore (action ReleaseBackdrop)

withOverlay
  :: forall a acc m st. Monad m => MonadStore (Action acc) st m => m a -> m a
withOverlay operation = do
  updateStore (action UseOverlay)
  a <- operation
  updateStore (action ReleaseOverlay)
  pure a

reduce
  :: forall acc st
   . (Variant acc -> State st -> State st)
  -> Action acc
  -> State st
  -> State st
reduce = Variant.on _overlayStateP case _ of
  UseBackdrop -> modify (\st -> st { backdropCounter = st.backdropCounter + 1 })
  ReleaseBackdrop -> modify
    (\st -> st { backdropCounter = st.backdropCounter - 1 })
  UseOverlay -> modify (\st -> st { overlayCounter = st.overlayCounter + 1 })
  ReleaseOverlay -> modify
    (\st -> st { overlayCounter = st.overlayCounter - 1 })
