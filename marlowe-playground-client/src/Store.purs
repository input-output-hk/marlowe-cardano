module Store
  ( module Store
  , module Store.ProjectState
  ) where

import Prelude

import Data.Lens (Iso')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Variant (Variant, case_)
import Data.Variant as Variant
import Project (Project)
import Store.AuthState as AuthState
import Store.OverlayState as OverlayState
import Store.ProjectState (ProjectState, ProjectStateAction)
import Store.ProjectState as ProjectState
import Type.Prelude (Proxy(..))
import Type.Row (type (+))

type StateRow =
  ( AuthState.StateRow
      + OverlayState.StateRow
      + ProjectState.StateRow
      + ()
  )

type StateRecord = { | StateRow }
newtype State = State StateRecord

-- | Reset action which is used during logout
_resetP = Proxy :: Proxy "reset"

reset :: Action
reset = Variant.inj _resetP unit

-- | Compiler bug?
-- |
-- | type ActionRow = ( OverlayState.ActionRow + AuthState.ActionRow + ProjectState.ActionRow + ())
-- |
-- | The above fails with:
-- |
-- | "PartiallyAppliedSynonym" (unknown module)
-- | Type synonym Type.Row.RowApply is partially applied.
-- |
type ActionRow =
  ( OverlayState.ActionRow
      (AuthState.ActionRow (ProjectState.ActionRow (reset :: Unit)))
  )

type Action = Variant ActionRow

derive instance Newtype State _

_State
  :: Iso' State
       { | AuthState.StateRow + OverlayState.StateRow + ProjectState.StateRow +
           ()
       }
_State = _Newtype

mkStore
  :: Maybe Project
  -> State
mkStore project = State
  $ OverlayState.insertInitialOverlayState
  $ ProjectState.insertInitialProjectState project
  $ AuthState.insertInitialAuthState
  $ {}

reduce :: State -> Action -> State
reduce (State st) action = do
  let
    reduce' = case_
      # OverlayState.reduce
      # ProjectState.reduce
      # AuthState.reduce
      # Variant.on _resetP (\_ _ -> un State (mkStore Nothing))
  State $ reduce' action st
