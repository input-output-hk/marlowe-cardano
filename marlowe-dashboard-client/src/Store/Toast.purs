module Store.Toast
  ( ToastAction(..)
  , ToastStore
  , emptyToastStore
  , getToasts
  , reduce
  ) where

import Prologue

import Component.Toast.Lenses (_expanded, _toasts)
import Component.Toast.Types (ToastEntry, ToastIndex(..), ToastMessage)
import Data.Lens (filtered, over, set, traversed)
import Data.List (List)
import Data.List as List

newtype ToastStore = ToastStore
  { toasts :: List ToastEntry
  , lastIndex :: ToastIndex
  }

derive instance Eq ToastStore

emptyToastStore :: ToastStore
emptyToastStore = ToastStore
  { toasts: mempty
  , lastIndex: ToastIndex 0
  }

getToasts :: ToastStore -> List ToastEntry
getToasts (ToastStore { toasts }) = toasts

compareEntry :: ToastEntry -> ToastEntry -> Ordering
compareEntry { index: indexA } { index: indexB } = compare indexA indexB

data ToastAction
  = Show ToastMessage
  | Clear ToastIndex
  | Expand ToastIndex

reduce :: ToastStore -> ToastAction -> ToastStore
reduce (ToastStore { toasts, lastIndex }) = case _ of
  Show message ->
    let
      newIndex = lastIndex + one
      toastEntry = { index: newIndex, message, expanded: false }
    in
      ToastStore
        { lastIndex: newIndex
        , toasts: List.insertBy compareEntry toastEntry toasts

        }
  Clear index ->
    ToastStore
      { toasts: List.filter (not <<< eq index <<< _.index) toasts
      , lastIndex
      }
  Expand index ->
    ToastStore
      { toasts: set
          (traversed <<< filtered (eq index <<< _.index) <<< _expanded)
          true
          toasts
      , lastIndex
      }
