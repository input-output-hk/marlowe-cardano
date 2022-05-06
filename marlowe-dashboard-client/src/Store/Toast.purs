module Store.Toast
  ( ToastAction(..)
  , ToastStore
  , emptyToastStore
  , getToasts
  , reduce
  ) where

import Prologue

import Component.Toast.Types (ToastEntry, ToastIndex(..), ToastMessage)
import Data.List (List)
import Data.List as List
import Data.Tuple.Nested ((/\))

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
compareEntry (indexA /\ _) (indexB /\ _) = compare indexA indexB

data ToastAction
  = Show ToastMessage
  | Clear ToastIndex

reduce :: ToastStore -> ToastAction -> ToastStore
reduce (ToastStore { toasts, lastIndex }) = case _ of
  Show msg ->
    let
      ToastIndex lastIndex' = lastIndex
      newIndex = ToastIndex $ lastIndex' + 1
    in
      ToastStore
        { lastIndex: newIndex
        , toasts: List.insertBy compareEntry (newIndex /\ msg) toasts

        }
  Clear index ->
    ToastStore
      { toasts: List.filter (not <<< eq index <<< fst) toasts
      , lastIndex
      }
