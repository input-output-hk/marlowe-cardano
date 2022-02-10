module Test.Web.Event.User.Options where

import Data.Maybe (Maybe)
import Data.Time.Duration (Seconds)
import Web.DOM (Document)

-- TODO support all options https://testing-library.com/docs/user-event/options

type UserOptions =
  { applyAccept :: Boolean
  , autoModify :: Boolean
  , delay :: Maybe Seconds
  , document :: Document
  , keyboardMap :: Array KeyboardKey
  }

type KeyboardKey =
  { -- | Code of the physical key.
    code :: Maybe String
  -- | Character or functional key descriptor
  , key :: Maybe String
  -- | Location on the keyboard for keys with multiple representations.
  , location :: Maybe DOMKeyLocation
  -- | Does the character in `key` require/imply AltRight to be pressed?
  , altGr :: Boolean
  -- | Does the character in `key` require/imply a shiftKey to be pressed?
  , shift :: Boolean
  }

data DOMKeyLocation
  = Standard
  | Left
  | Right
  | Numpad
