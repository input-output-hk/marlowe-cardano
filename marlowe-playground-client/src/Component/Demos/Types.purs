module Component.Demos.Types where

import Prologue

import Analytics (class IsEvent)
import Data.Newtype (class Newtype)
import Project (Workflow)

newtype Demo = Demo String

derive instance newtypeDemo :: Newtype Demo _

data Action
  = LoadDemo Workflow Demo
  | Cancel

instance isEventAction :: IsEvent Action where
  toEvent (LoadDemo lang _) = Just
    { category: Just "Demos"
    , action: "LoadDemo"
    , label: Just (show lang)
    , value: Nothing
    }
  toEvent Cancel = Just
    { category: Just "Demos", action: "Cancel", label: Nothing, value: Nothing }
