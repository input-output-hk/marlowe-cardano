module Component.CurrentStepActions.State
  ( component
  ) where

import Prologue

import Component.CurrentStepActions.Types (Action(..), DSL, Input, Msg(..))
import Component.CurrentStepActions.View (currentStepActions)
import Data.Map as Map
import Data.Maybe (maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen (raise)
import Halogen as H
import Halogen.Component.Reactive as HR
import Record as Record
import Type.Proxy (Proxy(..))

component
  :: forall query m
   . MonadAff m
  => H.Component query Input Msg m
component =
  HR.mkReactiveComponent
    { deriveState: \input ->
        maybe
          (Record.insert (Proxy :: _ "choiceValues") Map.empty input)
          (Record.merge input)
    , render: currentStepActions
    , eval: HR.defaultReactiveEval
        { handleAction = handleAction
        }
    }

handleAction :: forall m. Action -> DSL m Unit
handleAction (SelectAction namedAction chosenNum) =
  raise $ ActionSelected namedAction chosenNum

handleAction (ChangeChoice choiceId chosenNum) =
  H.modify_ \s ->
    s
      { choiceValues = s.choiceValues
          # maybe (Map.delete choiceId) (Map.insert choiceId) chosenNum
      }
