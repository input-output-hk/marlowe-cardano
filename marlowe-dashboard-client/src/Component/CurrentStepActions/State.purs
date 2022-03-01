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
import Record as Record
import Type.Proxy (Proxy(..))

component
  :: forall query m
   . MonadAff m
  => H.Component query Input Msg m
component =
  H.mkComponent
    { initialState: Record.insert (Proxy :: _ "choiceValues") Map.empty
    , render: currentStepActions
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< OnReceive
        }
    }

handleAction :: forall m. Action -> DSL m Unit
handleAction (OnReceive input) = H.modify_ $ Record.merge input

handleAction (SelectAction namedAction chosenNum) =
  raise $ ActionSelected namedAction chosenNum

handleAction (ChangeChoice choiceId chosenNum) =
  H.modify_ \s ->
    s
      { choiceValues = s.choiceValues
          # maybe (Map.delete choiceId) (Map.insert choiceId) chosenNum
      }
