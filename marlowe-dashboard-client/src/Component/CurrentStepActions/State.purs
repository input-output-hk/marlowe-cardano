module Component.CurrentStepActions.State where

import Prologue

import Component.CurrentStepActions.Types (Action, DSL, Input)
import Component.CurrentStepActions.View (currentStepActions)
import Effect.Aff.Class (class MonadAff)
import Halogen as H

component
  :: forall query m
   . MonadAff m
  => H.Component query Input Void m
component =
  H.mkComponent
    { initialState: identity
    , render: currentStepActions
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

handleAction :: forall m. Action -> DSL m Unit
handleAction _ = pure unit
