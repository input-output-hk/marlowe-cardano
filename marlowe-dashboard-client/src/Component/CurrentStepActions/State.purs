module Component.CurrentStepActions.State
  ( component
  ) where

import Prologue

import Component.CurrentStepActions.Types (Action(..), DSL, Input, Msg(..))
import Component.CurrentStepActions.View (currentStepActions)
import Data.Lens (modifying)
import Data.UserNamedActions (_Actions)
import Effect.Aff.Class (class MonadAff)
import Halogen (raise)
import Halogen as H
import Marlowe.Execution.Types (NamedAction(..))
import Page.Contract.Lenses (_namedActions)

component
  :: forall query m
   . MonadAff m
  => H.Component query Input Msg m
component =
  H.mkComponent
    { initialState: identity
    , render: currentStepActions
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }

handleAction :: forall m. Action -> DSL m Unit
handleAction (SelectAction namedAction) = raise $ ActionSelected namedAction
handleAction (ChangeChoice choiceId chosenNum) = modifying
  (_namedActions <<< _Actions)
  changeChoice
  where
  changeChoice (MakeChoice choiceId' bounds _)
    | choiceId == choiceId' = MakeChoice choiceId bounds chosenNum

  changeChoice namedAction = namedAction

