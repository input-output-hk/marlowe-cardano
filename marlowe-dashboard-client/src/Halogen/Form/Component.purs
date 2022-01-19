-- TODO move this to its own library
module Halogen.Form.Component where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (Form, FormHTML, runForm)
import Halogen.Form as Form
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
import Web.Event.Event (Event, preventDefault)

type Spec parentAction slots m input output =
  { form :: Form parentAction slots m input output
  , formClasses :: Array String
  }

data Msg parentAction output
  = Updated (Maybe output)
  | Raised parentAction

data Action parentAction slots m input output
  = PublicAction (Form.Action parentAction input)
  | Init
  | OnSubmit Event
  | UpdateResult (Maybe output) (FormHTML parentAction input slots m)
  | UpdateFromFormM
      (HS.Listener (Action parentAction slots m input output))
      input

update
  :: forall parentAction slots m input output
   . input
  -> Action parentAction slots m input output
update = PublicAction <<< Form.Update

raise
  :: forall parentAction slots m input output
   . parentAction
  -> Action parentAction slots m input output
raise = PublicAction <<< Form.Raise

type Component query parentAction m input output =
  H.Component query input (Msg parentAction output) m

type ComponentHTML parentAction slots m input output =
  H.ComponentHTML (Action parentAction slots m input output) slots m

component
  :: forall query parentAction slots m input output
   . MonadAff m
  => Eq input
  => Eq output
  => Show input
  => Spec parentAction slots m input output
  -> Component query parentAction m input output

component spec = H.mkComponent
  { render
  , initialState
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Init
      , receive = Just <<< PublicAction <<< Form.Update
      }
  }
  where
  render { formHtml } = formHtml
  mkFormHtml children = HH.form
    [ classNames spec.formClasses, HE.onSubmit OnSubmit ]
    children
  initialState input =
    { input
    , formHtml: mkFormHtml []
    , subscription: Nothing
    }
  handleAction = case _ of
    PublicAction (Form.Update input) -> do
      H.modify_ _ { input = input }
      handleUpdateWithNewSubscription
    PublicAction (Form.Raise parentAction) -> H.raise $ Raised parentAction
    Init -> handleUpdateWithNewSubscription
    OnSubmit event -> liftEffect $ preventDefault event
    UpdateFromFormM listener input -> do
      H.modify_ _ { input = input }
      handleUpdate listener
    UpdateResult result children -> do
      H.modify_ _
        { formHtml =
            mkFormHtml $ map (bimap (map PublicAction) PublicAction) children
        }
      H.raise $ Updated result
  handleUpdateWithNewSubscription = do
    subscription <- H.gets _.subscription
    traverse_ H.unsubscribe subscription
    { listener, emitter } <- liftEffect HS.create
    subscription' <- H.subscribe emitter
    H.modify_ _ { subscription = Just subscription' }
    handleUpdate listener
  handleUpdate listener = do
    input <- H.gets _.input
    Tuple result children <-
      lift
        $ runForm spec.form input
        $ liftEffect <<< HS.notify listener <<< UpdateFromFormM listener
    liftEffect $ HS.notify listener $ UpdateResult result children
