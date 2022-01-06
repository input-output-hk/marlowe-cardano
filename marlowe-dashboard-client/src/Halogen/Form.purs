-- TODO move this to its own library
module Halogen.Form where

import Prelude

import Control.Monad.Maybe.Trans (mapMaybeT)
import Control.Monad.State (gets, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (mapWriterT)
import Data.Bifunctor (bimap, lmap)
import Data.Lens (Lens', set, view)
import Data.Maybe (Maybe(..))
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.Component (hoistSlot)
import Halogen.HTML as HH
import Polyform (Reporter(..))
import Polyform.Reporter (runReporter)

data Msg action a
  = Updated (Maybe a)
  | Escalated action

data Action action input
  = Update input
  | Escalate action

type ComponentHTML action input slots m =
  H.ComponentHTML (Action action input) slots m

type Form action input slots m a =
  Reporter m (Array (ComponentHTML action input slots m)) input a

subform
  :: forall ac i j s m a
   . Functor m
  => Lens' i j
  -> Form ac j s m a
  -> Form ac i s m a
subform lens (Reporter (Star f)) = Reporter $ Star \i ->
  let
    adaptAction (Update j) = Update (set lens j i)
    adaptAction (Escalate ac) = Escalate ac
  in
    mapMaybeT
      (mapWriterT (map (map (map (bimap (map adaptAction) adaptAction)))))
      (f (view lens i))

hoistForm
  :: forall ac i s m1 m2
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> Form ac i s m1 ~> Form ac i s m2
hoistForm a (Reporter (Star f)) =
  Reporter
    $ Star
    $ mapMaybeT (mapWriterT $ a <<< map (map (map (lmap (hoistSlot a))))) <<< f

data InternalAction action input
  = Init
  | Receive input
  | PublicAction (Action action input)

component
  :: forall q i ac a m s
   . Monad m
  => Form ac i s m a
  -> H.Component q i (Msg ac a) m
component reporter =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Init
        , receive = Just <<< Receive
        }
    }
  where
  initialState input = { input: input, children: [], result: Nothing }
  render { children } =
    HH.form [] $ bimap (map PublicAction) PublicAction <$> children
  handleInput input = do
    modify_ _ { input = input }
    Tuple result children <- lift (runReporter reporter input)
    previousResult <- gets _.result
    modify_ _ { children = children, result = result }
    case previousResult, result of
      Nothing, Nothing -> pure unit
      _, _ -> H.raise (Updated result)

  handleAction = case _ of
    Init -> gets _.input >>= handleInput
    Receive input -> handleInput input
    PublicAction (Update input) -> handleInput input
    PublicAction (Escalate action) -> H.raise (Escalated action)
