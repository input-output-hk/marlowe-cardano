-- TODO move this to its own library
module Halogen.Form
  ( Input
  , Msg
  , Action(..)
  , ComponentHTML
  , Component
  , Form(..)
  , Slot
  , split
  , subform
  , hoistForm
  , component
  ) where

import Prelude

import Control.Monad.Maybe.Trans (mapMaybeT)
import Control.Monad.State (get, gets, modify_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (mapWriterT)
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Lens (Lens', _1, _2, set, view)
import Data.Maybe (Maybe(..))
import Data.Newtype (over)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.Component (hoistSlot)
import Halogen.Css as HC
import Halogen.HTML as HH
import Polyform (Reporter(..))
import Polyform.Reporter (runReporter)

type Input i =
  { value :: i
  , classNames :: Array String
  }

data Msg action a
  = Updated (Maybe a)
  | Escalated action

data Action action input m
  = Update input
  | UpdateM input (m input)
  | Escalate action

type ComponentHTML action input slots m =
  H.ComponentHTML (Action action input m) slots m

type Component query input action a m =
  H.Component query (Input input) (Msg action a) m

type Slot action a slot = forall q. H.Slot q (Msg action a) slot

type Form action input slots m a =
  Reporter m (Array (ComponentHTML action input slots m)) input a

split
  :: forall ac i j s m a b
   . Monad m
  => Form ac i s m a
  -> Form ac j s m b
  -> Form ac (Tuple i j) s m (Tuple a b)
split f g = Tuple <$> subform _1 f <*> subform _2 g

subform
  :: forall ac i j s m a
   . Functor m
  => Lens' i j
  -> Form ac j s m a
  -> Form ac i s m a
subform lens (Reporter (Star f)) = Reporter $ Star \i ->
  let
    setJ j = set lens j i
    adaptAction (Update j) = Update (set lens j i)
    adaptAction (UpdateM j mj) = UpdateM (setJ j) (map setJ mj)
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
hoistForm a =
  over Reporter $ over Star $ map $ mapMaybeT $ mapWriterT $ a <<< map hoistR
  where
  hoistR = map $ map $ bimap hoistFormSlot hoistAction
  hoistFormSlot = hoistSlot a <<< map hoistAction
  hoistAction (Update j) = Update j
  hoistAction (UpdateM j mj) = UpdateM j (a mj)
  hoistAction (Escalate ac) = Escalate ac

data InternalAction action input m
  = Init
  | Receive (Input input)
  | PublicAction (Action action input m)

component
  :: forall q i ac a m s
   . MonadAff m
  => Form ac i s m a
  -> H.Component q (Input i) (Msg ac a) m
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
  initialState :: Input i -> _
  initialState { value, classNames } =
    { classNames, value, children: [], result: Nothing, version: Nothing }
  render { classNames, children } =
    HH.form [ HC.classNames classNames ]
      $ bimap (map PublicAction) PublicAction <$> children
  handleInput currentVersion version { value, classNames } = do
    modify_ _ { value = value, classNames = classNames }
    Tuple result children <- lift (runReporter reporter value)
    finalVersion <- liftAff $ AVar.tryTake version
    case finalVersion of
      Nothing -> pure unit
      Just finalVersion'
        | finalVersion' /= currentVersion -> pure unit
        | otherwise -> do
            previousResult <- gets _.result
            modify_ _ { children = children, result = result }
            liftAff $ AVar.put (currentVersion + 1) version
            case previousResult, result of
              Nothing, Nothing -> pure unit
              _, _ -> H.raise (Updated result)

  handleAction Init = do
    { classNames, value } <- get
    version <- liftAff $ AVar.new 0
    modify_ _ { version = Just version }
    handleInput 0 version { classNames, value }
  handleAction action = do
    { classNames, version: mVersion } <- get
    for_ mVersion \version -> do
      currentVersion <- liftAff $ AVar.read version
      case action of
        Receive input -> handleInput currentVersion version input
        PublicAction (Update value) ->
          handleInput currentVersion version { value, classNames }
        PublicAction (UpdateM value mValue) -> do
          handleInput currentVersion version { value, classNames }
          value' <- lift mValue
          handleInput (currentVersion + 1) version { value: value', classNames }
        PublicAction (Escalate a) -> H.raise (Escalated a)
        Init -> pure unit
