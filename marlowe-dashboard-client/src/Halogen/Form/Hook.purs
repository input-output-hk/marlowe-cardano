-- TODO move this to its own library
module Halogen.Form.Hook
  ( FormResult
  , useDerivedForm
  , useForm
  ) where

import Prelude

import Control.Monad.Free (substFree)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Css as HC
import Halogen.Form (Form, runForm)
import Halogen.Form.FormM (FormF(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (type (<>), Hook)
import Halogen.Hooks as Hooks
import Web.Event.Event (preventDefault)

type UseForm slots m input output =
  Hooks.UseState input
    <> Hooks.UseState (Maybe output)
    <> Hooks.UseState (Array (H.ComponentHTML input slots m))
    <> Hooks.UseState (Maybe (AVar Int))
    <> Hooks.UseEffect
    <> Hooks.UseEffect
    <> Hooks.Pure

type UseDerivedForm slots m input output =
  Hooks.UseMemo (Form slots m input output) <> UseForm slots m input output

-- | The results of evaluating the useForm hook.
type FormResult slots m a =
  { html :: Array String -> H.ComponentHTML (Hooks.HookM m Unit) slots m
  , result :: Maybe a
  }

-- | A hook to run a form as a stateful computation over its input. Accepts a
-- | form and an initial input and returns the current form result and html
-- | view.
useDerivedForm
  :: forall params slots m input output
   . MonadAff m
  => Eq input
  => Eq output
  => Eq { | params }
  => { | params }
  -> input
  -> ({ | params } -> Form slots m input output)
  -> Hook m (UseDerivedForm slots m input output) (FormResult slots m output)
useDerivedForm params initialInput deriveForm = Hooks.do
  form <- Hooks.captures params Hooks.useMemo \_ -> deriveForm params
  useForm initialInput form

-- | A hook to run a form as a stateful computation over its input. Accepts a
-- | form and an initial input and returns the current form result and html
-- | view.
useForm
  :: forall slots m input output
   . MonadAff m
  => Eq input
  => Eq output
  => input
  -> Form slots m input output
  -> Hook m (UseForm slots m input output) (FormResult slots m output)
useForm initialInput form = Hooks.do
  Tuple input inputId <- Hooks.useState initialInput
  Tuple result resultId <- Hooks.useState Nothing
  Tuple children childrenId <- Hooks.useState []
  Tuple versionAVar versionAVarId <- Hooks.useState Nothing
  Hooks.useLifecycleEffect do
    versionAVar' <- liftAff $ AVar.new 0
    Hooks.put versionAVarId $ Just versionAVar'
    pure Nothing
  handleInputChange { input, versionAVar, inputId, resultId, childrenId }
  let handleAction = Hooks.put inputId
  Hooks.pure
    { html: \classNames ->
        HH.form
          [ HC.classNames classNames
          , HE.onSubmit $ liftEffect <<< preventDefault
          ]
          $ bimap (map handleAction) handleAction <$> children
    , result
    }
  where
  eqInputDeps { versionAVar: Nothing } { versionAVar: Just _ } = false
  eqInputDeps { versionAVar: Just _ } { versionAVar: Nothing } = false
  eqInputDeps deps1 deps2 = deps1.input == deps2.input
  handleInputChange deps =
    Hooks.capturesWith eqInputDeps deps Hooks.useTickEffect do
      for_ deps.versionAVar \versionAVar -> do
        let { input, inputId, resultId, childrenId } = deps
        initialVersion <- liftAff $ AVar.read versionAVar
        nextVersionRef <- liftEffect $ Ref.new initialVersion
        let
          interpretFormF :: FormF input m ~> Hooks.HookM m
          interpretFormF (Update newInput next) = do
            withVersionCheck initialVersion 0 versionAVar do
              Hooks.put inputId newInput
              liftEffect $ Ref.modify_ (add 1) nextVersionRef
            pure next

          interpretFormF (Lift m) = lift m

        Tuple result children <- Hooks.HookM
          $ substFree (interpretFormF >>> case _ of Hooks.HookM free -> free)
          $ runForm form input
        nextVersion <- liftEffect $ Ref.read nextVersionRef
        withVersionCheck initialVersion (nextVersion + 1) versionAVar do
          Hooks.put childrenId children
          Hooks.put resultId result
      pure Nothing

  withVersionCheck
    :: forall n. MonadAff n => Int -> Int -> AVar Int -> n Unit -> n Unit
  withVersionCheck initialVersion increment versionAVar g = do
    version <- liftAff $ AVar.take versionAVar
    when (version == initialVersion) g
    liftAff $ AVar.put (version + increment) versionAVar
