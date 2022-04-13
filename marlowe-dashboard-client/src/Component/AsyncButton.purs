module Component.AsyncButton (Query, Msg, Input, Slot, component, defaultInput) where

import Prologue

import Component.Icons as Icon
import Control.Alt ((<|>))
import Control.Alternative (guard)
import Css as Css
import Data.Array (filter)
import Data.Compactable (compact)
import Data.Foldable (fold, for_, traverse_)
import Data.Int (round)
import Data.Lens (Lens', _Just, assign, modifying, use, (^.), (^?))
import Data.Lens.Record (prop)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect.Aff (delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Animation (waitForAllAnimations)
import Halogen.Component.Reactive (_input, _transient)
import Halogen.Component.Reactive as HR
import Halogen.Css (classNames)
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))
import Web.HTML.HTMLElement (offsetHeight)

data Msg e a
  = Clicked
  | LeftIdle
  | EnteredIdle
  | EnteredSpin
  | LeftSpin
  | EnteredComplete (Either e a)
  | LeftComplete (Either e a)

type Input e a =
  { text :: String
  , result :: RemoteData e a
  , styles :: Array String
  , disabled :: Boolean
  , successMessage :: a -> String
  , failureMessage :: e -> String
  , successDuration :: Milliseconds
  , failureDuration :: Milliseconds
  }

defaultInput :: forall e a. Input e a
defaultInput =
  { text: ""
  , result: NotAsked
  , styles: []
  , disabled: false
  , successMessage: const "Success"
  , failureMessage: const "Failed"
  , successDuration: Milliseconds 1000.0
  , failureDuration: Milliseconds 2000.0
  }

data Query (a :: Type)

type Component e a = H.Component Query (Input e a) (Msg e a)

type Slot e a = H.Slot Query (Msg e a)

component :: forall e a m. MonadAff m => Component e a m
component = HR.mkReactiveComponent
  { deriveState: const unit
  , initialTransient:
      { animationState: Idle
      , height: Nothing
      }
  , render
  , eval: HR.fromHandleActionAndInput
      { handleAction
      , handleInput
      }
  }

type Transient e a =
  { animationState :: AnimationState e a
  , height :: Maybe String
  }

-- | An enum that represents the stages of animation for the button.
data AnimationState e a
  = Idle
  | IdleToSpin
  | Spin
  | SpinToIdle
  | SpinToComplete (Either e a)
  | Complete (Either e a)

type State e a = HR.State (Input e a) Unit (Transient e a)

type StateLens e a x = Lens' (State e a) x

_text :: forall e a. StateLens e a String
_text = _input <<< prop (Proxy :: _ "text")

_result :: forall e a. StateLens e a (RemoteData e a)
_result = _input <<< prop (Proxy :: _ "result")

_styles :: forall e a. StateLens e a (Array String)
_styles = _input <<< prop (Proxy :: _ "styles")

_failureMessage :: forall e a. StateLens e a (e -> String)
_failureMessage = _input <<< prop (Proxy :: _ "failureMessage")

_successMessage :: forall e a. StateLens e a (a -> String)
_successMessage = _input <<< prop (Proxy :: _ "successMessage")

_successDuration :: forall e a. StateLens e a Milliseconds
_successDuration = _input <<< prop (Proxy :: _ "successDuration")

_failureDuration :: forall e a. StateLens e a Milliseconds
_failureDuration = _input <<< prop (Proxy :: _ "failureDuration")

_disabled :: forall e a. StateLens e a Boolean
_disabled = _input <<< prop (Proxy :: _ "disabled")

_animationState :: forall e a. StateLens e a (AnimationState e a)
_animationState = _transient <<< prop (Proxy :: _ "animationState")

_height :: forall e a. StateLens e a (Maybe String)
_height = _transient <<< prop (Proxy :: _ "height")

data Action = OnClick

type HalogenM e a m = H.HalogenM (State e a) Action () (Msg e a) m

handleAction :: forall e a m. MonadAff m => Action -> HalogenM e a m Unit
handleAction = case _ of
  OnClick -> do
    result <- use _result
    case result of
      NotAsked -> do
        assign _result Loading
        H.raise Clicked
      _ -> pure unit

handleInput
  :: forall e a m. MonadAff m => Maybe (State e a) -> HalogenM e a m Unit
handleInput oldState = do
  mElement <- H.getHTMLElementRef buttonRef
  mHeight <- liftEffect $ traverse
    (map ((_ <> "px") <<< show <<< round) <<< offsetHeight)
    mElement
  -- if we already know the height, don't assign it again
  modifying _height (_ <|> mHeight)
  result <- use _result
  let oldResult = oldState ^? _Just <<< _result
  animationState <- use _animationState
  let
    transitionAnimation mVia = do
      for_ mVia \via -> do
        assign _animationState via
        liftAff $ traverse_ waitForAllAnimations mElement
      result' <- use _result
      case result' of
        NotAsked -> do
          assign _animationState Idle
          H.raise EnteredIdle
        Loading -> do
          assign _animationState Spin
          H.raise EnteredSpin
        Failure e -> do
          let resultEither = Left e
          assign _animationState $ Complete resultEither
          H.raise $ EnteredComplete resultEither
          failureDuration <- use _failureDuration
          liftAff $ delay failureDuration
          H.raise $ LeftComplete resultEither
        Success a -> do
          let resultEither = Right a
          assign _animationState $ Complete resultEither
          H.raise $ EnteredComplete resultEither
          successDuration <- use _successDuration
          liftAff $ delay successDuration
          H.raise $ LeftComplete resultEither

  case oldResult, result of
    Just NotAsked, NotAsked -> pure unit
    Just Loading, Loading -> pure unit
    Just (Failure _), Failure _ -> pure unit
    Just (Success _), Success _ -> pure unit
    _, NotAsked -> case animationState of
      Spin -> do
        H.raise LeftSpin
        transitionAnimation $ Just SpinToIdle
      Idle -> pure unit
      _ -> transitionAnimation Nothing
    _, Loading -> case animationState of
      Idle -> do
        H.raise LeftIdle
        transitionAnimation $ Just IdleToSpin
      Spin -> pure unit
      _ -> transitionAnimation Nothing
    _, Failure e -> case animationState of
      Spin -> do
        H.raise LeftSpin
        transitionAnimation $ Just $ SpinToComplete $ Left e
      Complete _ -> pure unit
      _ -> transitionAnimation Nothing
    _, Success a -> case animationState of
      Spin -> do
        H.raise LeftSpin
        transitionAnimation $ Just $ SpinToComplete $ Right a
      Complete _ -> pure unit
      _ -> transitionAnimation Nothing

buttonRef :: H.RefLabel
buttonRef = H.RefLabel "button"

render :: forall e a w. State e a -> HH.HTML w Action
render state =
  HH.div
    [ classNames $ [ "flex", "justify-center" ] ]
    [ HH.button
        ( compact
            [ pure $ classNames buttonStyles
            , pure $ HP.ref buttonRef
            , pure $ HP.disabled disabled
            , onClick_ <$> onClick
            , HP.style <$> elementStyle
            ]
        )
        content
    ]
  where
  result = state ^. _result
  disabled = state ^. _disabled
  animationState = state ^. _animationState
  height = state ^. _height
  styles = state ^. _styles
  successMessage = state ^. _successMessage
  failureMessage = state ^. _failureMessage
  onClick = case result of
    NotAsked -> OnClick <$ guard (not disabled)
    _ -> Nothing
  content = case animationState of
    Idle -> [ HH.text $ state ^. _text ]
    Complete (Left e) -> [ HH.text $ failureMessage e ]
    Complete (Right a) ->
      [ Icon.icon Icon.TaskAlt [ "font-normal", "text-green" ]
      , HH.text $ successMessage a
      ]
    _ -> [ HH.span [ classNames [ "invisible" ] ] [ HH.text "l" ] ]
  buttonStyles = baseButtonStyles <> conditionalButtonStyles <> styles
  baseButtonStyles = [ "leading-6", "transition-width", "duration-200" ]
  buttonAdjusted = filter (_ /= "leading-none") Css.button
  conditionalButtonStyles = case animationState of
    Idle -> buttonAdjusted <> Css.bgBlueGradient <> Css.withShadow <>
      [ "w-full" ]
    IdleToSpin -> spinStyles
    SpinToIdle -> spinStyles
    Spin -> spinStyles <> [ "animate-spin" ]
    SpinToComplete (Left _) -> resultStyles <> [ "border-red", "text-red" ]
    SpinToComplete (Right _) -> resultStyles <> [ "border-green", "text-green" ]
    Complete (Left _) -> resultStyles <> [ "border-red", "text-red" ]
    Complete (Right _) -> resultStyles <> [ "border-green", "text-green" ]
  spinStyles =
    [ "border-4"
    , "border-gray"
    , "border-left-green"
    , "rounded-full"
    , "cursor-default"
    , "outline-none"
    , "focus:outline-none"
    ]
  resultStyles = buttonAdjusted <>
    [ "w-full"
    , "border-2"
    , "flex"
    , "gap-2"
    , "items-center"
    , "justify-center"
    , "cursor-default"
    ]
  spinElementStyles = fold
    [ append "width: " <$> height, append "; height: " <$> height ]
  elementStyle = case animationState of
    IdleToSpin -> spinElementStyles
    Spin -> spinElementStyles
    _ -> append "height: " <$> height
