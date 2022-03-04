module Component.Autocomplete
  ( Action
  , ChildSlots
  , ComponentHTML
  , Input
  , Msg(..)
  , Query(..)
  , Slot
  , State
  , component
  ) where

import Prologue

import Component.Form
  ( renderErrorLabel
  , renderInput
  , renderInputBox
  , renderLabel
  )
import Control.Alternative (guard)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (runMaybeT)
import Data.Array as Array
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Foldable (for_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String.Extra as String
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.FieldState (FieldState(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (selectEq)
import NSelect as Select
import Store as Store
import Type.Proxy (Proxy(..))
import Web.UIEvent.FocusEvent (FocusEvent, relatedTarget)

data Query (a :: Type)

type Input output =
  { id :: String
  , label :: String
  , fieldState :: FieldState String output
  , options :: Bimap String output
  }

data Msg output
  = Updated (FieldState String output)

data Action output
  = OnNSelectMsg (Select.Message (Action output))
  | OnReceive (Connected (Maybe String) (Input output))
  | OnBlur FocusEvent

_select = Proxy :: Proxy "select"

type ChildSlots output =
  ( "select" :: Select.Slot (Action output) Unit
  )

type ComponentHTML output m =
  H.ComponentHTML (Action output) (ChildSlots output) m

type Slot output = H.Slot Query (Msg output)

type DSL output m a =
  H.HalogenM (State output) (Action output) (ChildSlots output) (Msg output) m a

type State output =
  { options :: Bimap String output
  , filtered :: Array String
  , fieldState :: FieldState String output
  , visited :: Boolean
  , id :: String
  , label :: String
  , openDropdown :: Maybe String
  }

type Component output m =
  H.Component Query (Input output) (Msg output) m

component
  :: forall output m
   . Ord output
  => Show output
  => MonadAff m
  => MonadStore Store.Action Store.Store m
  => Component output m
component = connect (selectEq _.openDropdown) $ H.mkComponent
  { initialState: initialState false
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< OnReceive
      }
  }

initialState
  :: forall output
   . Ord output
  => Boolean
  -> Connected (Maybe String) (Input output)
  -> State output
initialState visited { context, input: { fieldState, options, id, label } } =
  setFieldState fieldState
    { fieldState
    , visited
    , options
    , filtered: []
    , id
    , label
    , openDropdown: context
    }

setFieldState
  :: forall output
   . Ord output
  => FieldState String output
  -> State output
  -> State output
setFieldState fieldState state = case fieldState of
  Blank -> state
    { fieldState = Blank
    , filtered = Set.toUnfoldable $ Bimap.keysL state.options
    }
  Incomplete value -> case Bimap.lookupL value state.options of
    Nothing ->
      state
        { fieldState = Incomplete value
        , filtered = Set.toUnfoldable
            $ Set.filter (String.startsWith (Pattern value))
            $ Bimap.keysL state.options
        }
    Just a -> setFieldState (Complete a) state
  Complete a -> case Bimap.lookupR a state.options of
    Nothing -> setFieldState Blank state
    Just s ->
      state
        { fieldState = Complete a
        , filtered = [ s ]
        }

render
  :: forall output m
   . Ord output
  => MonadAff m
  => State output
  -> ComponentHTML output m
render { visited, fieldState, id, label, options, filtered } =
  HH.slot _select unit Select.component selectInput OnNSelectMsg
  where
  selectInput = { itemCount: Array.length filtered, render: renderSelect }
  renderSelect { isOpen, highlightedIndex } =
    let
      error = guard (visited && not isOpen) *> case fieldState of
        Complete _ -> Nothing
        _ -> Just "Not found."
      value = case fieldState of
        Blank -> ""
        Incomplete v -> v
        Complete a -> fromMaybe "" $ Bimap.lookupR a options
      dropdown = HH.keyed (HH.ElemName "ul")
        (Select.setMenuProps [])
        (mapWithIndex item filtered)
      item index v = Tuple v $ HH.li
        ( Select.setItemProps index
            [ classNames $ [ "px-4", "py-2" ] <>
                if index == highlightedIndex then
                  [ "bg-lightgray", "cursor-pointer" ]
                else []
            ]
        )
        [ HH.text v ]
      rootClasses = [ "relative", if isOpen then "z-20" else "z-10" ]
      boxClasses =
        [ "flex-col"
        , "absolute"
        , "top-0"
        , "left-0"
        , "right-0"
        , "bg-white"
        , "divide-y"
        , "divide-gray"
        , "items-stretch"
        ]
    in
      HH.div (Select.setRootProps ([ classNames rootClasses ]))
        [ renderLabel id label
        , HH.div [ HP.style "height: 54.4px" ] []
        , renderInputBox error boxClasses $ join
            [ pure $ renderInput id
                ( Select.setInputProps
                    [ HP.value value
                    , HE.onBlur $ Select.raise <<< OnBlur
                    ]
                )
            , guard isOpen $> dropdown
            ]
        , renderErrorLabel error
        ]

handleAction
  :: forall output m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => Ord output
  => Show output
  => Action output
  -> DSL output m Unit
handleAction = case _ of
  OnReceive input -> do
    openDropdown <- H.gets _.openDropdown
    H.modify_ \s -> initialState s.visited input
    when (openDropdown /= input.context) do
      H.tell _select unit $ case input.context of
        Just dropdown | dropdown == input.input.id -> Select.Open
        _ -> Select.Close
  OnNSelectMsg msg -> case msg of
    Select.Selected index -> void $ runMaybeT $ do
      { filtered, options } <- H.lift H.get
      value <- hoistMaybe $ Array.index filtered index
      output <- hoistMaybe $ Bimap.lookupL value options
      H.lift do
        { fieldState } <- H.modify _
          { fieldState = Complete output
          , filtered = [ value ]
          }
        H.raise $ Updated fieldState
        H.tell _select unit $ Select.Close
    Select.InputValueChanged value -> do
      { fieldState } <- H.modify $ setFieldState $ Incomplete value
      H.raise $ Updated fieldState
    Select.VisibilityChanged opened -> do
      { id } <- H.modify \s -> s { visited = s.visited || not opened }
      when opened do
        updateStore $ Store.DropdownOpened id
    Select.Emit action -> handleAction action
    _ -> pure unit
  OnBlur event -> do
    for_ (relatedTarget event) \_ -> do
      updateStore Store.DropdownClosed
