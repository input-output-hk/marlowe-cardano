module Page.HaskellEditor.View where

import Prologue hiding (div)

import Component.BottomPanel.Types (Action(..)) as BottomPanel
import Component.BottomPanel.View (render) as BottomPanel
import Component.MetadataTab (render) as MetadataTab
import Data.Argonaut (printJsonDecodeError)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Enum (toEnum, upFromIncluding)
import Data.Lens (_Right, has, view, (^.))
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), ComponentHTML)
import Halogen.Classes
  ( bgWhite
  , flex
  , flexCol
  , flexGrow
  , fullHeight
  , group
  , maxH70p
  , minH0
  , overflowHidden
  , paddingX
  , spaceBottom
  )
import Halogen.Css (classNames)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML
  ( HTML
  , button
  , code_
  , div
  , div_
  , option
  , pre_
  , section
  , section_
  , select
  , slot
  , text
  )
import Halogen.HTML.Events (onClick, onSelectedIndexChange)
import Halogen.HTML.Properties (class_, classes, enabled)
import Halogen.HTML.Properties as HP
import Halogen.Monaco (monacoComponent)
import Language.Haskell.Interpreter
  ( CompilationError(..)
  , InterpreterError(..)
  , InterpreterResult(..)
  )
import Language.Haskell.Monaco as HM
import MainFrame.Types (ChildSlots, _haskellEditorSlot)
import Marlowe.Extended as E
import Marlowe.Extended.Metadata (MetaData)
import Network.RemoteData (RemoteData(..), _Success)
import Page.HaskellEditor.Types
  ( Action(..)
  , BottomPanelView(..)
  , State
  , _bottomPanelState
  , _compilationResult
  , _haskellEditorKeybindings
  , _metadataHintInfo
  )
import StaticAnalysis.BottomPanel (analysisPane)
import Text.Pretty (pretty)

render
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> ComponentHTML Action ChildSlots m
render metadata state =
  div [ classes [ flex, flexCol, fullHeight ] ]
    [ section [ classes [ paddingX, minH0, flexGrow, overflowHidden ] ]
        [ haskellEditor ]
    , section [ classes [ paddingX, maxH70p ] ]
        [ renderSubmodule
            _bottomPanelState
            BottomPanelAction
            (BottomPanel.render panelTitles wrapBottomPanelContents)
            state
        ]
    ]
  where
  panelTitles =
    [ { title: "Metadata", view: MetadataView, classes: [] }
    , { title: "Generated code", view: GeneratedOutputView, classes: [] }
    , { title: "Static Analysis", view: StaticAnalysisView, classes: [] }
    , { title: "Errors", view: ErrorsView, classes: [] }
    ]

  -- TODO: improve this wrapper helper
  actionWrapper = BottomPanel.PanelAction

  wrapBottomPanelContents panelView = bimap (map actionWrapper) actionWrapper $
    panelContents state metadata panelView

otherActions :: forall p. State -> HTML p Action
otherActions state =
  div [ classes [ group ] ]
    [ editorOptions state
    , compileButton state
    , sendToSimulationButton state
    -- FIXME: I think we want to change this action to be called from the simulator
    --        with the action "soon to be implemented" ViewAsBlockly
    -- , sendResultButton state "Send To Blockly" SendResultToBlockly
    ]

editorOptions :: forall p. State -> HTML p Action
editorOptions state =
  div [ class_ (ClassName "editor-options") ]
    [ select
        [ HP.id "editor-options"
        , HP.value $ show $ state ^. _haskellEditorKeybindings
        , onSelectedIndexChange (maybe DoNothing ChangeKeyBindings <<< toEnum)
        ]
        (map keybindingItem (upFromIncluding bottom))
    ]
  where
  keybindingItem item =
    if state ^. _haskellEditorKeybindings == item then
      option [ class_ (ClassName "selected-item"), HP.value (show item) ]
        [ text $ show item ]
    else
      option [ HP.value (show item) ] [ text $ show item ]

haskellEditor :: forall m. MonadAff m => ComponentHTML Action ChildSlots m
haskellEditor = slot _haskellEditorSlot unit component unit HandleEditorMessage
  where
  component = monacoComponent $ HM.settings $ const $ pure unit

compileButton :: forall p. State -> HTML p Action
compileButton state =
  button
    [ onClick $ const Compile
    , enabled enabled'
    , classes classes'
    ]
    [ text buttonText ]
  where
  buttonText = case view _compilationResult state of
    Loading -> "Compiling..."
    Success _ -> "Compiled"
    _ -> "Compile"

  enabled' = case view _compilationResult state of
    NotAsked -> true
    _ -> false

  classes' =
    [ ClassName "btn" ]
      <> case view _compilationResult state of
        Success (Right _) -> [ ClassName "success" ]
        Success (Left _) -> [ ClassName "error" ]
        _ -> []

sendToSimulationButton :: forall p. State -> HTML p Action
sendToSimulationButton state =
  button
    [ onClick $ const SendResultToSimulator
    , enabled enabled'
    , classNames [ "btn" ]
    ]
    [ text "Send To Simulator" ]
  where
  compilationResult = view _compilationResult state

  enabled' = case compilationResult of
    Success (Right (InterpreterResult _)) -> true
    _ -> false

panelContents
  :: forall m
   . MonadAff m
  => State
  -> MetaData
  -> BottomPanelView
  -> ComponentHTML Action ChildSlots m
panelContents state _ GeneratedOutputView =
  section [ classNames [ "py-4" ] ] case view _compilationResult state of
    Success (Right (InterpreterResult result)) ->
      [ div [ classes [ bgWhite, spaceBottom, ClassName "code" ] ]
          numberedText
      ]
      where
      numberedText = (code_ <<< Array.singleton <<< text) <$> split
        (Pattern "\n")
        ((printContractOrError <<< _.result) result)
    _ -> [ text "There is no generated code" ]
  where
  parseJSONContract :: String -> Either JsonDecodeError E.Contract
  parseJSONContract = parseDecodeJson

  printContractOrError :: String -> String
  printContractOrError s =
    case parseJSONContract s of
      Left err -> printJsonDecodeError err
      Right contract -> (show <<< pretty) contract

panelContents state metadata StaticAnalysisView =
  section_
    if isCompiled then
      [ analysisPane
          metadata
          { warnings: AnalyseContract
          , reachability: AnalyseReachabilityContract
          , refund: AnalyseContractForCloseRefund
          }
          { valueAction: SetValueTemplateParam
          , timeAction: SetTimeTemplateParam
          }
          state
      ]
    else
      [ div [ classNames [ "py-4", "choice-error" ] ]
          [ text
              "Haskell code needs to be compiled in order to run static analysis"
          ]
      ]
  where
  isCompiled = has (_compilationResult <<< _Success <<< _Right) state

panelContents state _ ErrorsView =
  section [ classNames [ "py-4" ] ] case view _compilationResult state of
    Success (Left (TimeoutError error)) -> [ text error ]
    Success (Left (CompilationErrors errors)) -> map compilationErrorPane errors
    _ -> [ text "No errors" ]

panelContents state metadata MetadataView =
  section [ classNames [ "py-4" ] ]
    [ MetadataTab.render
        { metadataHintInfo: state ^. _metadataHintInfo
        , metadata
        }
        MetadataAction
    ]

compilationErrorPane :: forall p. CompilationError -> HTML p Action
compilationErrorPane (RawError error) = div_ [ text error ]

compilationErrorPane (CompilationError error) =
  div
    [ class_ $ ClassName "compilation-error"
    ]
    [ text $ "Line " <> show error.row <> ", Column " <> show error.column <>
        ":"
    , code_ [ pre_ [ text $ String.joinWith "\n" error.text ] ]
    ]
