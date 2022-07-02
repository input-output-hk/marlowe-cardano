module Page.JavascriptEditor.View where

import Prologue hiding (div)

import Component.BottomPanel.Types (Action(..)) as BottomPanel
import Component.BottomPanel.View (render) as BottomPanel
import Component.MetadataTab (render) as MetadataTab
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Enum (toEnum, upFromIncluding)
import Data.Lens (view, (^.))
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
  , a
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
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA (label, role)
import Language.Javascript.Interpreter
  ( CompilationError(..)
  , InterpreterResult(..)
  )
import Language.Marlowe.Extended.V1.Metadata (MetaData)
import MainFrame.Types (ChildSlots, _jsEditorSlot)
import Page.JavascriptEditor.State (mkEditor)
import Page.JavascriptEditor.Types
  ( Action(..)
  , BottomPanelView(..)
  , State
  , _bottomPanelState
  , _compilationResult
  , _keybindings
  , _metadataHintInfo
  )
import Page.JavascriptEditor.Types as JS
import StaticAnalysis.BottomPanel
  ( analysisPane
  )
import Text.Pretty (pretty)

render
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> ComponentHTML Action ChildSlots m
render metadata state =
  div [ HP.classes [ flex, flexCol, fullHeight ] ]
    [ section
        [ HP.classes [ paddingX, minH0, flexGrow, overflowHidden ]
        , role "heading"
        , label "javascript-editor"
        ]
        [ jsEditor ]
    , section [ HP.classes [ paddingX, maxH70p ] ]
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
  div [ HP.classes [ group ] ]
    [ editorOptions state
    , compileButton state
    , sendToSimulationButton state
    ]

sendToSimulationButton :: forall p. State -> HTML p Action
sendToSimulationButton state =
  button
    [ onClick $ const SendResultToSimulator
    , HP.enabled enabled'
    , classNames [ "btn" ]
    ]
    [ text "Send To Simulator" ]
  where
  compilationResult = view _compilationResult state

  enabled' = case compilationResult of
    JS.CompiledSuccessfully _ -> true
    _ -> false

editorOptions :: forall p. State -> HTML p Action
editorOptions state =
  div [ HP.class_ (ClassName "editor-options") ]
    [ select
        [ HP.id "editor-options"
        , HP.value $ show $ state ^. _keybindings
        , onSelectedIndexChange (maybe DoNothing ChangeKeyBindings <<< toEnum)
        ]
        (map keybindingItem (upFromIncluding bottom))
    ]
  where
  keybindingItem item =
    if state ^. _keybindings == item then
      option [ HP.class_ (ClassName "selected-item"), HP.value (show item) ]
        [ text $ show item ]
    else
      option [ HP.value (show item) ] [ text $ show item ]

jsEditor :: forall m. MonadAff m => ComponentHTML Action ChildSlots m
jsEditor = slot _jsEditorSlot unit mkEditor unit HandleEditorMessage

compileButton :: forall p. State -> HTML p Action
compileButton state =
  button
    [ onClick $ const Compile
    , HP.enabled enabled'
    , HP.classes classes'
    ]
    [ text buttonText ]
  where
  buttonText = case view _compilationResult state of
    JS.Compiling -> "Compiling..."
    JS.CompiledSuccessfully _ -> "Compiled"
    JS.CompilationError _ -> "Compiled"
    JS.NotCompiled -> "Compile"

  enabled' = case view _compilationResult state of
    JS.NotCompiled -> true
    _ -> false

  classes' =
    [ ClassName "btn" ]
      <> case view _compilationResult state of
        JS.CompiledSuccessfully _ -> [ ClassName "success" ]
        JS.CompilationError _ -> [ ClassName "error" ]
        _ -> []

panelContents
  :: forall m
   . MonadAff m
  => State
  -> MetaData
  -> BottomPanelView
  -> ComponentHTML Action ChildSlots m
panelContents state _ GeneratedOutputView =
  section [ classNames [ "py-4" ] ] case view _compilationResult state of
    JS.CompiledSuccessfully (InterpreterResult result) ->
      [ div [ HP.classes [ bgWhite, spaceBottom, ClassName "code" ] ]
          numberedText
      ]
      where
      numberedText = (code_ <<< Array.singleton <<< text) <$> split
        (Pattern "\n")
        ((show <<< pretty <<< _.result) result)
    _ -> [ text "There is no generated code" ]

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
      [ div [ HP.classes [ ClassName "choice-error" ] ]
          [ text
              "JavaScript code needs to be compiled in order to run static analysis"
          ]
      ]
  where
  isCompiled = case view _compilationResult state of
    JS.CompiledSuccessfully _ -> true
    _ -> false

panelContents state _ ErrorsView =
  section [ classNames [ "py-4" ] ] case view _compilationResult state of
    JS.CompilationError err -> [ compilationErrorPane err ]
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
compilationErrorPane (RawError error) = div_
  [ text "There was an error when running the JavaScript code:"
  , code_ [ pre_ [ text $ error ] ]
  ]

compilationErrorPane (JSONParsingError error) =
  div_
    [ text "There was an error when parsing the resulting JSON:"
    , code_ [ pre_ [ text $ error ] ]
    , text
        "Please, use the JS API provided (see tutorial and examples). If you did use the JS API and still get this error, kindly report the problem at "
    , a
        [ HP.href
            "https://github.com/input-output-hk/marlowe-cardano/issues/new"
        ]
        [ text "https://github.com/input-output-hk/marlowe-cardano/issues/new" ]
    , text " including the code that caused the error. Thank you"
    ]

compilationErrorPane (CompilationError error) =
  div
    [ HP.class_ $ ClassName "compilation-error"
    ]
    [ text $ "There is a syntax error in line " <> show error.row <> ", column "
        <> show error.column
        <> ":"
    , code_ [ pre_ [ text $ String.joinWith "\n" error.text ] ]
    ]
