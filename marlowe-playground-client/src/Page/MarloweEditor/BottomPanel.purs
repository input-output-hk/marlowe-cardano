module Page.MarloweEditor.BottomPanel
  ( panelContents
  ) where

import Prologue hiding (div)

import Component.MetadataTab (render) as MetadataTab
import Data.Array (drop, head)
import Data.Array as Array
import Data.Lens (to, (^.))
import Data.String (take)
import Data.String.Extra (unlines)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Classes
  ( fontBold
  , fullWidth
  , grid
  , gridColsDescriptionLocation
  , justifySelfEnd
  , minW0
  , overflowXScroll
  , underline
  )
import Halogen.Css (classNames)
import Halogen.HTML (a, div, pre_, section, span_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import MainFrame.Types (ChildSlots)
import Marlowe.Extended.Metadata (MetaData)
import Page.MarloweEditor.Types
  ( Action(..)
  , BottomPanelView(..)
  , State
  , _editorErrors
  , _editorWarnings
  , _hasHoles
  , _metadataHintInfo
  , _showErrorDetail
  )
import StaticAnalysis.BottomPanel (analysisPane)
import Text.Extra (lines)

panelContents
  :: forall m
   . MonadAff m
  => State
  -> MetaData
  -> BottomPanelView
  -> ComponentHTML Action ChildSlots m
panelContents state metadata MetadataView =
  section [ classNames [ "py-4" ] ]
    [ MetadataTab.render
        { metadata
        , metadataHintInfo: state ^. _metadataHintInfo
        }
        MetadataAction
    ]

panelContents state metadata StaticAnalysisView =
  section [ classNames [ "flex", "flex-col" ] ]
    if (state ^. _hasHoles) then
      [ div [ classNames [ "py-4" ] ]
          [ text
              "The contract needs to be complete (no holes) before doing static analysis."
          ]
      ]
    else
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

panelContents state _ MarloweWarningsView =
  section [ classNames [ "py-4" ] ]
    if Array.null warnings then
      [ pre_ [ text "No warnings" ] ]
    else
      [ div [ classes [ grid, gridColsDescriptionLocation, fullWidth ] ]
          (headers <> (renderWarning =<< warnings))
      ]
  where
  warnings = state ^. _editorWarnings

  headers =
    [ div [ class_ fontBold ] [ text "Description" ]
    , div [ class_ fontBold ] [ text "Line Number" ]
    ]

  renderWarning warning =
    [ span_ $ [ text warning.message ]
    , a
        [ onClick $ const $ MoveToPosition warning.startLineNumber
            warning.startColumn
        , classes [ underline, justifySelfEnd ]
        ]
        [ text $ show warning.startLineNumber ]
    ]

panelContents state _ MarloweErrorsView =
  section [ classNames [ "py-4" ] ]
    if Array.null errors then
      [ pre_ [ text "No errors" ] ]
    else
      [ div [ classes [ grid, gridColsDescriptionLocation, fullWidth ] ]
          (headers <> (renderError =<< errors))
      ]
  where
  errors = state ^. (_editorErrors <<< to (map formatError))

  headers =
    [ div [ class_ fontBold ] [ text "Description" ]
    , div [ class_ fontBold ] [ text "Line Number" ]
    ]

  renderError error =
    [ div [ classes [ minW0, overflowXScroll ] ]
        ( [ a
              [ onClick $ const $ ShowErrorDetail
                  (state ^. (_showErrorDetail <<< to not))
              ]
              [ text $ (if state ^. _showErrorDetail then "- " else "+ ") <>
                  error.firstLine
              ]
          ]
            <>
              if (state ^. _showErrorDetail) then
                [ pre_ [ text error.restLines ] ]
              else
                []
        )
    , a
        [ onClick $ const $ MoveToPosition error.startLineNumber
            error.startColumn
        , class_ underline
        ]
        [ text $ show error.startLineNumber ]
    ]

  formatError { message, startColumn, startLineNumber } =
    let
      lines' = lines message

      firstLine /\ restLines =
        if (take 12 <$> head lines') == Just "Syntax error" then
          "Syntax error" /\ (unlines $ drop 2 lines')
        else
          "Error" /\ unlines lines'
    in
      { message, startColumn, startLineNumber, firstLine, restLines }
