module Page.BlocklyEditor.BottomPanel
  ( panelContents
  ) where

import Prologue hiding (div)

import Component.MetadataTab (render) as MetadataTab
import Data.Array as Array
import Data.Lens ((^.))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Classes
  ( flex
  , flexCol
  , fontBold
  , fullWidth
  , grid
  , gridColsDescriptionLocation
  , justifySelfEnd
  , underline
  )
import Halogen.Css (classNames)
import Halogen.HTML (a, div, div_, pre_, section, span_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes)
import Language.Marlowe.Extended.V1.Metadata.Types (MetaData)
import MainFrame.Types (ChildSlots)
import Page.BlocklyEditor.Types
  ( Action(..)
  , BottomPanelView(..)
  , State
  , _hasHoles
  , _metadataHintInfo
  , _warnings
  )
import StaticAnalysis.BottomPanel (analysisPane)

panelContents
  :: forall m
   . MonadAff m
  => State
  -> MetaData
  -> BottomPanelView
  -> ComponentHTML Action ChildSlots m
panelContents state metadata MetadataView =
  MetadataTab.render
    { metadataHintInfo: state ^. _metadataHintInfo
    , metadata
    }
    MetadataAction

panelContents state metadata StaticAnalysisView =
  section [ classes [ flex, flexCol ] ]
    if (state ^. _hasHoles) then
      [ div_
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

panelContents state _ BlocklyWarningsView =
  section [ classNames [ "py-4" ] ]
    if Array.null warnings then
      [ pre_ [ text "No warnings" ] ]
    else
      [ div [ classes [ grid, gridColsDescriptionLocation, fullWidth ] ]
          (headers <> (renderWarning =<< warnings))
      ]
  where
  warnings = state ^. _warnings

  headers =
    [ div [ class_ fontBold ] [ text "Description" ]
    , div [ class_ fontBold ] [ text "Location" ]
    ]

  renderWarning warning =
    [ span_ $ [ text $ show warning ]
    , a
        [ onClick $ const $ SelectWarning warning
        , classes [ underline, justifySelfEnd ]
        ]
        [ text $ "select block" ]
    ]
