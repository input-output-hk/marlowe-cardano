module Page.Simulation.Lenses where

import Component.BottomPanel.Types as BottomPanel
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Help (HelpContext)
import Page.Simulation.Types (State, BottomPanelView)

_showRightPanel :: Lens' State Boolean
_showRightPanel = prop (Proxy :: _ "showRightPanel")

_helpContext :: Lens' State HelpContext
_helpContext = prop (Proxy :: _ "helpContext")

_bottomPanelState :: Lens' State (BottomPanel.State BottomPanelView)
_bottomPanelState = prop (Proxy :: _ "bottomPanelState")

_decorationIds :: Lens' State (Array String)
_decorationIds = prop (Proxy :: _ "decorationIds")
