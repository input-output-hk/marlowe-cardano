module Component.MetadataTab.State (carryMetadataAction) where

import Prologue hiding (div)
import Component.MetadataTab.Types (MetadataAction(..))
import Contrib.Halogen (mkActionDispatcher)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (modifying)
import Data.Newtype (un)
import Data.Variant (match) as Variant
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen.Query (HalogenM)
import MainFrame.Types (Action, ChildSlots, State, _contractMetadata)
import Marlowe.Extended.Metadata (MetaData)
import Type.Prelude (Proxy(..))

carryMetadataAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  MetadataAction ->
  HalogenM State Action ChildSlots Void m Unit
carryMetadataAction action = do
  modifying _contractMetadata (dispatch $ un MetadataAction action)
  where
  dispatch = Variant.match (mkActionDispatcher (Proxy :: Proxy MetaData))
