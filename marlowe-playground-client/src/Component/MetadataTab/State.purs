module Component.MetadataTab.State (carryMetadataAction) where

import Prologue hiding (div)
import Component.MetadataTab.Types (MetadataAction(..), labels)
import Contrib.Halogen.State.Record (mkSettersDispatcher)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (assign, modifying, over)
import Data.Map (alter, delete, insert, update) as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap (alter, delete, insert) as OMap
import Data.Newtype (un)
import Data.Tuple.Nested ((/\))
import Data.Variant (case_, on)
import Data.Variant (expand, match) as Variant
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen.Query (HalogenM)
import MainFrame.Types (Action, ChildSlots, State, _contractMetadata, _hasUnsavedChanges)
import Marlowe.Extended.Metadata (MetaData, _choiceInfo, _roleDescriptions, _slotParameterDescriptions, _valueParameterInfo, updateChoiceInfo, updateValueParameterInfo)
import Type.Prelude (Proxy(..))

carrySetAction = mkSettersDispatcher (Proxy :: Proxy MetaData)

carryMetadataAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  MetadataAction ->
  HalogenM State Action ChildSlots Void m Unit
carryMetadataAction = do
  -- | Let's cache generic set actions handlers
  \(MetadataAction action) -> do
    let
      go =
        ( (Variant.match carrySetAction <<< Variant.expand)
            # on labels.setRoleDescription (\(tokenName /\ description) -> over _roleDescriptions (Map.insert tokenName description))
            # on labels.deleteRoleDescription (\tokenName -> over _roleDescriptions (Map.delete tokenName))
            # on labels.setSlotParameterDescription (\(slotParam /\ description) -> over _slotParameterDescriptions (OMap.insert slotParam description))
            # on labels.deleteSlotParameterDescription (\slotParam -> over _slotParameterDescriptions (OMap.delete slotParam))
            # on labels.setValueParameterDescription
                ( \(valueParameterName /\ description) ->
                    over _valueParameterInfo (updateValueParameterInfo _ { valueParameterDescription = description } valueParameterName)
                )
            # on labels.setValueParameterFormat
                ( \(valueParameterName /\ format) ->
                    over _valueParameterInfo (updateValueParameterInfo _ { valueParameterFormat = format } valueParameterName)
                )
            # on labels.deleteValueParameterInfo (\valueParameterName -> over _valueParameterInfo (OMap.delete valueParameterName))
            # on labels.setChoiceDescription
                ( \(choiceName /\ description) ->
                    over _choiceInfo (updateChoiceInfo _ { choiceDescription = description } choiceName)
                )
            # on labels.setChoiceFormat
                ( \(choiceName /\ format) ->
                    over _choiceInfo (updateChoiceInfo _ { choiceFormat = format } choiceName)
                )
            # on labels.deleteChoiceInfo (\choiceName -> over _choiceInfo (Map.delete choiceName))
        )
    modifying _contractMetadata $ go action
    assign (_hasUnsavedChanges) true
