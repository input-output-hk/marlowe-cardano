module Component.MetadataTab.State (carryMetadataAction) where

import Prologue hiding (div)
import Component.MetadataTab.Types (MetadataAction(..), actionLabels)
import Contrib.Halogen.State.Record (mkSettersDispatcher, mkUpdatersDispatcher)
import Control.Monad.Reader (class MonadAsk)
import Data.Lens (assign, modifying, over)
import Data.Map (delete, insert) as Map
import Data.Map.Ordered.OMap (delete, moveLeft, moveRight) as OMap
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Variant (on)
import Data.Variant (expand, match) as Variant
import Effect.Aff.Class (class MonadAff)
import Env (Env)
import Halogen.Query (HalogenM)
import MainFrame.Types (Action, ChildSlots, State, _contractMetadata, _hasUnsavedChanges)
import Marlowe.Extended.Metadata (MetaData, _choiceInfo, _roleDescriptions, _valueParameterInfo, updateChoiceInfo, updateValueParameterInfo)
import Record (merge) as Record
import Type.Prelude (Proxy(..))

carryMetadataAction ::
  forall m.
  MonadAff m =>
  MonadAsk Env m =>
  MetadataAction ->
  HalogenM State Action ChildSlots Void m Unit
carryMetadataAction = do
  -- | Let's cache generic set actions handlers
  let
    genericDispatcher =
      Record.merge
        (mkSettersDispatcher (Proxy :: Proxy MetaData))
        (mkUpdatersDispatcher (Proxy :: Proxy MetaData))
  \(MetadataAction action) -> do
    let
      go =
        ( (Variant.match genericDispatcher <<< Variant.expand)
            # on actionLabels.deleteChoiceInfo (\choiceName -> over _choiceInfo (Map.delete choiceName))
            # on actionLabels.deleteRoleDescription (\tokenName -> over _roleDescriptions (Map.delete tokenName))
            # on actionLabels.deleteValueParameterInfo (\valueParameterName -> over _valueParameterInfo (OMap.delete valueParameterName))
            # on actionLabels.moveDownValueParameterDescription
                ( \valueParameterName ->
                    over _valueParameterInfo (fromMaybe <*> OMap.moveRight valueParameterName)
                )
            # on actionLabels.moveUpValueParameterDescription
                ( \valueParameterName ->
                    over _valueParameterInfo (fromMaybe <*> OMap.moveLeft valueParameterName)
                )
            # on actionLabels.setChoiceDescription
                ( \(choiceName /\ description) ->
                    over _choiceInfo (updateChoiceInfo _ { choiceDescription = description } choiceName)
                )
            # on actionLabels.setChoiceFormat
                ( \(choiceName /\ format) ->
                    over _choiceInfo (updateChoiceInfo _ { choiceFormat = format } choiceName)
                )
            # on actionLabels.setRoleDescription (\(tokenName /\ description) -> over _roleDescriptions (Map.insert tokenName description))
            # on actionLabels.setValueParameterDescription
                ( \(valueParameterName /\ description) ->
                    over _valueParameterInfo (updateValueParameterInfo _ { valueParameterDescription = description } valueParameterName)
                )
            # on actionLabels.setValueParameterFormat
                ( \(valueParameterName /\ format) ->
                    over _valueParameterInfo (updateValueParameterInfo _ { valueParameterFormat = format } valueParameterName)
                )
        )
    modifying _contractMetadata $ go action
    assign (_hasUnsavedChanges) true
