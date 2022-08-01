module Component.MetadataTab.State (carryMetadataAction) where

import Prologue hiding (div)

import Component.MetadataTab.Types (MetadataAction(..))
import Contrib.Data.Unfoldable (Move) as Unfoldable
import Data.Array ((!!))
import Data.Lens (assign, modifying, over, set)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen.Query (HalogenM)
import Language.Marlowe.Extended.V1.Metadata
  ( updateChoiceInfo
  , updateValueParameterInfo
  )
import Language.Marlowe.Extended.V1.Metadata.Lenses
  ( _choiceInfo
  , _contractLongDescription
  , _contractName
  , _contractShortDescription
  , _contractType
  , _roleDescriptions
  , _timeParameterDescriptions
  , _valueParameterInfo

  )
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ChoiceInfo
  , NumberFormat
  , ValueParameterInfo

  )
import MainFrame.Types
  ( Action
  , ChildSlots
  , State
  , _contractMetadata
  , _hasUnsavedChanges
  )
import Marlowe (Api)
import Servant.PureScript (class MonadAjax)

moveTo
  :: forall key value
   . Ord key
  => Unfoldable.Move
  -> OMap key value
  -> OMap key value
moveTo move m = fromMaybe m do
  key /\ _ <- OMap.toUnfoldable m !! move.from
  OMap.moveTo move.to key m

carryMetadataAction
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => MetadataAction
  -> HalogenM State Action ChildSlots Void m Unit
carryMetadataAction action = do
  modifying (_contractMetadata) case action of
    SetContractName name -> set _contractName name
    SetContractType typeName -> set _contractType typeName
    SetContractShortDescription description -> set _contractShortDescription
      description
    SetContractLongDescription description -> set _contractLongDescription
      description
    SetRoleDescription tokenName description -> over _roleDescriptions $
      Map.insert tokenName description
    DeleteRoleDescription tokenName -> over _roleDescriptions $ Map.delete
      tokenName
    SetTimeParameterDescription timeParam description ->
      over _timeParameterDescriptions $ OMap.insert timeParam description
    MoveTimeParameterDescription move -> over _timeParameterDescriptions $
      moveTo move
    DeleteTimeParameterDescription timeParam -> over _timeParameterDescriptions
      $ OMap.delete timeParam
    SetValueParameterDescription valueParameterName description ->
      over _valueParameterInfo $ updateValueParameterInfo
        (setValueParameterDescription description)
        valueParameterName
    MoveValueParameterDescription move ->
      over _valueParameterInfo $ moveTo move
    SetValueParameterFormat valueParameterName format ->
      over _valueParameterInfo $ updateValueParameterInfo
        (setValueParameterFormat format)
        valueParameterName
    DeleteValueParameterInfo valueParameterName -> over _valueParameterInfo $
      OMap.delete valueParameterName
    SetChoiceDescription choiceName description -> over _choiceInfo $
      updateChoiceInfo (setChoiceDescription description) choiceName
    SetChoiceFormat choiceName format -> over _choiceInfo $ updateChoiceInfo
      (setChoiceFormat format)
      choiceName
    DeleteChoiceInfo choiceName -> over _choiceInfo $ Map.delete choiceName
  assign (_hasUnsavedChanges) true
  where
  setChoiceDescription :: String -> ChoiceInfo -> ChoiceInfo
  setChoiceDescription newDescription x = x
    { choiceDescription = newDescription }

  setChoiceFormat :: NumberFormat -> ChoiceInfo -> ChoiceInfo
  setChoiceFormat newChoiceFormat x = x { choiceFormat = newChoiceFormat }

  setValueParameterDescription
    :: String -> ValueParameterInfo -> ValueParameterInfo
  setValueParameterDescription newDescription x = x
    { valueParameterDescription = newDescription }

  setValueParameterFormat
    :: NumberFormat -> ValueParameterInfo -> ValueParameterInfo
  setValueParameterFormat newValueParameterFormat x = x
    { valueParameterFormat = newValueParameterFormat }
