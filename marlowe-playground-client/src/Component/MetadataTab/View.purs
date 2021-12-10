module Component.MetadataTab.View where

import Prologue hiding (div, min)
import Component.MetadataTab.Types (MetadataAction, metadataAction)
import Contrib.Record (mkRecordProxies)
import Data.Array (concat, concatMap)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens (Lens', (^.))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Set (Set, toUnfoldable)
import Data.Set.Ordered.OSet (OSet)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.Classes (minusBtn, plusBtn, btn)
import Halogen.HTML (ClassName(..), HTML, button, div, em_, h6_, input, option, select, text)
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties (InputType(..), class_, classes, min, placeholder, required, selected, type_, value)
import Marlowe.Extended (contractTypeArray, contractTypeInitials, contractTypeName, initialsToContractType)
import Marlowe.Extended.Metadata (ChoiceInfo, MetaData, MetadataHintInfo, NumberFormat(..), NumberFormatType(..), ValueParameterInfo, _choiceInfo, _choiceNames, _roleDescriptions, _roles, _slotParameterDescriptions, _slotParameters, _valueParameterInfo, _valueParameters, defaultForFormatType, fromString, getFormatType, isDecimalFormat, isDefaultFormat, toString, updateChoiceInfo, updateValueParameterInfo)
import Type.Prelude (Proxy(..))

proxies ::
  { choiceInfo :: Proxy "choiceInfo"
  , contractLongDescription :: Proxy "contractLongDescription"
  , contractName :: Proxy "contractName"
  , contractShortDescription :: Proxy "contractShortDescription"
  , contractType :: Proxy "contractType"
  , roleDescriptions :: Proxy "roleDescriptions"
  , slotParameterDescriptions :: Proxy "slotParameterDescriptions"
  , valueParameterInfo :: Proxy "valueParameterInfo"
  }
proxies = mkRecordProxies (Proxy :: Proxy MetaData)

onlyDescriptionRenderer :: forall a p. (String -> String -> a) -> (String -> a) -> String -> String -> Boolean -> String -> String -> Array (HTML p a)
onlyDescriptionRenderer setAction deleteAction key info needed typeNameTitle typeNameSmall =
  [ div [ class_ $ ClassName "metadata-prop-label" ]
      [ text $ typeNameTitle <> " " <> show key <> ": " ]
  , div [ class_ $ ClassName "metadata-prop-edit" ]
      [ input
          [ type_ InputText
          , placeholder $ "Description for " <> typeNameSmall <> " " <> show key
          , class_ $ ClassName "metadata-input"
          , value info
          , onValueChange $ setAction key
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-delete" ]
      [ button
          [ classes [ if needed then plusBtn else minusBtn, ClassName "align-top", btn ]
          , onClick $ const $ deleteAction key
          ]
          [ text "-" ]
      ]
  ]
    <> if needed then [] else [ div [ classes [ ClassName "metadata-error", ClassName "metadata-prop-not-used" ] ] [ text "Not used" ] ]

type FormattedNumberInfo a
  = { key :: String
    , description :: String
    , format :: NumberFormat
    , setFormat :: String -> NumberFormat -> a
    , setDescription :: String -> String -> a
    , deleteInfo :: String -> a
    }

formattedNumberMetadataRenderer :: forall a p. FormattedNumberInfo a -> Boolean -> String -> String -> Array (HTML p a)
formattedNumberMetadataRenderer { key, description, format, setFormat, setDescription, deleteInfo } needed typeNameTitle typeNameSmall =
  [ div [ class_ $ ClassName "metadata-prop-label" ]
      [ text $ typeNameTitle <> " " <> show key <> ": " ]
  , div [ class_ $ ClassName "metadata-prop-formattednum-col1" ]
      [ select
          [ class_ $ ClassName "metadata-input"
          , onValueChange $ setFormat key <<< setNumberFormatType
          ]
          [ option
              [ value $ toString DefaultFormatType
              , selected $ isDefaultFormat format
              ]
              [ text $ "Default format"
              ]
          , option
              [ value $ toString DecimalFormatType
              , selected $ isDecimalFormat format
              ]
              [ text $ "Fixed point amount"
              ]
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-formattednum-col2" ]
      [ input
          [ type_ InputText
          , placeholder $ "Description for " <> typeNameSmall <> " " <> show key
          , class_ $ ClassName "metadata-input"
          , value description
          , onValueChange $ setDescription key
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-delete" ]
      [ button
          [ classes [ if needed then plusBtn else minusBtn, ClassName "align-top", btn ]
          , onClick $ const $ deleteInfo key
          ]
          [ text "-" ]
      ]
  ]
    <> (if needed then [] else [ div [ classes [ ClassName "metadata-error", ClassName "metadata-prop-not-used" ] ] [ text "Not used" ] ])
    <> case format of
        DefaultFormat -> []
        DecimalFormat numDecimals labelStr ->
          [ div [ class_ $ ClassName "metadata-prop-formattednum-col1" ]
              [ input
                  [ type_ InputNumber
                  , placeholder $ "Number of decimal digits for " <> typeNameSmall <> " " <> show key
                  , class_ $ ClassName "metadata-input"
                  , value $ if numDecimals == 0 then "" else show numDecimals
                  , required true
                  , min zero
                  , onValueChange $ setFormat key <<< setDecimals labelStr
                  ]
              ]
          , div [ class_ $ ClassName "metadata-prop-formattednum-col2" ]
              [ input
                  [ type_ InputText
                  , placeholder $ "Currency label for " <> typeNameSmall <> " " <> show key
                  , class_ $ ClassName "metadata-input"
                  , value labelStr
                  , onValueChange $ setFormat key <<< DecimalFormat numDecimals
                  ]
              ]
          ]
        TimeFormat -> []
  where
  setNumberFormatType :: String -> NumberFormat
  setNumberFormatType str = case fromString str of
    Just formatType
      | formatType == getFormatType format -> format
      | otherwise -> defaultForFormatType formatType
    Nothing -> defaultForFormatType DefaultFormatType

  setDecimals :: String -> String -> NumberFormat
  setDecimals labelStr x =
    DecimalFormat
      ( case Int.fromString x of
          Just y
            | y >= 0 -> y
          _ -> 0
      )
      labelStr

choiceMetadataRenderer :: forall p. String -> ChoiceInfo -> Boolean -> String -> String -> Array (HTML p MetadataAction)
choiceMetadataRenderer key { choiceDescription, choiceFormat } = do
  let
    choiceInfo = metadataAction proxies.choiceInfo
  formattedNumberMetadataRenderer
    { key: key
    , description: choiceDescription
    , format: choiceFormat
    , setFormat:
        \name format ->
          choiceInfo
            $ updateChoiceInfo (_ { choiceFormat = format }) name
    , setDescription:
        \name description ->
          choiceInfo
            $ updateChoiceInfo (_ { choiceDescription = description }) name
    , deleteInfo: choiceInfo <<< Map.delete
    }

valueParameterMetadataRenderer :: forall p. String -> ValueParameterInfo -> Boolean -> String -> String -> Array (HTML p MetadataAction)
valueParameterMetadataRenderer key { valueParameterDescription, valueParameterFormat } = do
  let
    valueParameterInfo = metadataAction proxies.valueParameterInfo
  formattedNumberMetadataRenderer
    { key: key
    , description: valueParameterDescription
    , format: valueParameterFormat
    , setFormat:
        \name format ->
          valueParameterInfo
            $ updateValueParameterInfo (_ { valueParameterFormat = format }) name
    , setDescription:
        \name description ->
          valueParameterInfo
            $ updateValueParameterInfo (_ { valueParameterDescription = description }) name
    , deleteInfo: valueParameterInfo <<< OMap.delete
    }

metadataList ::
  forall b c p.
  Map String c ->
  Set String ->
  (String -> c -> Boolean -> String -> String -> Array (HTML p b)) ->
  String -> String -> (String -> b) -> Array (HTML p b)
metadataList metadataMap hintSet metadataRenderer typeNameTitle typeNameSmall setEmptyMetadata =
  if Map.isEmpty combinedMap then
    []
  else
    [ div [ class_ $ ClassName "metadata-group-title" ]
        [ h6_ [ em_ [ text $ typeNameTitle <> " descriptions" ] ] ]
    ]
      <> ( concatMap
            ( \(key /\ val) ->
                ( case val of
                    Just (info /\ needed) -> metadataRenderer key info needed typeNameTitle typeNameSmall
                    Nothing ->
                      [ div [ classes [ ClassName "metadata-error", ClassName "metadata-prop-not-defined" ] ]
                          [ text $ typeNameTitle <> " " <> show key <> " meta-data not defined" ]
                      , div [ class_ $ ClassName "metadata-prop-create" ]
                          [ button
                              [ classes [ minusBtn, ClassName "align-top", btn ]
                              , onClick $ const $ (setEmptyMetadata key)
                              ]
                              [ text "+" ]
                          ]
                      ]
                )
            )
            $ Map.toUnfoldable combinedMap
        )
  where
  mergeMaps :: forall c2. (Maybe (c2 /\ Boolean)) -> (Maybe (c2 /\ Boolean)) -> (Maybe (c2 /\ Boolean))
  mergeMaps (Just (x /\ _)) _ = Just (x /\ true)

  mergeMaps _ _ = Nothing

  -- The value of the Map has the following meaning:
  -- * Nothing means the entry is in the contract but not in the metadata
  -- * Just (_ /\ false) means the entry is in the metadata but not in the contract
  -- * Just (_ /\ true) means the entry is both in the contract and in the metadata
  -- If it is nowhere we just don't store it in the map
  combinedMap =
    Map.unionWith mergeMaps
      (map (\x -> Just (x /\ false)) metadataMap)
      (Map.fromFoldable (map (\x -> x /\ Nothing) ((toUnfoldable hintSet) :: List String)))

sortableMetadataList ::
  forall a c p.
  OMap String c ->
  OSet String ->
  (String -> c -> Boolean -> String -> String -> Array (HTML p a)) ->
  String -> String -> (String -> a) -> Array (HTML p a)
sortableMetadataList metadataMap hintSet metadataRenderer typeNameTitle typeNameSmall setEmptyMetadata =
  if OMap.isEmpty combinedMap then
    []
  else
    [ div [ class_ $ ClassName "metadata-group-title" ]
        [ h6_ [ em_ [ text $ typeNameTitle <> " descriptions" ] ] ]
    ]
      <> ( concatMap
            ( \(key /\ val) ->
                ( case val of
                    Just (info /\ needed) -> metadataRenderer key info needed typeNameTitle typeNameSmall
                    Nothing ->
                      [ div [ classes [ ClassName "metadata-error", ClassName "metadata-prop-not-defined" ] ]
                          [ text $ typeNameTitle <> " " <> show key <> " meta-data not defined" ]
                      , div [ class_ $ ClassName "metadata-prop-create" ]
                          [ button
                              [ classes [ minusBtn, ClassName "align-top", btn ]
                              , onClick $ const $ setEmptyMetadata key
                              ]
                              [ text "+" ]
                          ]
                      ]
                )
            )
            $ OMap.toUnfoldable combinedMap
        )
  where
  mergeMaps :: forall c2. (Maybe (c2 /\ Boolean)) -> (Maybe (c2 /\ Boolean)) -> (Maybe (c2 /\ Boolean))
  mergeMaps (Just (x /\ _)) _ = Just (x /\ true)

  mergeMaps _ _ = Nothing

  -- The value of the Map has the following meaning:
  -- * Nothing means the entry is in the contract but not in the metadata
  -- * Just (_ /\ false) means the entry is in the metadata but not in the contract
  -- * Just (_ /\ true) means the entry is both in the contract and in the metadata
  -- If it is nowhere we just don't store it in the map
  combinedMap =
    OMap.unionWith mergeMaps
      (map (\x -> Just (x /\ false)) metadataMap)
      (foldMap (\x -> OMap.singleton x Nothing) hintSet)

metadataView :: forall p. MetadataHintInfo -> MetaData -> HTML p MetadataAction
metadataView metadataHints metadata =
  div [ classes [ ClassName "metadata-form" ] ]
    ( concat
        [ [ div [ class_ $ ClassName "metadata-mainprop-label" ]
              [ text "Contract type: " ]
          , div [ class_ $ ClassName "metadata-mainprop-edit" ]
              [ select
                  [ class_ $ ClassName "metadata-input"
                  , onValueChange $ metadataAction proxies.contractType <<< const <<< initialsToContractType
                  ] do
                  ct <- contractTypeArray
                  pure
                    $ option
                        [ value $ contractTypeInitials ct
                        , selected (ct == metadata.contractType)
                        ]
                        [ text $ contractTypeName ct
                        ]
              ]
          ]
        , [ div [ class_ $ ClassName "metadata-mainprop-label" ]
              [ text "Contract name: " ]
          , div [ class_ $ ClassName "metadata-mainprop-edit" ]
              [ input
                  [ type_ InputText
                  , placeholder "Contract name"
                  , class_ $ ClassName "metadata-input"
                  , value metadata.contractName
                  , onValueChange $ metadataAction proxies.contractName <<< const
                  ]
              ]
          ]
        , [ div [ class_ $ ClassName "metadata-mainprop-label" ]
              [ text "Contract short description: " ]
          , div [ class_ $ ClassName "metadata-mainprop-edit" ]
              [ input
                  [ type_ InputText
                  , placeholder "Contract description"
                  , class_ $ ClassName "metadata-input"
                  , value metadata.contractShortDescription
                  , onValueChange $ metadataAction proxies.contractShortDescription <<< const
                  ]
              ]
          ]
        , [ div [ class_ $ ClassName "metadata-mainprop-label" ]
              [ text "Contract long description: " ]
          , div [ class_ $ ClassName "metadata-mainprop-edit" ]
              [ input
                  [ type_ InputText
                  , placeholder "Contract description"
                  , class_ $ ClassName "metadata-input"
                  , value metadata.contractLongDescription
                  , onValueChange $ metadataAction proxies.contractLongDescription <<< const
                  ]
              ]
          ]
        , generateMetadataList
            _roleDescriptions
            _roles
            (onlyDescriptionRenderer setRoleDecription deleteRoleDescription)
            "Role"
            "role"
            (flip setRoleDecription mempty)
        , generateMetadataList _choiceInfo _choiceNames choiceMetadataRenderer "Choice" "choice" (flip setChoiceDescription mempty)
        , generateSortableMetadataList _slotParameterDescriptions _slotParameters (onlyDescriptionRenderer setSlotParameterDescription deleteSlotParameterDescription) "Slot parameter" "slot parameter" (flip setSlotParameterDescription mempty)
        , generateSortableMetadataList _valueParameterInfo _valueParameters valueParameterMetadataRenderer "Value parameter" "value parameter" (flip setValueParameterDescription mempty)
        ]
    )
  where
  setRoleDecription k = metadataAction proxies.roleDescriptions <<< Map.insert k

  deleteRoleDescription = metadataAction proxies.roleDescriptions <<< Map.delete

  setSlotParameterDescription k = metadataAction proxies.slotParameterDescriptions <<< OMap.insert k

  deleteSlotParameterDescription = metadataAction proxies.slotParameterDescriptions <<< OMap.delete

  setChoiceDescription name description =
    metadataAction proxies.choiceInfo
      $ updateChoiceInfo (_ { choiceDescription = description }) name

  setValueParameterDescription name description =
    metadataAction proxies.valueParameterInfo
      $ updateValueParameterInfo (_ { valueParameterDescription = description }) name

  generateMetadataList ::
    forall c.
    Lens' MetaData (Map String c) ->
    Lens' MetadataHintInfo (Set String) ->
    (String -> c -> Boolean -> String -> String -> Array (HTML p _)) ->
    String ->
    String ->
    (String -> _) ->
    Array (HTML p _)
  generateMetadataList mapLens setLens = metadataList (metadata ^. mapLens) (metadataHints ^. setLens)

  generateSortableMetadataList ::
    forall c.
    Lens' MetaData (OMap String c) ->
    Lens' MetadataHintInfo (OSet String) ->
    (String -> c -> Boolean -> String -> String -> Array (HTML p _)) ->
    String ->
    String ->
    (String -> _) ->
    Array (HTML p MetadataAction)
  generateSortableMetadataList mapLens setLens = sortableMetadataList (metadata ^. mapLens) (metadataHints ^. setLens)
