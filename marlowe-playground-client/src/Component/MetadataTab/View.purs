module Component.MetadataTab.View (metadataView) where

import Prologue hiding (div, min)

import Component.MetadataTab.Types (MetadataAction(..))
import Contrib.Data.Array.Builder (cons, unsafeBuild) as AB
import Contrib.Data.List.Infinite.Finalize (zip) as Infinite.Finalize
import Contrib.Halogen.Components.Sortable (DragHandlers(..), GenDragHandlers)
import Data.Array (concat, concatMap)
import Data.Array (length) as Array
import Data.Foldable (foldMap, foldMapDefaultL)
import Data.Int as Int
import Data.Lens (Lens', (^.))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Monoid (guard) as Monoid
import Data.Set (Set, toUnfoldable)
import Data.Set.Ordered.OSet (OSet)
import Data.Tuple.Nested (type (/\), (/\))
import Halogen.Classes (btn, minusBtn, plusBtn)
import Halogen.HTML
  ( ClassName(..)
  , HTML
  , button
  , div
  , div_
  , em_
  , h6_
  , input
  , option
  , select
  , span
  , text
  )
import Halogen.HTML.Events (onClick, onValueChange)
import Halogen.HTML.Properties
  ( InputType(..)
  , class_
  , classes
  , draggable
  , min
  , placeholder
  , required
  , selected
  , type_
  , value
  )
import Language.Marlowe.Extended.V1.Metadata
  ( contractTypeArray
  , contractTypeInitials
  , contractTypeName
  , defaultForFormatType
  , fromString
  , getFormatType
  , initialsToContractType
  , isDecimalFormat
  , isDefaultFormat
  , toString
  )
import Language.Marlowe.Extended.V1.Metadata.Lenses
  ( _choiceInfo
  , _choiceNames
  , _roleDescriptions
  , _roles
  , _timeParameterDescriptions
  , _timeParameters
  , _valueParameterInfo
  , _valueParameters

  )
import Language.Marlowe.Extended.V1.Metadata.Types
  ( ChoiceInfo
  , MetaData
  , MetadataHintInfo
  , NumberFormat(..)
  , NumberFormatType(..)
  )
import Record (merge, set) as Record
import Type.Prelude (Proxy(..))

type SubformOptsRow v =
  ( key :: String
  , value :: v
  , needed :: Boolean
  , sortable :: Boolean
  , typeNameTitle :: String
  , typeNameSmall :: String
  )

type SubformOpts v = { | SubformOptsRow v }

propLabel :: forall a p. { sortable :: Boolean, title :: String } -> HTML p a
propLabel { sortable, title } = div [ class_ $ ClassName "metadata-prop-label" ]
  $
    Monoid.guard sortable
      [ span [ class_ $ ClassName "metadata-prop-drag" ] [ text "☰" ] ]
      <> [ text $ title ]

onlyDescriptionRenderer
  :: forall a p r
   . { delete :: String -> a, set :: String -> String -> a | r }
  -> SubformOpts String
  -> Array (HTML p a)
onlyDescriptionRenderer
  actions
  opts =
  [ propLabel
      { sortable: opts.sortable
      , title: opts.typeNameTitle <> " " <> show opts.key <> ": "
      }
  , div [ class_ $ ClassName "metadata-prop-edit" ]
      [ input
          [ type_ InputText
          , placeholder $ "Description for " <> opts.typeNameSmall <> " " <>
              show opts.key
          , class_ $ ClassName "metadata-input"
          , value opts.value
          , onValueChange $ actions.set opts.key
          ]
      ]
  , div [ class_ $ ClassName "metadata-prop-delete" ]
      [ button
          [ classes
              [ if opts.needed then plusBtn else minusBtn
              , ClassName "align-top"
              , btn
              ]
          , onClick $ const $ actions.delete opts.key
          ]
          [ text "-" ]
      ]
  ]
    <>
      if opts.needed then []
      else
        [ div
            [ classes
                [ ClassName "metadata-error"
                , ClassName "metadata-prop-not-used"
                ]
            ]
            [ text "Not used" ]
        ]

type FormattedNumberInfo a =
  { setFormat :: String -> NumberFormat -> a
  , setDescription :: String -> String -> a
  , deleteInfo :: String -> a
  | SubformOptsRow
      { description :: String
      , format :: NumberFormat
      }
  }

formattedNumberMetadataRenderer
  :: forall a p
   . FormattedNumberInfo a
  -> Array (HTML p a)
formattedNumberMetadataRenderer
  { key
  , needed
  , value: { description, format }
  , setFormat
  , setDescription
  , deleteInfo
  , typeNameTitle
  , typeNameSmall
  , sortable
  } =
  [ propLabel
      { sortable: sortable
      , title: typeNameTitle <> " " <> show key <> ": "
      }
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
          [ classes
              [ if needed then plusBtn else minusBtn
              , ClassName "align-top"
              , btn
              ]
          , onClick $ const $ deleteInfo key
          ]
          [ text "-" ]
      ]
  ]
    <>
      ( if needed then []
        else
          [ div
              [ classes
                  [ ClassName "metadata-error"
                  , ClassName "metadata-prop-not-used"
                  ]
              ]
              [ text "Not used" ]
          ]
      )
    <> case format of
      DefaultFormat -> []
      DecimalFormat numDecimals labelStr ->
        [ div [ class_ $ ClassName "metadata-prop-formattednum-col1" ]
            [ input
                [ type_ InputNumber
                , placeholder $ "Number of decimal digits for " <> typeNameSmall
                    <> " "
                    <> show key
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
                , placeholder $ "Currency label for " <> typeNameSmall <> " " <>
                    show key
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

choiceMetadataRenderer
  :: forall p
   . SubformOpts ChoiceInfo
  -> Array (HTML p MetadataAction)
choiceMetadataRenderer
  { key, needed, value, sortable, typeNameTitle, typeNameSmall } =
  formattedNumberMetadataRenderer
    { key: key
    , value:
        { description: value.choiceDescription
        , format: value.choiceFormat
        }
    , needed
    , setFormat: SetChoiceFormat
    , setDescription: SetChoiceDescription
    , sortable
    , typeNameTitle
    , typeNameSmall
    , deleteInfo: DeleteChoiceInfo
    }

metadataList
  :: forall a c p r v
   . Monoid v
  => { set :: String -> v -> a | r }
  -> Map String c
  -> Set String
  -> (SubformOpts c -> Array (HTML p a))
  -> String
  -> String
  -> Array (HTML p a)
metadataList
  actions
  metadataMap
  hintSet
  metadataRenderer
  typeNameTitle
  typeNameSmall =
  if Map.isEmpty combinedMap then
    []
  else
    [ div [ class_ $ ClassName "metadata-group-title" ]
        [ h6_ [ em_ [ text $ typeNameTitle <> " descriptions" ] ] ]
    ]
      <>
        ( concatMap
            ( \(key /\ val) ->
                ( case val of
                    Just (value /\ needed) -> metadataRenderer
                      { key
                      , value
                      , needed
                      , typeNameTitle
                      , typeNameSmall
                      , sortable: false
                      }
                    Nothing ->
                      [ div
                          [ classes
                              [ ClassName "metadata-error"
                              , ClassName "metadata-prop-not-defined"
                              ]
                          ]
                          [ text $ typeNameTitle <> " " <> show key <>
                              " meta-data not defined"
                          ]
                      , div [ class_ $ ClassName "metadata-prop-create" ]
                          [ button
                              [ classes [ minusBtn, ClassName "align-top", btn ]
                              , onClick $ const $ (actions.set key mempty)
                              ]
                              [ text "+" ]
                          ]
                      ]

                )
            )
            $ Map.toUnfoldable combinedMap
        )
  where
  mergeMaps
    :: forall c2
     . (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
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
      ( Map.fromFoldable
          (map (\x -> x /\ Nothing) ((toUnfoldable hintSet) :: List String))
      )

sortableMetadataList
  :: forall a c p r v
   . Monoid v
  => { dragging :: DraggingState a
     , set :: String -> v -> a
     | r
     }
  -> OMap String c
  -> OSet String
  -> (SubformOpts c -> Array (HTML p a))
  -> String
  -> String
  -> Array (HTML p a)
sortableMetadataList
  actions
  metadataMap
  hintSet
  metadataRenderer
  typeNameTitle
  typeNameSmall = do
  let
    items :: Array _
    items = Infinite.Finalize.zip
      actions.dragging.genDragHandlers
      (OMap.toUnfoldable combinedMap :: Array _)
    itemsCount = Array.length items
    sortable = itemsCount > 1
    row key value needed h = do
      let
        props =
          if sortable then
            [ h.onDragEnd
            , h.onDragStart
            , h.onDragEnter
            , class_ $ ClassName "metadata-sortable-form-row"
            , draggable true
            ]
          else
            [ class_ $ ClassName "metadata-sortable-form-row" ]
        children = metadataRenderer
          { key, needed, typeNameTitle, typeNameSmall, sortable, value }
      div props children
  if OMap.isEmpty combinedMap then
    []
  else AB.unsafeBuild $
    AB.cons do
      div [ class_ $ ClassName "metadata-group-title" ]
        [ h6_ [ em_ [ text $ typeNameTitle <> " descriptions" ] ] ]
      <> items `flip foldMapDefaultL` \(DragHandlers h /\ key /\ item) ->
        case item of
          Just (val /\ needed) -> AB.cons $ row key val needed h
          Nothing -> AB.cons $ div
            [ class_ $ ClassName "metadata-sortable-form-row" ]
            [ div
                [ classes
                    [ ClassName "metadata-error"
                    , ClassName "metadata-prop-not-defined"
                    ]
                ]
                [ text $ typeNameTitle <> " " <> show key <>
                    " meta-data not defined"
                ]
            , div [ class_ $ ClassName "metadata-prop-create" ]
                [ button
                    [ classes [ minusBtn, ClassName "align-top", btn ]
                    , onClick $ const $ actions.set key mempty
                    ]
                    [ text "+" ]
                ]
            ]
  where
  mergeMaps
    :: forall c2
     . (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
    -> (Maybe (c2 /\ Boolean))
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

type DraggingState a =
  { dragged :: Maybe Int
  , genDragHandlers :: GenDragHandlers a
  }

type Dragging a =
  { timeParameterDescriptions :: DraggingState a
  , valueParameterInfos :: DraggingState a
  }

type Handlers a =
  { raise :: MetadataAction -> a
  , dragging :: Dragging a
  }

metadataView
  :: forall a p
   . Handlers a
  -> MetadataHintInfo
  -> MetaData
  -> HTML p a
metadataView handlers metadataHints metadata = div_
  [ div [ classes [ ClassName "metadata-form" ] ] $ concat
      [ [ div [ class_ $ ClassName "metadata-mainprop-label" ]
            [ text "Contract type: " ]
        , div [ class_ $ ClassName "metadata-mainprop-edit" ]
            [ select
                [ class_ $ ClassName "metadata-input"
                , onValueChange $ handlers.raise <<< SetContractType <<<
                    initialsToContractType
                ]
                do
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
                , onValueChange $ handlers.raise <<< SetContractName
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
                , onValueChange $ handlers.raise <<< SetContractShortDescription
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
                , onValueChange $ handlers.raise <<< SetContractLongDescription
                ]
            ]
        ]
      ]
  , div [ classes [ ClassName "metadata-form" ] ] do
      let
        actions =
          { delete: handlers.raise <<< DeleteRoleDescription
          , set: map handlers.raise <<< SetRoleDescription
          }
        render = onlyDescriptionRenderer actions
      generateMetadataList
        actions
        _roleDescriptions
        _roles
        render
        "Role"
        "role"
  , div [ classes [ ClassName "metadata-form" ] ] do
      let
        actions = { set: SetChoiceDescription }

      map handlers.raise <$> generateMetadataList
        actions
        _choiceInfo
        _choiceNames
        choiceMetadataRenderer
        "Choice"
        "choice"
  , div
      [ classes
          [ ClassName "sortable-metadata-form"
          , ClassName "metadata-only-description"
          ]
      ]
      do
        let
          actions =
            { delete: handlers.raise <<< DeleteTimeParameterDescription
            , dragging: handlers.dragging.timeParameterDescriptions
            , set: map handlers.raise <<< SetTimeParameterDescription
            }
          render = onlyDescriptionRenderer actions
        generateSortableMetadataList
          actions
          _timeParameterDescriptions
          _timeParameters
          render
          "Time parameter"
          "time parameter"
  , div [ classes [ ClassName "sortable-metadata-form" ] ] do
      let
        actions =
          { set: map handlers.raise <<< SetValueParameterDescription
          , dragging: handlers.dragging.valueParameterInfos
          }
        render opts = do
          let
            value =
              { description: opts.value.valueParameterDescription
              , format: opts.value.valueParameterFormat
              }
          formattedNumberMetadataRenderer
            $ Record.set (Proxy :: Proxy "value") value
            $ Record.merge
                { setFormat: map handlers.raise <<< SetValueParameterFormat
                , setDescription: map handlers.raise <<<
                    SetValueParameterDescription
                , deleteInfo: handlers.raise <<< DeleteValueParameterInfo
                }
                opts

      generateSortableMetadataList
        actions
        _valueParameterInfo
        _valueParameters
        render
        "Value parameter"
        "value parameter"
  ]
  where
  generateMetadataList
    :: forall b c v r
     . Monoid v
    => { set :: String -> v -> b | r }
    -> Lens' MetaData (Map String c)
    -> Lens' MetadataHintInfo (Set String)
    -> (SubformOpts c -> Array (HTML p b))
    -> String
    -> String
    -> Array (HTML p b)
  generateMetadataList actions mapLens setLens =
    metadataList actions (metadata ^. mapLens) (metadataHints ^. setLens)

  generateSortableMetadataList
    :: forall c r v
     . Monoid v
    => { dragging :: DraggingState a
       , set :: String -> v -> a
       | r
       }
    -> Lens' MetaData (OMap String c)
    -> Lens' MetadataHintInfo (OSet String)
    -> (SubformOpts c -> Array (HTML p a))
    -> String
    -> String
    -> Array (HTML p a)
  generateSortableMetadataList actions mapLens setLens render label labelLower =
    sortableMetadataList
      actions
      (metadata ^. mapLens)
      (metadataHints ^. setLens)
      render
      label
      labelLower
