module Component.Template.View (render) where

import Prologue hiding (Either(..), div)

import Capability.Toast (class Toast)
import Component.AddContact (_addContact)
import Component.AddContact as AddContact
import Component.ContractSetup (_contractSetup, markdownHintWithTitle)
import Component.ContractSetup as ContractSetup
import Component.ContractSetup.Types (ContractFields, ContractParams)
import Component.Hint.State (hint)
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Component.LoadingSubmitButton.State (loadingSubmitButton)
import Component.Popper (Placement(..))
import Component.Template.Types (Action(..), ComponentHTML, State, Wizard(..))
import Css as Css
import Data.Array (mapWithIndex)
import Data.ContractTimeout (ContractTimeout)
import Data.ContractTimeout as CT
import Data.ContractValue (ContractValue)
import Data.ContractValue as CV
import Data.Lens (view, (^.))
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.PABConnectedWallet (_assets)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen.Css (classNames)
import Halogen.HTML
  ( HTML
  , a
  , button
  , div
  , div_
  , h2
  , h3
  , h4_
  , li
  , p
  , p_
  , span
  , span_
  , text
  , ul_
  )
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (href, id)
import Halogen.HTML.Properties.ARIA (describedBy, labelledBy)
import Halogen.Store.Monad (class MonadStore)
import Humanize (humanizeValue)
import Images (contractIcon)
import Marlowe.Extended.Metadata
  ( ContractTemplate
  , MetaData
  , ValueParameterInfo
  , _metaData
  , _timeParameterDescriptions
  )
import Marlowe.Market (contractTemplates)
import Marlowe.PAB (contractCreationFee)
import Marlowe.Semantics (Assets, adaToken, getAda)
import Store as Store
import Text.Markdown.TrimmedInline (markdownToHTML)

render
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => State
  -> ComponentHTML m
render state =
  div
    [ classNames
        [ "h-full"
        , "grid"
        , "grid-rows-auto-auto-1fr"
        , "divide-y"
        , "divide-gray"
        ]
    ]
    [ h2
        [ classNames Css.cardHeader ]
        [ text "Contract templates" ]
    , contractTemplateBreadcrumb state
    , case state.wizard of
        Start -> contractSelection
        Overview template fields -> contractOverview template fields
        Setup _ input -> HH.slot
          _contractSetup
          unit
          ContractSetup.component
          input
          OnContractSetupMsg
        AddContact name _ _ -> HH.slot
          _addContact
          unit
          AddContact.component
          { initializeNickname: name }
          OnAddContactMsg
        Review template params ->
          let
            assets = state.wallet ^. _assets
          in
            contractReview assets template params
    ]

------------------------------------------------------------
contractTemplateBreadcrumb
  :: forall p. State -> HTML p Action
contractTemplateBreadcrumb state =
  div
    [ classNames
        [ "overflow-x-auto"
        , "flex"
        , "align-baseline"
        , "px-4"
        , "gap-1"
        , "border-gray"
        , "border-b"
        , "text-xs"
        ]
    ]
    case state.wizard of
      Start -> [ activeItem "Templates" ]
      Overview template _ ->
        [ previousItem "Templates" OnBack
        , arrow
        , activeItem template.metaData.contractName
        ]
      Setup template _ ->
        [ previousItem "Templates" OnReset
        , arrow
        , previousItem template.metaData.contractName OnBack
        , arrow
        , activeItem "Setup"
        ]
      AddContact _ template _ ->
        [ previousItem "Templates" OnReset
        , arrow
        , previousItem template.metaData.contractName
            $ OnTemplateChosen template
        , arrow
        , previousItem "Setup" OnBack
        , arrow
        , activeItem "new contact"
        ]
      Review template _ ->
        [ previousItem "Templates" OnReset
        , arrow
        , previousItem template.metaData.contractName
            $ OnTemplateChosen template
        , arrow
        , previousItem "Setup" OnBack
        , arrow
        , activeItem "Review and pay"
        ]
  where
  activeItem itemText =
    span
      [ classNames
          [ "whitespace-nowrap"
          , "py-2.5"
          , "border-black"
          , "border-b-2"
          , "font-semibold"
          ]
      ]
      [ text itemText ]

  previousItem itemText action =
    a
      [ classNames
          [ "whitespace-nowrap"
          , "py-2.5"
          , "text-purple"
          , "border-transparent"
          , "border-b-2"
          , "hover:border-purple"
          , "font-semibold"
          ]
      , onClick_ action
      ]
      [ text itemText ]

  arrow = span [ classNames [ "mt-2" ] ] [ icon_ Icon.Next ]

contractSelection :: forall p. HTML p Action
contractSelection =
  div
    [ classNames [ "h-full", "overflow-y-auto" ] ]
    [ div_ $ mapWithIndex contractTemplateLink contractTemplates
    ]
  where
  -- Cautionary tale: Initially I made these `divs` inside a `div`, but because they are very
  -- similar to the overview div for the corresponding contract template, I got a weird event
  -- propgation bug when clicking on the "back" button in the contract overview section. I'm not
  -- entirely clear on what was going on, but either Halogen's diff of the DOM or the browser
  -- itself ended up thinking that the "back" button in the contract overview was inside one of
  -- these `divs` (even though they are never rendered at the same time). Anyway, changing these
  -- to `li` items inside a `ul` (a perfectly reasonable semantic choice anyway) solves this
  -- problem.
  contractTemplateLink i contractTemplate =
    a
      [ classNames
          [ "flex"
          , "gap-4"
          , "items-center"
          , "p-4"
          , "border-gray"
          , "border-b"
          , "cursor-pointer"
          ]
      , onClick_ $ OnTemplateChosen contractTemplate
      , href "#"
      , labelledBy $ "contract-name-" <> show i
      , describedBy $ "contract-description-" <> show i
      ]
      [ contractIcon contractTemplate.metaData.contractType
      , div_
          [ h2
              [ classNames [ "font-semibold", "mb-2" ]
              , id $ "contract-name-" <> show i
              ]
              [ text contractTemplate.metaData.contractName ]
          , p
              [ classNames [ "font-xs" ]
              , id $ "contract-description-" <> show i
              ]
              $ markdownToHTML
                  contractTemplate.metaData.contractShortDescription
          ]
      , icon_ Icon.Next
      ]

contractOverview
  :: forall p. ContractTemplate -> Maybe ContractFields -> HTML p Action
contractOverview contractTemplate fields =
  div
    [ classNames [ "h-full", "grid", "grid-rows-1fr-auto" ] ]
    [ div
        [ classNames [ "h-full", "overflow-y-auto", "p-4" ] ]
        [ h2
            [ classNames
                [ "flex"
                , "gap-2"
                , "items-center"
                , "text-lg"
                , "font-semibold"
                , "mb-2"
                ]
            ]
            [ contractIcon contractTemplate.metaData.contractType
            , text $ contractTemplate.metaData.contractName <> " overview"
            ]
        , p [ classNames [ "mb-4" ] ] $ markdownToHTML
            contractTemplate.metaData.contractShortDescription
        , p_ $ markdownToHTML contractTemplate.metaData.contractLongDescription
        ]
    , div
        [ classNames
            [ "flex", "items-baseline", "p-4", "border-gray", "border-t" ]
        ]
        [ a
            [ classNames [ "flex-1", "text-center" ]
            , onClick_ OnBack
            ]
            [ text "Back" ]
        , button
            [ classNames $ Css.primaryButton <> [ "flex-1", "text-left" ] <>
                Css.withIcon Icon.ArrowRight
            , onClick_ $ OnSetup contractTemplate fields
            ]
            [ text "Setup" ]
        ]
    ]

contractReview
  :: forall m
   . MonadAff m
  => Assets
  -> ContractTemplate
  -> ContractParams
  -> ComponentHTML m
contractReview assets template params =
  let
    hasSufficientFunds = getAda assets >= contractCreationFee

    metaData = view _metaData template

    { timeouts, values } = params
  in
    div
      [ classNames
          [ "flex"
          , "flex-col"
          , "p-4"
          , "gap-4"
          , "max-h-full"
          , "overflow-y-auto"
          ]
      ]
      [ div
          [ classNames [ "rounded", "shadow" ] ]
          [ h3
              [ classNames
                  [ "flex"
                  , "gap-1"
                  , "items-center"
                  , "leading-none"
                  , "text-sm"
                  , "font-semibold"
                  , "p-2"
                  , "mb-2"
                  , "border-gray"
                  , "border-b"
                  ]
              ]
              [ icon Icon.Terms [ "text-purple" ]
              , text "Terms"
              ]
          , div
              [ classNames [ "p-4" ] ]
              [ ul_ $ timeParameter metaData <$> Map.toUnfoldable timeouts
              , ul_ $ valueParameter metaData.valueParameterInfo
                  <$> Map.toUnfoldable values
              ]
          ]
      , div
          [ classNames [ "rounded", "shadow" ] ]
          [ h3
              [ classNames
                  [ "p-4"
                  , "flex"
                  , "justify-between"
                  , "bg-lightgray"
                  , "font-semibold"
                  , "rounded-t"
                  ]
              ]
              [ span_ [ text "Demo wallet balance:" ]
              , span_ [ text $ humanizeValue adaToken $ getAda assets ]
              ]
          , div [ classNames [ "px-5", "pb-6", "md:pb-8" ] ]
              [ p
                  [ classNames [ "mt-4", "text-sm", "font-semibold" ] ]
                  [ text "Confirm payment of:" ]
              , p
                  [ classNames
                      [ "mb-4", "text-purple", "font-semibold", "text-2xl" ]
                  ]
                  [ text $ humanizeValue adaToken contractCreationFee ]
              , div
                  [ classNames [ "flex", "items-baseline" ] ]
                  [ a
                      [ classNames [ "flex-1", "text-center" ]
                      , onClick_ OnBack
                      ]
                      [ text "Back" ]
                  , loadingSubmitButton
                      { ref: "action-pay-and-start"
                      , caption: "Pay and start"
                      , styles: [ "flex-1" ]
                      , enabled: true
                      , handler: OnStartContract template params
                      }
                  ]
              , div
                  [ classNames [ "mt-4", "text-sm", "text-red" ] ]
                  if hasSufficientFunds then
                    []
                  else
                    [ text
                        "You have insufficient funds to initialise this contract."
                    ]
              ]
          ]
      ]

------------------------------------------------------------
timeParameter
  :: forall m
   . MonadAff m
  => MetaData
  -> Tuple String ContractTimeout
  -> ComponentHTML m
timeParameter metaData (key /\ timeout) =
  let
    timeParameterDescriptions = view _timeParameterDescriptions metaData

    description = fromMaybe "no description available" $ OMap.lookup key
      timeParameterDescriptions
  in
    parameter key description $ CT.toString timeout <> " minutes"

valueParameter
  :: forall m
   . MonadAff m
  => OMap String ValueParameterInfo
  -> Tuple String ContractValue
  -> ComponentHTML m
valueParameter infoMap (key /\ value) =
  let
    info = OMap.lookup key infoMap

    description =
      maybe "no description available" _.valueParameterDescription info

    formattedValue = CV.toString value
  in
    parameter key description formattedValue

parameter
  :: forall m
   . MonadAff m
  => String
  -> String
  -> String
  -> ComponentHTML m
parameter label description value =
  li
    [ classNames [ "mb-2" ] ]
    [ h4_
        [ span
            [ classNames [ "text-sm", "text-darkgray", "font-semibold" ] ]
            [ text label ]
        , hint
            [ "ml-2" ]
            ("template-parameter-" <> label)
            Auto
            (markdownHintWithTitle label description)
        ]
    , p_ [ text value ]
    ]
