module Component.Template.View (contractTemplateCard) where

import Prologue hiding (Either(..), div)

import Component.Contacts.State (adaToken, getAda)
import Component.ContractSetup (_contractSetup)
import Component.ContractSetup as ContractSetup
import Component.ContractSetup.Types (ContractParams)
import Component.Hint.State (hint)
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Component.LoadingSubmitButton.State (loadingSubmitButton)
import Component.Popper (Placement(..))
import Component.Template.Types (Action(..), State(..))
import Css as Css
import Data.ContractTimeout (ContractTimeout)
import Data.ContractTimeout as CT
import Data.ContractValue (ContractValue)
import Data.ContractValue as CV
import Data.Lens (view)
import Data.Map as Map
import Data.Map.Ordered.OMap (OMap)
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, maybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen.Css (classNames)
import Halogen.HTML
  ( ComponentHTML
  , HTML
  , PlainHTML
  , a
  , button
  , div
  , div_
  , h2
  , h3
  , h4
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
import Halogen.Store.Monad (class MonadStore)
import Humanize (contractIcon, humanizeValue)
import MainFrame.Types (ChildSlots)
import Marlowe.Extended.Metadata
  ( ContractTemplate
  , MetaData
  , NumberFormat(..)
  , ValueParameterInfo
  , _metaData
  , _slotParameterDescriptions
  )
import Marlowe.Market (contractTemplates)
import Marlowe.PAB (contractCreationFee)
import Marlowe.Semantics (Assets)
import Store as Store
import Text.Markdown.TrimmedInline (markdownToHTML)

contractTemplateCard
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => Assets
  -> State
  -> ComponentHTML Action ChildSlots m
contractTemplateCard assets state =
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
    , case state of
        Start -> contractSelection
        Overview template -> contractOverview template
        Setup _ input _ -> HH.slot
          _contractSetup
          unit
          ContractSetup.component
          input
          OnContractSetupMsg
        Review template params -> contractReview assets template params
    ]

------------------------------------------------------------
contractTemplateBreadcrumb
  :: forall p. State -> HTML p Action
contractTemplateBreadcrumb contractSetupStage =
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
    case contractSetupStage of
      Start -> [ activeItem "Templates" ]
      Overview template ->
        [ previousItem "Templates" OnReset
        , arrow
        , activeItem template.metaData.contractName
        ]
      Setup template _ _ ->
        [ previousItem "Templates" OnReset
        , arrow
        , previousItem template.metaData.contractName
            $ OnTemplateChosen template
        , arrow
        , activeItem "Setup"
        ]
      Review template params ->
        [ previousItem "Templates" OnReset
        , arrow
        , previousItem template.metaData.contractName
            $ OnTemplateChosen template
        , arrow
        , previousItem "Setup" $ OnSetup template (Just params)
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
    [ ul_ $ contractTemplateLink <$> contractTemplates
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
  contractTemplateLink contractTemplate =
    li
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
      ]
      [ contractIcon contractTemplate.metaData.contractType
      , div_
          [ h2
              [ classNames [ "font-semibold", "mb-2" ] ]
              [ text contractTemplate.metaData.contractName ]
          , p
              [ classNames [ "font-xs" ] ]
              $ markdownToHTML
                  contractTemplate.metaData.contractShortDescription
          ]
      , icon_ Icon.Next
      ]

contractOverview :: forall p. ContractTemplate -> HTML p Action
contractOverview contractTemplate =
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
            , onClick_ $ OnSetup contractTemplate Nothing
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
  -> ComponentHTML Action ChildSlots m
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
              [ ul_ $ slotParameter metaData <$> Map.toUnfoldable timeouts
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
slotParameter
  :: forall m
   . MonadAff m
  => MetaData
  -> Tuple String ContractTimeout
  -> ComponentHTML Action ChildSlots m
slotParameter metaData (key /\ timeout) =
  let
    slotParameterDescriptions = view _slotParameterDescriptions metaData

    description = fromMaybe "no description available" $ OMap.lookup key
      slotParameterDescriptions
  in
    parameter key description $ CT.toString timeout <> " minutes"

valueParameter
  :: forall m
   . MonadAff m
  => OMap String ValueParameterInfo
  -> Tuple String ContractValue
  -> ComponentHTML Action ChildSlots m
valueParameter infoMap (key /\ value) =
  let
    info = OMap.lookup key infoMap

    format = maybe DefaultFormat _.valueParameterFormat info

    description =
      maybe "no description available" _.valueParameterDescription info

    formattedValue = CV.toString format value
  in
    parameter key description formattedValue

parameter
  :: forall m
   . MonadAff m
  => String
  -> String
  -> String
  -> ComponentHTML Action ChildSlots m
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

-- TODO: This function is also included in the Marlowe Playground code. We could/should move it
-- into a shared folder, but it's not obvious where. It could go in the Hint module, but then it
-- would introduce an unnecessary markdown dependency into the Plutus Playground. So some more
-- thought/restructuring is required.
markdownHintWithTitle :: String -> String -> PlainHTML
markdownHintWithTitle title markdown =
  div_
    $
      [ h4
          -- With min-w-max we define that the title should never break into
          -- a different line.
          [ classNames
              [ "no-margins"
              , "text-lg"
              , "font-semibold"
              , "flex"
              , "items-center"
              , "pb-2"
              , "min-w-max"
              ]
          ]
          [ icon Icon.HelpOutline [ "mr-1", "font-normal" ]
          , text title
          ]
      ]
        <> markdownToHTML markdown
