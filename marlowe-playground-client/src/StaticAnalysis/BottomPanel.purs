module StaticAnalysis.BottomPanel
  ( analysisResultPane
  , analyzeButton
  , clearButton
  ) where

import Prologue hiding (div)

import Data.BigInt.Argonaut as BigInt
import Data.Lens ((^.))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List, null, toUnfoldable)
import Data.List as List
import Data.List.NonEmpty (toList)
import Data.Time.Duration (Minutes)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Classes (btn, spaceBottom, spaceRight, spaceTop, spanText)
import Halogen.HTML
  ( ClassName(..)
  , HTML
  , b_
  , br_
  , button
  , div
  , h3
  , li_
  , ol
  , span_
  , text
  , ul
  )
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, enabled)
import MainFrame.Types (ChildSlots)
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Semantics
  ( ChoiceId(..)
  , Input(..)
  , TimeInterval(..)
  , TransactionInput(..)
  )
import Marlowe.Symbolic.Types.Response as R
import Marlowe.Template (TemplateContent(..))
import Marlowe.ViewPartials (displayWarningList)
import Network.RemoteData (RemoteData(..))
import Page.Simulation.View (TemplateParameterActionsGen, templateParameters)
import Pretty (showPrettyToken)
import Servant.PureScript (printAjaxError)
import StaticAnalysis.Types
  ( AnalysisExecutionState(..)
  , AnalysisState
  , MultiStageAnalysisData(..)
  , _analysisExecutionState
  , _analysisState
  , _templateContent
  )
import Types (WarningAnalysisError(..))

analyzeButton
  :: forall p action. Boolean -> Boolean -> String -> action -> HTML p action
analyzeButton isLoading isEnabled name action =
  button
    [ onClick $ const action
    , enabled isEnabled
    , classes [ spaceTop, spaceBottom, spaceRight, btn ]
    ]
    [ text (if isLoading then "Analysing..." else name) ]

clearButton
  :: forall p action. Boolean -> String -> action -> HTML p action
clearButton isEnabled name action =
  button
    [ onClick $ const action
    , enabled isEnabled
    , classes [ spaceTop, spaceBottom, spaceRight, btn ]
    ]
    [ text name ]

analysisResultPane
  :: forall action m state
   . MonadAff m
  => MetaData
  -> TemplateParameterActionsGen action
  -> { analysisState :: AnalysisState, tzOffset :: Minutes | state }
  -> ComponentHTML action ChildSlots m
analysisResultPane metadata actionGen state =
  let
    { timeContent, valueContent } =
      state ^. (_analysisState <<< _templateContent <<< _Newtype)

    templateContent = TemplateContent { timeContent, valueContent }
    result = state ^. (_analysisState <<< _analysisExecutionState)
    tzOffset = state.tzOffset
    explanation = div [ classes [ ClassName "padded-explanation" ] ]
  in
    case result of
      NoneAsked ->
        explanation
          [ text ""
          , templateParameters
              metadata
              templateContent
              actionGen
              tzOffset
          ]
      WarningAnalysis staticSubResult -> case staticSubResult of
        NotAsked ->
          explanation
            [ text ""
            ]
        Success (R.Valid) ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Warning Analysis Result: Pass" ]
            , text
                "Static analysis could not find any execution that results in any warning."
            ]
        Success
          ( R.CounterExample
              { initialSlot, transactionList, transactionWarning }
          ) ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Warning Analysis Result: Warnings Found" ]
            , text "Static analysis found the following counterexample:"
            , ul [ classes [ ClassName "indented-enum-initial" ] ]
                [ li_
                    [ spanText "Warnings issued: "
                    , displayWarningList transactionWarning
                    ]
                , li_
                    [ spanText "Initial slot: "
                    , b_ [ spanText (BigInt.toString initialSlot) ]
                    ]
                , li_
                    [ spanText "Offending transaction list: "
                    , displayTransactionList transactionList
                    ]
                ]
            ]
        Success (R.Error str) ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Error during warning analysis" ]
            , text "Analysis failed for the following reason:"
            , ul [ classes [ ClassName "indented-enum-initial" ] ]
                [ li_
                    [ b_ [ spanText str ]
                    ]
                ]
            ]
        Failure (WarningAnalysisAjaxError error) ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Error during warning analysis" ]
            , text "Analysis failed for the following reason:"
            , ul [ classes [ ClassName "indented-enum-initial" ] ]
                [ li_
                    [ b_
                        [ spanText $ printAjaxError error
                        ]
                    ]
                ]
            ]
        Failure WarningAnalysisIsExtendedMarloweError ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Error during warning analysis" ]
            , text "Analysis failed for the following reason:"
            , ul [ classes [ ClassName "indented-enum-initial" ] ]
                [ li_
                    [ b_
                        [ spanText
                            "The code has templates. Static analysis can only be run in core Marlowe code."
                        ]
                    ]
                ]
            ]
        Loading -> text ""
      ReachabilityAnalysis reachabilitySubResult ->
        case reachabilitySubResult of
          AnalysisNotStarted ->
            explanation
              [ text ""
              ]
          AnalysisInProgress
            { numSubproblems: totalSteps
            , numSolvedSubproblems: doneSteps
            , counterExampleSubcontracts: foundcounterExampleSubcontracts
            } ->
            explanation
              ( [ text
                    ( "Reachability analysis in progress, " <> show doneSteps
                        <> " subcontracts out of "
                        <> show totalSteps
                        <> " analysed..."
                    )
                ]
                  <>
                    if null foundcounterExampleSubcontracts then
                      [ br_, text "No unreachable subcontracts found so far." ]
                    else
                      ( [ br_
                        , text
                            "Found the following unreachable subcontracts so far:"
                        ]
                          <>
                            [ ul
                                [ classes [ ClassName "indented-enum-initial" ]
                                ]
                                do
                                  contractPath <- toUnfoldable
                                    foundcounterExampleSubcontracts
                                  pure (li_ [ text (show contractPath) ])
                            ]
                      )
              )
          AnalysisFailure err ->
            explanation
              [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                  [ text "Error during reachability analysis" ]
              , text "Reachability analysis failed for the following reason:"
              , ul [ classes [ ClassName "indented-enum-initial" ] ]
                  [ li_
                      [ b_ [ spanText err ]
                      ]
                  ]
              ]
          AnalysisFoundCounterExamples { counterExampleSubcontracts } ->
            explanation
              ( [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                    [ text
                        "Reachability Analysis Result: Unreachable Subcontract Found"
                    ]
                , text
                    "Static analysis found the following subcontracts that are unreachable:"
                ]
                  <>
                    [ ul [ classes [ ClassName "indented-enum-initial" ] ] do
                        contractPath <- toUnfoldable
                          (toList counterExampleSubcontracts)
                        pure (li_ [ text (show contractPath) ])
                    ]
              )
          AnalysisFinishedAndPassed ->
            explanation
              [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                  [ text "Reachability Analysis Result: Pass" ]
              , text
                  "Reachability analysis could not find any subcontract that is not reachable."
              ]
      CloseAnalysis closeAnalysisSubResult -> case closeAnalysisSubResult of
        AnalysisNotStarted ->
          explanation
            [ text ""
            ]
        AnalysisInProgress
          { numSubproblems: totalSteps
          , numSolvedSubproblems: doneSteps
          , counterExampleSubcontracts: foundcounterExampleSubcontracts
          } ->
          explanation
            ( [ text
                  ( "Close analysis in progress, " <> show doneSteps
                      <> " subcontracts out of "
                      <> show totalSteps
                      <> " analysed..."
                  )
              ]
                <>
                  if null foundcounterExampleSubcontracts then
                    [ br_, text "No refunds on Close found so far." ]
                  else
                    ( [ br_
                      , text "Found the following refunds on Close so far:"
                      ]
                        <>
                          [ ul [ classes [ ClassName "indented-enum-initial" ] ]
                              do
                                contractPath <- toUnfoldable
                                  foundcounterExampleSubcontracts
                                pure (li_ [ text (show contractPath) ])
                          ]
                    )
            )
        AnalysisFailure err ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Error during Close refund analysis" ]
            , text "Close refund analysis failed for the following reason:"
            , ul [ classes [ ClassName "indented-enum-initial" ] ]
                [ li_
                    [ b_ [ spanText err ]
                    ]
                ]
            ]
        AnalysisFoundCounterExamples { counterExampleSubcontracts } ->
          explanation
            ( [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                  [ text
                      "Close Refund Analysis Result: Some of the Close constructs may refund assets"
                  ]
              , text
                  "The following Close constructs may implicitly refund money:"
              ]
                <>
                  [ ul [ classes [ ClassName "indented-enum-initial" ] ] do
                      contractPath <- toUnfoldable
                        (toList counterExampleSubcontracts)
                      pure (li_ [ text (show contractPath) ])
                  ]
                <>
                  [ text
                      "This does not necessarily mean there is anything wrong with the contract."
                  ]
            )
        AnalysisFinishedAndPassed ->
          explanation
            [ h3 [ classes [ ClassName "analysis-result-title" ] ]
                [ text "Close Refund Analysis Result: No implicit refunds" ]
            , text
                "None of the Close constructs refunds any money, all refunds are explicit."
            ]

displayTransactionList
  :: forall p action. Array TransactionInput -> HTML p action
displayTransactionList transactionList =
  ol [ classes [ ClassName "indented-enum" ] ]
    ( do
        ( TransactionInput
            { interval: TimeInterval from to
            , inputs: inputList
            }
        ) <-
          transactionList
        pure
          ( li_
              [ span_
                  [ b_ [ text "Transaction" ]
                  , text " with time interval "
                  , b_ [ text $ (show from <> " to " <> show to) ]
                  , if List.null inputList then
                      text " and no inputs (empty transaction)."
                    else
                      text " and inputs:"
                  ]
              , if List.null inputList then
                  text ""
                else
                  displayInputList inputList
              ]
          )
    )

displayInputList :: forall p action. List Input -> HTML p action
displayInputList inputList =
  ol [ classes [ ClassName "indented-loweralpha-enum" ] ]
    ( do
        input <- (toUnfoldable inputList)
        pure (li_ (displayInput input))
    )

displayInput :: forall p i. Input -> Array (HTML p i)
displayInput (IDeposit owner party tok money) =
  [ b_ [ text "IDeposit" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " deposits "
  , b_ [ text $ show money ]
  , text " units of "
  , b_ [ text $ showPrettyToken tok ]
  , text " into account of "
  , b_ [ text (show owner) ]
  , text "."
  ]

displayInput (IChoice (ChoiceId choiceId party) chosenNum) =
  [ b_ [ text "IChoice" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " chooses number "
  , b_ [ text $ show chosenNum ]
  , text " for choice "
  , b_ [ text $ show choiceId ]
  , text "."
  ]

displayInput (INotify) =
  [ b_ [ text "INotify" ]
  , text " - The contract is notified that an observation became "
  , b_ [ text "True" ]
  ]
