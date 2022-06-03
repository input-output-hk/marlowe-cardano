module StaticAnalysis.BottomPanel
  ( analysisPane
  ) where

import Prologue hiding (div)

import Data.Array as Array
import Data.BigInt.Argonaut as BigInt
import Data.Lens (to, (^.))
import Data.List (List(..), null, toUnfoldable, (:))
import Data.List as List
import Data.List.NonEmpty (toList)
import Data.Time.Duration (Minutes)
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Classes (btn, spaceBottom, spaceRight, spaceTop, spanText)
import Halogen.Css (classNames)
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
  , span
  , span_
  , text
  , ul
  )
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (classes, enabled)
import Humanize (humanizeInterval, humanizeValue)
import Icons (Icon(..), icon)
import MainFrame.Types (ChildSlots)
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Semantics (ChoiceId(..), Input(..), TransactionInput(..))
import Marlowe.Symbolic.Types.Response (Result)
import Marlowe.Symbolic.Types.Response as R
import Marlowe.ViewPartials (displayWarningList)
import Network.RemoteData (RemoteData(..))
import Page.Simulation.View (TemplateParameterActionsGen, templateParameters)
import Servant.PureScript (printAjaxError)
import StaticAnalysis.Types
  ( AnalysisExecutionState(..)
  , AnalysisState
  , ContractPath
  , ContractPathStep(..)
  , MultiStageAnalysisData(..)
  , _analysisExecutionState
  , _analysisState
  , _templateContent
  , isCloseAnalysisLoading
  , isReachabilityLoading
  , isStaticLoading
  )
import Types (WarningAnalysisData, WarningAnalysisError(..))

analyzeButton
  :: forall p action. Boolean -> Boolean -> String -> action -> HTML p action
analyzeButton isLoading isEnabled name action =
  button
    [ onClick $ const action
    , enabled isEnabled
    , classes [ spaceTop, spaceBottom, spaceRight, btn ]
    ]
    [ text (if isLoading then "Analysing..." else name) ]

type AnalyseActionGen action =
  { warnings :: action
  , reachability :: action
  , refund :: action
  }

analysisPane
  :: forall action m state
   . MonadAff m
  => MetaData
  -> AnalyseActionGen action
  -> TemplateParameterActionsGen action
  -> { analysisState :: AnalysisState, tzOffset :: Minutes | state }
  -> ComponentHTML action ChildSlots m
analysisPane metadata analysisAction templateAction state =
  div
    [ classNames [ "flex" ]
    ]
    [ div [ classNames [ "flex-grow" ] ]
        [ analysisResultPane state
        , analysisButtons
        ]
    , div
        [ classNames
            [ "pl-4"
            , "border-0"
            , "border-l"
            , "border-solid"
            , "border-layout-accent"
            ]
        ]
        [ h3 [ classNames [ "analysis-result-title" ] ]
            [ text "Template parameters" ]

        , templateParameters
            "static-analisys"
            metadata
            templateContent
            templateAction
            tzOffset
        ]
    ]
  where
  analysisButtons = div [ classNames [ "pr-4" ] ]
    [ analyzeButton loadingWarningAnalysis analysisEnabled
        "Analyse for warnings"
        analysisAction.warnings
    , analyzeButton loadingReachability analysisEnabled
        "Analyse reachability"
        analysisAction.reachability
    , analyzeButton loadingCloseAnalysis analysisEnabled
        "Analyse for refunds on Close"
        analysisAction.refund
    ]

  templateContent =
    state ^. (_analysisState <<< _templateContent)
  tzOffset = state.tzOffset

  loadingWarningAnalysis = state ^. _analysisState <<< _analysisExecutionState
    <<< to isStaticLoading

  loadingReachability = state ^. _analysisState <<< _analysisExecutionState <<<
    to isReachabilityLoading

  loadingCloseAnalysis = state ^. _analysisState <<< _analysisExecutionState <<<
    to isCloseAnalysisLoading

  nothingLoading = not loadingWarningAnalysis && not loadingReachability && not
    loadingCloseAnalysis

  analysisEnabled = nothingLoading

analysisResultPane
  :: forall action m state
   . MonadAff m
  => { analysisState :: AnalysisState, tzOffset :: Minutes | state }
  -> ComponentHTML action ChildSlots m
analysisResultPane state =
  let
    result = state ^. (_analysisState <<< _analysisExecutionState)
    tzOffset = state.tzOffset
  in
    case result of
      NoneAsked -> h3 [ classNames [ "analysis-result-title" ] ]
        [ text "Static Analysis: None Asked" ]
      WarningAnalysis staticSubResult -> warningAnalysisResult tzOffset
        staticSubResult
      ReachabilityAnalysis reachabilitySubResult -> reachabilityAnalysisResult
        reachabilitySubResult
      CloseAnalysis closeAnalysisSubResult -> closeAnalysisResult
        closeAnalysisSubResult

warningAnalysisResult
  :: forall action m
   . MonadAff m
  => Minutes
  -> WarningAnalysisData Result
  -> ComponentHTML action ChildSlots m
warningAnalysisResult tzOffset staticSubResult = div
  [ classNames [ "padded-explanation" ] ]

  case staticSubResult of
    NotAsked -> [ text "" ]
    Success (R.Valid) ->
      [ h3 [ classes [ ClassName "analysis-result-title" ] ]
          [ text "Warning Analysis Result: Pass" ]
      , text
          "Static analysis could not find any execution that results in any warning."
      ]
    Success
      ( R.CounterExample
          { initialSlot, transactionList, transactionWarning }
      ) ->
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
              [ spanText "Offending sequence: "
              , displayTransactionList tzOffset transactionList
              ]
          ]
      ]
    Success (R.Error str) ->
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
    Loading -> [ text "" ]

reachabilityAnalysisResult
  :: forall action m
   . MonadAff m
  => MultiStageAnalysisData
  -> ComponentHTML action ChildSlots m
reachabilityAnalysisResult reachabilitySubResult = div
  [ classNames [ "padded-explanation" ] ]
  case reachabilitySubResult of
    AnalysisNotStarted -> [ text "" ]
    AnalysisInProgress
      { numSubproblems: totalSteps
      , numSolvedSubproblems: doneSteps
      , counterExampleSubcontracts: foundcounterExampleSubcontracts
      } ->
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
                          pure
                            ( li_ $ displayContractPath
                                (text "Unreachable code")
                                contractPath
                            )
                    ]
              )
      )
    AnalysisFailure err ->
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
                pure
                  ( li_ $ displayContractPath (text "Unreachable code")
                      contractPath
                  )
            ]
      )
    AnalysisFinishedAndPassed ->
      [ h3 [ classes [ ClassName "analysis-result-title" ] ]
          [ text "Reachability Analysis Result: Pass" ]
      , text
          "Reachability analysis could not find any subcontract that is not reachable."
      ]

closeAnalysisResult
  :: forall action m
   . MonadAff m
  => MultiStageAnalysisData
  -> ComponentHTML action ChildSlots m
closeAnalysisResult closeAnalysisSubResult = div
  [ classNames [ "padded-explanation" ] ]
  case closeAnalysisSubResult of
    AnalysisNotStarted -> [ text "" ]
    AnalysisInProgress
      { numSubproblems: totalSteps
      , numSolvedSubproblems: doneSteps
      , counterExampleSubcontracts: foundcounterExampleSubcontracts
      } ->
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
                          pure
                            ( li_ $ displayContractPath (text "Close")
                                contractPath
                            )
                    ]
              )
      )
    AnalysisFailure err ->
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
                pure
                  (li_ $ displayContractPath (text "Close") contractPath)
            ]
          <>
            [ text
                "This does not necessarily mean there is anything wrong with the contract."
            ]
      )
    AnalysisFinishedAndPassed ->
      [ h3 [ classes [ ClassName "analysis-result-title" ] ]
          [ text "Close Refund Analysis Result: No implicit refunds" ]
      , text
          "None of the Close constructs refunds any money, all refunds are explicit."
      ]

displayTransactionList
  :: forall p action. Minutes -> Array TransactionInput -> HTML p action
displayTransactionList tzOffset transactionList =
  ol [ classes [ ClassName "indented-enum" ] ]
    ( do
        ( TransactionInput
            { interval
            , inputs: inputList
            }
        ) <-
          transactionList
        pure
          ( li_
              [ span_
                  [ span [ classNames [ "capitalize" ] ]
                      [ text $ humanizeInterval tzOffset interval ]
                  , text " a "
                  , b_ [ text "transaction" ]
                  , if List.null inputList then
                      text " with no inputs (empty transaction)."
                    else
                      text " with the following inputs:"
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
  [ b_ [ text "Deposit" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " deposits "
  , b_ [ text $ humanizeValue tok money ]
  , text " into account of "
  , b_ [ text (show owner) ]
  , text "."
  ]

displayInput (IChoice (ChoiceId choiceId party) chosenNum) =
  [ b_ [ text "Choice" ]
  , text " - Party "
  , b_ [ text $ show party ]
  , text " chooses number "
  , b_ [ text $ BigInt.toString chosenNum ]
  , text " for choice "
  , b_ [ text $ show choiceId ]
  , text "."
  ]

displayInput (INotify) =
  [ b_ [ text "Notify" ]
  , text " - The contract is notified that an observation became "
  , b_ [ text "True" ]
  ]

displayContractPath :: forall p i. HTML p i -> ContractPath -> Array (HTML p i)
displayContractPath root list =
  Array.intersperse
    (span [ classNames [ "text-darkgray", "mx-1" ] ] [ icon ArrowRight ]) $ go
    list
  where
  go Nil = [ root ]
  go (head : tail) = Array.cons (displayStep head) (go tail)

  whenCaseToStr = case _ of
    0 -> "1st"
    1 -> "2nd"
    2 -> "3rd"
    n -> show (n + 1) <> "th"

  boldSpanText txt = span [ classNames [ "bold" ] ] [ text txt ]

  displayStep = case _ of
    PayContPath -> boldSpanText "Pay"
    IfTruePath -> span_ [ boldSpanText "If", text " true" ]
    IfFalsePath -> span_ [ boldSpanText "If", text "false" ]
    WhenCasePath n -> span_
      [ boldSpanText "When ", text $ whenCaseToStr n <> " case" ]
    WhenTimeoutPath -> span_ [ boldSpanText "When ", text "timeout" ]
    LetPath -> boldSpanText "Let"
    AssertPath -> boldSpanText "Assert"
