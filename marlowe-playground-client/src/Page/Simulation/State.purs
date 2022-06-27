module Page.Simulation.State
  ( handleAction
  , editorGetValue
  , getCurrentContract
  , mkStateBase
  ) where

import Prologue hiding (div)

import Component.BottomPanel.State (handleAction) as BottomPanel
import Component.BottomPanel.Types (Action(..), State, initialState) as BottomPanel
import Control.Monad.Except (lift, runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Argonaut (encodeJson, stringify)
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt, fromString)
import Data.Decimal (fromNumber, truncated)
import Data.Decimal as Decimal
import Data.Either (fromRight, hush)
import Data.Foldable (for_)
import Data.Formatter.DateTime (formatDateTime)
import Data.Hashable (hash)
import Data.Lens (assign, modifying, use)
import Data.Lens.Extra (peruse)
import Data.List.NonEmpty (last)
import Data.List.NonEmpty as NEL
import Data.List.Types (NonEmptyList)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.MediaType.Common (applicationJSON)
import Data.NonEmptyList.Extra (tailIfNotEmpty)
import Data.RawJson (RawJson(..))
import Data.String (splitAt)
import Data.Time.Duration (Minutes)
import Data.Traversable (sequence)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Now (now, nowDateTime)
import Foreign.Generic (ForeignError, decode)
import Foreign.JSON (parseJSON)
import Halogen (HalogenM, query, tell)
import Halogen.Extra (mapSubmodule)
import Halogen.Monaco (Message(..), Query(..)) as Monaco
import Help (HelpContext(..))
import Language.Marlowe.Core.V1.Semantics (inBounds)
import Language.Marlowe.Core.V1.Semantics.Types
  ( ChoiceId(..)
  , Contract
  , Input(..)
  , Party(..)

  )
import Language.Marlowe.Extended.V1.Metadata (MetaData)
import MainFrame.Types (ChildSlots, _projectName, _simulatorEditorSlot)
import Marlowe (Api)
import Marlowe as Server
import Marlowe.Holes (Contract) as Term
import Marlowe.Holes (Location(..), Term, fromTerm, getLocation)
import Marlowe.Monaco as MM
import Marlowe.Parser (parseContract)
import Marlowe.Template (_timeContent, _valueContent, fillTemplate)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Page.Simulation.Lenses
  ( _bottomPanelState
  , _decorationIds
  , _helpContext
  , _showRightPanel
  )
import Page.Simulation.Types (Action(..), BottomPanelView(..), State, StateBase)
import Servant.PureScript (class MonadAjax, printAjaxError)
import SessionStorage as SessionStorage
import Simulator.Lenses
  ( _SimulationNotStarted
  , _currentContract
  , _currentMarloweState
  , _executionState
  , _initialTime
  , _marloweState
  , _templateContent
  , _termContract
  )
import Simulator.State
  ( advanceTime
  , applyInput
  , emptyMarloweState
  , initialMarloweState
  , startSimulation
  , updateChoice
  )
import Simulator.Types
  ( ActionInputId(..)
  , ExecutionState(..)
  , PartiesAction(..)
  )
import StaticData (simulatorBufferLocalStorageKey)
import Web.Blob.Download (HandleMethod(..), download)
import Web.DOM.Document as D
import Web.DOM.Element (setScrollTop)
import Web.DOM.Element as E
import Web.DOM.HTMLCollection as WC
import Web.File.Blob (fromString) as Blob
import Web.HTML as Web
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window as W

mkStateBase :: Minutes -> StateBase ()
mkStateBase tzOffset =
  { showRightPanel: true
  , marloweState: NEL.singleton emptyMarloweState
  , tzOffset
  , helpContext: MarloweHelp
  , bottomPanelState: BottomPanel.initialState CurrentStateView
  , decorationIds: []
  }

toBottomPanel
  :: forall m a
   . Functor m
  => HalogenM (BottomPanel.State BottomPanelView)
       (BottomPanel.Action BottomPanelView Action)
       ChildSlots
       Void
       m
       a
  -> HalogenM State Action ChildSlots Void m a
toBottomPanel = mapSubmodule _bottomPanelState BottomPanelAction

mkContract
  :: forall m
   . MonadAff m
  => MonadEffect m
  => MonadAjax Api m
  => HalogenM State Action ChildSlots Void m (Maybe (Term Term.Contract))
mkContract = runMaybeT do
  termContract <- MaybeT $ peruse
    ( _currentMarloweState
        <<< _executionState
        <<< _SimulationNotStarted
        <<< _termContract
    )
  templateContent <- MaybeT $ peruse
    ( _currentMarloweState <<< _executionState <<< _SimulationNotStarted
        <<< _templateContent
    )
  pure $ fillTemplate templateContent termContract

handleAction
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => MetaData
  -> Action
  -> HalogenM State Action ChildSlots Void m Unit
handleAction metadata (HandleEditorMessage Monaco.EditorReady) = do
  contents <- fromMaybe "" <$>
    (liftEffect $ SessionStorage.getItem simulatorBufferLocalStorageKey)
  handleAction metadata $ LoadContract contents
  editorSetTheme

handleAction _ (HandleEditorMessage (Monaco.TextChanged _)) = pure unit

handleAction metadata (SetInitialTime initialTime) = do
  assign
    ( _currentMarloweState <<< _executionState <<< _SimulationNotStarted <<<
        _initialTime
    )
    initialTime
  setOraclePrice metadata

handleAction metadata (SetValueTemplateParam key value) = do
  modifying
    ( _currentMarloweState <<< _executionState <<< _SimulationNotStarted
        <<< _templateContent
        <<< _valueContent
    )
    (Map.insert key value)
  setOraclePrice metadata

handleAction metadata (SetTimeTemplateParam key value) = do
  modifying
    ( _currentMarloweState <<< _executionState <<< _SimulationNotStarted
        <<< _templateContent
        <<< _timeContent
    )
    (Map.insert key value)
  setOraclePrice metadata

handleAction metadata StartSimulation = do
  {- The marloweState is a non empty list of an object that includes the ExecutionState (SimulationRunning | SimulationNotStarted)
  Inside the SimulationNotStarted we can find the information needed to start the simulation. By running
  this code inside of a maybeT, we make sure that the Head of the list has the state SimulationNotStarted -}
  mInitialTime <- peruse
    ( _currentMarloweState <<< _executionState <<< _SimulationNotStarted
        <<< _initialTime
    )
  mContract <- mkContract
  void $ sequence $ startSimulation <$> mInitialTime <*> mContract
  updateOracleAndContractEditor metadata

handleAction _ DownloadAsJson = mkContract >>= (_ >>= fromTerm) >>> case _ of
  Just (contract :: Contract) -> do
    dateTime <- liftEffect $ nowDateTime
    projectName <- use _projectName
    let
      contractJson = stringify $ encodeJson contract
      blob = Blob.fromString contractJson applicationJSON
      abs i = if i < 0 then -1 * i else i
      id = abs $ hash contractJson
      dateTime' = fromRight "WrongDateFormat" $ formatDateTime
        "YYYY-MM-DD-HH:mm:ss"
        dateTime
      fullName = projectName <> "-" <> dateTime' <> "-" <> show id <> ".json"
    liftEffect do
      download (FileDownload fullName) blob
  Nothing -> pure unit

handleAction metadata (MoveTime instant) = void $ runMaybeT do
  advanceTime instant
  lift $ updateOracleAndContractEditor metadata

handleAction metadata (AddInput input bounds) = do
  when validInput do
    applyInput input
    updateOracleAndContractEditor metadata
  where
  validInput = case input of
    (IChoice _ chosenNum) -> inBounds chosenNum bounds
    _ -> true

handleAction _ (SetChoice choiceId chosenNum) = updateChoice choiceId chosenNum

handleAction metadata ResetSimulator = do
  modifying _marloweState (NEL.singleton <<< last)
  updateOracleAndContractEditor metadata

handleAction metadata Undo = do
  modifying _marloweState tailIfNotEmpty
  updateOracleAndContractEditor metadata

handleAction metadata (LoadContract contents) = do
  liftEffect $ SessionStorage.setItem simulatorBufferLocalStorageKey contents
  let
    mTermContract = hush $ parseContract contents
  currentTime <- liftEffect now
  for_ mTermContract \termContract ->
    assign
      _marloweState
      ( NEL.singleton
          $ initialMarloweState currentTime termContract metadata
      )

  editorSetValue contents

handleAction metadata (BottomPanelAction (BottomPanel.PanelAction action)) =
  handleAction
    metadata
    action

handleAction _ (BottomPanelAction action) = do
  toBottomPanel (BottomPanel.handleAction action)

handleAction _ (ChangeHelpContext help) = do
  assign _helpContext help
  scrollHelpPanel

handleAction _ (ShowRightPanel val) = assign _showRightPanel val

handleAction _ EditSource = pure unit

stripPair :: String -> Boolean /\ String
stripPair pair = case splitAt 4 pair of
  { before, after }
    | before == "inv-" -> true /\ after
    | before == "dir-" -> false /\ after
  _ -> false /\ pair

setOraclePrice
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => MetaData
  -> HalogenM State Action ChildSlots Void m Unit
setOraclePrice metadata = do
  execState <- peruse (_currentMarloweState <<< _executionState)
  case execState of
    Just (SimulationRunning esr) -> do
      let
        (PartiesAction actions) = esr.possibleActions
      case Map.lookup (Role "kraken") actions of
        Just acts -> do
          case Array.head (Map.toUnfoldable acts) of
            Just (Tuple (ChoiceInputId choiceId@(ChoiceId pair _)) _) -> do
              let
                inverse /\ strippedPair = stripPair pair
              price <- getPrice inverse "kraken" strippedPair
              handleAction metadata (SetChoice choiceId price)
            _ -> pure unit
        Nothing -> pure unit
    _ -> pure unit

type Resp =
  { result :: { price :: Number }
  , allowance :: { remaining :: Number, upgrade :: String, cost :: Number }
  }

getPrice
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => Boolean
  -> String
  -> String
  -> HalogenM State Action ChildSlots Void m BigInt
getPrice inverse exchange pair = do
  result <- lift $ RemoteData.fromEither
    <$> Server.getApiOracleByExchangeByPair exchange pair
  calculatedPrice <-
    liftEffect case result of
      NotAsked -> pure "0"
      Loading -> pure "0"
      Failure e -> do
        log $ "Failure" <> printAjaxError e
        pure "0"
      Success (RawJson json) -> do
        let
          response :: Either (NonEmptyList ForeignError) Resp
          response =
            runExcept
              $ do
                  foreignJson <- parseJSON json
                  decode foreignJson
        case response of
          Right resp -> do
            let
              price = fromNumber resp.result.price

              adjustedPrice = (if inverse then one / price else price) *
                fromNumber 100000000.0
            log $ "Got price: " <> show resp.result.price
              <> ", remaining calls: "
              <> show resp.allowance.remaining
            pure $ Decimal.toString (truncated adjustedPrice)
          Left err -> do
            log $ "Left " <> show err
            pure "0"
  let
    price = fromMaybe zero (fromString calculatedPrice)
  pure price

getCurrentContract
  :: forall m. HalogenM State Action ChildSlots Void m (Maybe String)
getCurrentContract = editorGetValue

scrollHelpPanel
  :: forall m. MonadEffect m => HalogenM State Action ChildSlots Void m Unit
scrollHelpPanel =
  liftEffect do
    window <- Web.window
    document <- toDocument <$> W.document window
    mSidePanel <- WC.item 0 =<< D.getElementsByClassName "sidebar-composer"
      document
    mDocPanel <- WC.item 0 =<< D.getElementsByClassName "documentation-panel"
      document
    case mSidePanel, mDocPanel of
      Just sidePanel, Just docPanel -> do
        sidePanelHeight <- E.scrollHeight sidePanel
        docPanelHeight <- E.scrollHeight docPanel
        availableHeight <- E.clientHeight sidePanel
        let
          newScrollHeight =
            if sidePanelHeight < availableHeight then
              sidePanelHeight
            else
              sidePanelHeight - docPanelHeight - 120.0
        setScrollTop newScrollHeight sidePanel
      _, _ -> pure unit

editorSetTheme
  :: forall state action msg m. HalogenM state action ChildSlots msg m Unit
editorSetTheme = void $ query _simulatorEditorSlot unit
  (Monaco.SetTheme MM.daylightTheme.name unit)

editorSetValue
  :: forall state action msg m
   . String
  -> HalogenM state action ChildSlots msg m Unit
editorSetValue contents = void $ query _simulatorEditorSlot unit
  (Monaco.SetText contents unit)

editorGetValue
  :: forall state action msg m
   . HalogenM state action ChildSlots msg m (Maybe String)
editorGetValue = query _simulatorEditorSlot unit (Monaco.GetText identity)

updateOracleAndContractEditor
  :: forall m
   . MonadAff m
  => MonadAjax Api m
  => MetaData
  -> HalogenM State Action ChildSlots Void m Unit
updateOracleAndContractEditor metadata = do
  mContract <- peruse _currentContract
  -- Update the decorations around the current part of the running contract
  oldDecorationIds <- use _decorationIds
  case getLocation <$> mContract of
    Just (Range r) -> do
      let
        decorationOptions =
          { isWholeLine: false
          , className: "monaco-simulation-text-decoration"
          , linesDecorationsClassName: "monaco-simulation-line-decoration"
          }
      mNewDecorationIds <- query _simulatorEditorSlot unit $
        Monaco.SetDeltaDecorations oldDecorationIds
          [ { range: r, options: decorationOptions } ]
          identity
      for_ mNewDecorationIds (assign _decorationIds)
      void $ tell _simulatorEditorSlot unit $ Monaco.RevealRange r
    _ -> do
      void $ query _simulatorEditorSlot unit $ Monaco.SetDeltaDecorations
        oldDecorationIds
        []
        identity
      assign _decorationIds []
      void $ tell _simulatorEditorSlot unit $ Monaco.SetPosition
        { column: 1, lineNumber: 1 }
  setOraclePrice metadata
