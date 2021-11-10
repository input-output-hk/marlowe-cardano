module Component.Blockly.State (blocklyComponent) where

import Prologue hiding (div)
import Blockly.Dom (explainError, getDom)
import Blockly.Events (fromEvent, newParentId, oldParentId, newElementId)
import Blockly.Internal
  ( BlockDefinition
  , ElementId(..)
  , centerOnBlock
  , getBlockById
  , getBlockType
  , newBlock
  , select
  , updateToolbox
  , addChangeListener
  , removeChangeListener
  )
import Blockly.Internal as Blockly
import Blockly.Toolbox (Toolbox)
import Blockly.Types
  ( BlocklyEvent
  , BlocklyState
  , Workspace
  , isDragStart
  , isDragStop
  )
import Blockly.Types as BT
import Component.Blockly.Types
  ( Action(..)
  , Message(..)
  , Query(..)
  , State
  , _blocklyEventSubscription
  , _blocklyReadyFired
  , _blocklyState
  , _errorMessage
  , blocklyRef
  , emptyState
  )
import Component.Blockly.View (render)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Either (either, note)
import Data.Foldable (foldl, oneOf)
import Data.Lens (Lens', assign, set, use, view)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Traversable (for, for_, traverse_)
import Effect (Effect)
import Effect.Aff (Aff, finally, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception.Unsafe (unsafeThrow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Halogen
  ( Component
  , HalogenM
  , SubscriptionId
  , getHTMLElementRef
  , liftEffect
  , mkComponent
  , modify_
  , raise
  )
import Halogen as H
import Halogen.ElementResize (elementResize)
import Halogen.Subscription as HS
import Marlowe.Blockly (buildBlocks)
import Marlowe.Holes (Term(..), Location(..))
import Marlowe.Linter (_location)
import Marlowe.Parser as Parser
import Text.Extra as Text
import Type.Proxy (Proxy(..))
import Web.DOM.ResizeObserver (ResizeObserverBoxOptions(..))
import Web.Event.EventTarget (EventListener, eventListener)
import Web.HTML.HTMLElement as HTMLElement

blocklyComponent
  :: forall m
   . MonadAff m
  => String
  -> Array BlockDefinition
  -> Toolbox
  -> Component Query Unit Message m
blocklyComponent rootBlockName blockDefinitions toolbox =
  mkComponent
    { initialState: const emptyState
    , render
    , eval:
        H.mkEval
          { handleQuery
          , handleAction
          , initialize: Just $ Inject rootBlockName blockDefinitions toolbox
          , finalize: Just Finalize
          , receive: Just <<< SetData
          }
    }

handleQuery
  :: forall slots m a
   . MonadAff m
  => Query a
  -> HalogenM State Action slots Message m (Maybe a)
handleQuery (SetCode code next) = do
  mState <- use _blocklyState
  for_ mState \blocklyState -> do
    let
      contract =
        either
          (const $ Hole blocklyState.rootBlockName Proxy NoLocation)
          identity
          $ Parser.parseContract (Text.stripParens code)
    -- Create the blocks temporarily disabling the blockly events until they settle
    -- FIXME: check why buildBlocks requires we pass newBlock
    runWithoutEventSubscription 100 BlocklyEvent $ buildBlocks newBlock
      blocklyState
      contract
  assign _errorMessage Nothing
  pure $ Just next

handleQuery (SetError err next) = do
  assign _errorMessage $ Just err
  pure $ Just next

handleQuery (GetWorkspace f) = do
  mState <- use _blocklyState
  for mState \bs -> do
    xml <- liftEffect $ Blockly.workspaceXML bs.blockly bs.workspace
    pure $ f xml

handleQuery (LoadWorkspace xml next) = do
  mState <- use _blocklyState
  for_ mState \{ blockly, workspace } ->
    liftEffect $ Blockly.loadWorkspace blockly workspace xml
  assign _errorMessage Nothing
  pure $ Just next

handleQuery (GetBlockRepresentation next) = do
  eBlock <-
    runExceptT do
      blocklyState <- ExceptT $ note "BlocklyState not set" <$> use
        _blocklyState
      withExceptT explainError (getDom blocklyState)
  case eBlock of
    Left e -> do
      assign _errorMessage $ Just $ unexpected e
      pure Nothing
    Right block -> do
      assign _errorMessage Nothing
      pure <<< Just <<< next $ block
  where
  unexpected s =
    "An unexpected error has occurred, please raise a support issue at https://github.com/input-output-hk/marlowe-cardano/issues/new: "
      <> s

handleQuery (SelectWarning warning next) = do
  let
    blockId = locationToBlockId $ view _location warning
  void
    $ runMaybeT do
        blocklyState <- MaybeT $ use _blocklyState
        block <- MaybeT $ liftEffect $ getBlockById blocklyState.workspace
          blockId
        MaybeT $ map pure
          $ liftEffect do
              select block
              centerOnBlock blocklyState.workspace blockId
  pure $ Just next

handleQuery (SetToolbox toolbox next) = do
  void
    $ runMaybeT do
        blocklyState <- MaybeT $ use _blocklyState
        MaybeT $ map pure $ liftEffect $ updateToolbox toolbox
          blocklyState.workspace
  pure $ Just next

-- We cannot guarantee at the type level that the only type of location we handle in this editor
-- is a BlockId location, so we throw a useful error if we ever get to this situation
locationToBlockId :: Location -> String
locationToBlockId (BlockId blockId) = blockId

locationToBlockId (Range _) = unsafeThrow
  "Unexpected Range location found in MarloweParser"

locationToBlockId NoLocation = unsafeThrow
  "Unexpected NoLocation found in MarloweParser"

handleAction
  :: forall m slots
   . MonadAff m
  => Action
  -> HalogenM State Action slots Message m Unit
handleAction (Inject rootBlockName blockDefinitions toolbox) = do
  mElement <- (pure <<< map HTMLElement.toElement) =<< getHTMLElementRef
    blocklyRef
  blocklyState <-
    liftEffect do
      state <- Blockly.createBlocklyInstance rootBlockName
        (ElementId "blocklyWorkspace")
        (ElementId "workspaceBlocks")
        toolbox
      Blockly.addBlockTypes state.blockly blockDefinitions
      Blockly.initializeWorkspace state
      pure state
  -- Subscribe to the resize events on the main section to resize blockly automatically.
  for_ mElement $ H.subscribe <<< elementResize ContentBox
    (const ResizeWorkspace)
  -- Subscribe to blockly events to see when the code has changed.
  eventSubscription <- H.subscribe $ blocklyEvents BlocklyEvent
    blocklyState.workspace
  modify_
    ( set _blocklyState (Just blocklyState)
        <<< set _blocklyEventSubscription (Just eventSubscription)
    )

handleAction (SetData _) = pure unit

handleAction (BlocklyEvent (BT.Select event)) = case newElementId event of
  Nothing -> raise $ BlockSelection Nothing
  Just blockId -> do
    void
      $ runMaybeT do
          blocklyState <- MaybeT $ use _blocklyState
          block <- MaybeT $ liftEffect $ getBlockById blocklyState.workspace
            blockId
          MaybeT $ map pure $ raise $ BlockSelection $ Just
            { blockId, blockType: getBlockType block }

handleAction (BlocklyEvent (BT.FinishLoading _)) = do
  alreadyFired <- use _blocklyReadyFired
  when (not alreadyFired) do
    raise BlocklyReady
    assign _blocklyReadyFired true

handleAction (BlocklyEvent event) = detectCodeChanges CodeChange event

handleAction ResizeWorkspace = do
  mState <- use _blocklyState
  for_ mState \{ blockly, workspace } ->
    liftEffect $ Blockly.resize blockly workspace

-- When blockly becames visible or unvisible, we call hideChaff to avoid a visual glitch
-- See PR https://github.com/input-output-hk/plutus/pull/2787
handleAction Finalize = do
  mState <- use _blocklyState
  for_ mState \{ blockly } ->
    liftEffect $ Blockly.hideChaff blockly

blocklyEvents
  :: forall action. (BlocklyEvent -> action) -> Workspace -> HS.Emitter action
blocklyEvents toAction workspace =
  HS.makeEmitter \push -> do
    listener <-
      eventListener \event ->
        let
          mEvent =
            -- Blockly can fire all of the following events https://developers.google.com/blockly/guides/configure/web/events
            -- but at the moment we only care for the following ones
            oneOf
              [ BT.Create <$> fromEvent event
              , BT.Move <$> fromEvent event
              , BT.Change <$> fromEvent event
              , BT.FinishLoading <$> fromEvent event
              , BT.UI <$> fromEvent event
              , BT.Select <$> fromEvent event
              ]
        in
          traverse_ (push <<< toAction) mEvent
    addChangeListener workspace listener
    pure $ removeChangeListener workspace listener

_eventsWhileDragging
  :: forall state
   . Lens' { eventsWhileDragging :: Maybe (List BlocklyEvent) | state }
       (Maybe (List BlocklyEvent))
_eventsWhileDragging = prop (Proxy :: _ "eventsWhileDragging")

-- | Using the blockly events, detect when the contract has changed and fire a halogen message.
-- |
-- | There are two events that signal us that the contract has changed.
-- |   * When the Change event is fired (which means that a property inside a block has changed)
-- |   * When the Move event is fired and the old and new parent are different (which means that
-- |     a block has been attached/detached)
-- |
-- | We also need to track the UI events to see if we are in the middle of a drag-n-drop.
-- | When we attach a block into a new parent there is no problem, but when we detach a block there
-- | is a small inconsistency.
-- | When we start draging a block outside from its parent, Blockly will fire the Move event and let us
-- | believe that the contract has changed. But if we ask Blockly to generate the contract at that point,
-- | the result will include the block we just detached. So we need to accumulate the events fired during
-- | drag-n-drop and analyze them once we drop it.
detectCodeChanges
  :: forall m state action slots message
   . MonadAff m
  => message
  -> BlocklyEvent
  -> HalogenM { eventsWhileDragging :: Maybe (List BlocklyEvent) | state }
       action
       slots
       message
       m
       Unit
detectCodeChanges codeChange event = do
  mDraggingEvents <- use _eventsWhileDragging
  case mDraggingEvents of
    Nothing ->
      -- If we are not inside a drag and drop, let's evaluate the event directly
      if (doesEventModifiesContract event) then
        H.raise codeChange
      else
        -- If the event starts a drag and drop, store it in the state
        when (isDragStart event) $ assign _eventsWhileDragging (Just Nil)
    Just eventsWhileDragging ->
      -- If we are inside a drag and drop, accumulate the events and analyze them all together
      if (not $ isDragStop event) then
        assign _eventsWhileDragging (Just (event : eventsWhileDragging))
      else do
        let
          hasChanged = foldl (\accu ev -> accu || doesEventModifiesContract ev)
            false
            eventsWhileDragging
        when hasChanged $ H.raise codeChange
        assign _eventsWhileDragging Nothing
  where
  doesEventModifiesContract :: BlocklyEvent -> Boolean
  doesEventModifiesContract = case _ of
    (BT.Change _) -> true
    (BT.Move ev) -> newParentId ev /= oldParentId ev
    _ -> false

-- | This function listens to blockly events and debounces the event listener so that the resulting
-- | Aff is completed after `time` milliseconds without events.
-- | NOTE: The part of this code that depends on blockly is rather minimal (addChangeListener workspace and
-- |       removeChangeListener workspace). We don't care about the contents of the event, just the fact that
-- |       it is fired, so we can debounce it.
-- |       If we need a debounce functionality later on, see if it makes sense to refactor this. I didn't do it
-- |       for the PR that introduced this code as it's not clear at the moment wether the debounce should be
-- |       tied to event listeners or if we can do it with plain Aff's
waitForEvents :: Workspace -> Int -> Aff Unit
waitForEvents workspace time = liftEffect (Ref.new Nothing) >>=
  waitForEventsAndCleanResources
  where
  -- Make sure that the event listener is removed no matter what
  waitForEventsAndCleanResources :: Ref (Maybe EventListener) -> Aff Unit
  waitForEventsAndCleanResources listenerRef =
    finally
      (liftEffect $ removeListener listenerRef)
      (waitForEvent listenerRef)

  waitForEvent :: Ref (Maybe EventListener) -> Aff Unit
  waitForEvent listenerRef = makeAff resolver
    where
    resolver cb = do
      -- The timerRef is a mutable reference to the timeoutId so we can
      -- cancel the "debounce" timer every time there is a new event.
      timerRef <- Ref.new Nothing
      let
        -- Helper fn that creates the timer that resolves the Aff if there
        -- is no event in `time` milliseconds
        resolveAfterTimeout = do
          timeoutId <- setTimeout time $ cb $ Right unit
          Ref.write (Just timeoutId) timerRef
      -- Create the initial timer
      resolveAfterTimeout
      -- Create and subscribe the event listener
      listener <-
        eventListener \_ -> do
          -- Clear the previous timer and and fire a new one
          mTimeoutId <- Ref.read timerRef
          for_ mTimeoutId clearTimeout
          resolveAfterTimeout
      Ref.write (Just listener) listenerRef
      addChangeListener workspace listener
      -- We can return a nonCanceler because the cleanup is done in the finally
      pure nonCanceler

  removeListener :: Ref (Maybe EventListener) -> Effect Unit
  removeListener listenerRef = do
    mListener <- Ref.read listenerRef
    for_ mListener \listener ->
      removeChangeListener workspace listener

-- Runs an effectful action temporarily disabling the blockly events, waiting
-- `time` milliseconds for those events to settle, and then re-subscribe.
runWithoutEventSubscription
  :: forall m state action message slots
   . MonadAff m
  => Int
  -> (BlocklyEvent -> action)
  -> Effect Unit
  -> HalogenM
       { blocklyState :: Maybe BlocklyState
       , blocklyEventSubscription :: Maybe SubscriptionId
       | state
       }
       action
       slots
       message
       m
       Unit
runWithoutEventSubscription time toAction doEffect = do
  let
    _blocklyEventSubscription = prop (Proxy :: _ "blocklyEventSubscription")

    _blocklyState = prop (Proxy :: _ "blocklyState")
  mSubscription <- use _blocklyEventSubscription
  mBlocklyState <- use _blocklyState
  for_ mBlocklyState \{ workspace } -> do
    -- Unsubscribe from blockly events
    for_ mSubscription H.unsubscribe
    -- Do the efectful computation that will trigger events we want to skip
    liftEffect doEffect
    liftAff $ waitForEvents workspace time
    -- Resubscribe to blockly events
    subscription <- H.subscribe (blocklyEvents toAction workspace)
    assign _blocklyEventSubscription (Just subscription)
