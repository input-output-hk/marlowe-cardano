module Control.Concurrent.AVarMap
  ( AVarMap
  , empty
  , keys
  , kill
  , mask
  , member
  , modify
  , modify'
  , modifyM
  , modifyM'
  , modifyM_
  , modify_
  , new
  , put
  , read
  , status
  , take
  , traceStatus
  , tryPut
  , tryRead
  , tryTake
  ) where

import Prologue

import Control.Monad.Fork.Class (BracketCondition(..), bracket)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Rec.Class (untilJust)
import Control.Monad.Trans.Class (lift)
import Control.Parallel (parSequence, parTraverse_)
import Data.Align (align)
import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Alignable (AlignableMap(..))
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Set (Set)
import Data.These (These(..))
import Data.Traversable (traverse)
import Debug (class DebugWarning, traceM)
import Effect.Aff (Aff, Error, forkAff, joinFiber, supervise)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, askUnliftAff, unliftAff)

-- A mapping of keys to AVars with an interface similar to AVar.
newtype AVarMap k v = AVarMap (AVar (Map k (AVar v)))

-- | Creates a fresh AVarMap initial values.
new :: forall m k v. MonadAff m => Map k v -> m (AVarMap k v)
new = liftAff <<< (map AVarMap <<< AVar.new <=< traverse AVar.new)

-- | Creates a fresh AVarMap.
empty :: forall m k v. MonadAff m => m (AVarMap k v)
empty = liftAff $ AVarMap <$> AVar.new Map.empty

-- | Synchronously checks the status of the AVarMap
status
  :: forall m k v
   . Ord k
  => MonadAff m
  => AVarMap k v
  -> m (Map k (AVar.AVarStatus v))
status (AVarMap avarsA) = liftAff do
  avars <- AVar.read avarsA
  traverse AVar.status avars

-- | Print the status of the AVar map to the console.
traceStatus
  :: forall m k v
   . Show v
  => EncodeJson k
  => DebugWarning
  => Ord k
  => MonadAff m
  => AVarMap k v
  -> m Unit
traceStatus avarMap = do
  statuses <- status avarMap
  traceM $ encodeJson $ showStatus <$> statuses
  where
  showStatus = case _ of
    AVar.Filled a -> "(Filled " <> show a <> ")"
    AVar.Killed e -> "(Killed " <> show e <> ")"
    AVar.Empty -> "Empty"

-- | Take the AVar at the given key. This call will block if A. the
-- | avar for the key is empty or B. the key is not found in the map. If the
-- | key is not found, a new empty AVar will be added, and take will be called
-- | on it.
take :: forall m k v. Ord k => MonadAff m => k -> AVarMap k v -> m v
take k (AVarMap avarsA) = liftAff do
  avar <- avarsA # modifyAVarM' \avars -> do
    avar <- maybe AVar.empty pure $ Map.lookup k avars
    pure { result: avar, value: Map.insert k avar avars }
  supervise $ AVar.take avar

-- | Attempt to take the AVar at the given key. This call will not block, but
-- | return a `Nothing` instead. It will not add new keys to the map either.
tryTake :: forall m k v. Ord k => MonadAff m => k -> AVarMap k v -> m (Maybe v)
tryTake k (AVarMap avarsA) = liftAff $ runMaybeT do
  avars <- lift $ AVar.read avarsA
  avar <- hoistMaybe $ Map.lookup k avars
  MaybeT $ AVar.tryTake avar

-- | Make the AVars in an AVarMap match the values in the given map.
-- | This call doesn't block because the risk of deadlocks it too high. Keys
-- | not preset in the mask will be not be touched. Keys provided in
-- | the mask will fill the AVars at those locations with their given values if
-- | their values are `Just`, and empty them otherwise.
mask
  :: forall m k v
   . Ord k
  => MonadAff m
  => Map k (Maybe v)
  -> AVarMap k v
  -> m Unit
mask maskValues (AVarMap avarsA) = liftAff do
  fibers <- avarsA # modifyAVarM' \avars -> do
    fibersAndAVars <- parSequence $ unwrap $ align updateAVar
      (AlignableMap avars)
      (AlignableMap maskValues)
    pure
      { value: snd <$> fibersAndAVars
      , result: fst =<< Map.values fibersAndAVars
      }
  parTraverse_ joinFiber fibers
  where
  -- Key is in AVarMap, but not in the mask. Do nothing.
  updateAVar (This avar) = pure $ Tuple mempty avar
  -- Key is in mask, but not in AVarMap. Create a new filled AVar for it.
  updateAVar (That v) = do
    Tuple mempty <$> maybe AVar.empty AVar.new v
  -- Key is in mask and AVarMap. Empty it without blocking.
  updateAVar (Both avar Nothing) =
    Tuple mempty <$> (AVar.tryTake avar $> avar)
  -- Key is in mask and AVarMap. Make a best effort to fill it without locking.
  -- This won't deadlock, but it is potentially vulnerable to starvation if it
  -- keeps failing to fill the AVar before a competitor does. To mitigate this,
  -- and to unlock the avar map its self promptly, the attempt to fill the avar
  -- is forked to a new fiber, which is joined with after the avar map has been
  -- put back.
  updateAVar (Both avar (Just v)) = do
    fiber <- forkAff $ untilJust do
      succeeded <- AVar.tryPut v avar
      if succeeded then do
        pure $ Just unit
      else do
        void $ AVar.tryTake avar
        pure Nothing
    pure $ Tuple (pure fiber) avar

-- | Put a value into the AVar at the given key. This call will block if the
-- | value of the AVar at the key has been taken. If the key is not present in
-- | the map, a new filled AVar will be inserted.
put :: forall m k v. Ord k => MonadAff m => k -> v -> AVarMap k v -> m Unit
put k v (AVarMap avarsA) = liftAff do
  aff <- avarsA # modifyAVarM' \avars ->
    case Map.lookup k avars of
      Nothing -> do
        avar <- AVar.new v
        pure { result: pure unit, value: Map.insert k avar avars }
      Just avar -> do
        pure { result: AVar.put v avar, value: avars }
  supervise aff

-- | Try to put a value into the AVar at the given key. This call will not block
-- | if the value of the AVar at the key has been taken, it will return false
-- | instead. If the key is not present in the map, false will be returned and
-- | the map will not be modified.
tryPut
  :: forall m k v. Ord k => MonadAff m => k -> v -> AVarMap k v -> m Boolean
tryPut k v (AVarMap avarsA) = liftAff do
  avars <- AVar.read avarsA
  case Map.lookup k avars of
    Nothing -> pure false
    Just avar -> supervise $ AVar.tryPut v avar

-- | Read the AVar at the given key. This call will block if A. the
-- | avar for the key is empty or B. the key is not found in the map. If the
-- | key is not found, a new empty AVar will be added, and read will be called
-- | on it. This call will not leave the AVar empty, and multiple reads will
-- | resolve simultaneously.
read :: forall m k v. Ord k => MonadAff m => k -> AVarMap k v -> m v
read k (AVarMap avarsA) = liftAff do
  avar <- avarsA # modifyAVarM' \avars -> do
    avar <- maybe AVar.empty pure $ Map.lookup k avars
    pure { result: avar, value: Map.insert k avar avars }
  supervise $ AVar.read avar

-- | Attempt to read the AVar at the given key. This call will not block, but
-- | return a `Nothing` instead. It will not add new keys to the map either. It
-- | will not leave the AVar empty, and multiple reads will resolve at the same
-- | time.
tryRead :: forall m k v. Ord k => MonadAff m => k -> AVarMap k v -> m (Maybe v)
tryRead k (AVarMap avarsA) = liftAff $ runMaybeT do
  avars <- lift $ AVar.read avarsA
  avar <- hoistMaybe $ Map.lookup k avars
  MaybeT $ AVar.tryRead avar

-- | Atomically modify the AVar at the given key with an effectful computation.
-- | This call is blocking if the AVar is empty. `put` is guaranteed to be
-- | called before returning, and the AVar will not be left empty. The updated
-- | value is returned.
modifyM
  :: forall m k v
   . Ord k
  => MonadUnliftAff m
  => k
  -> (v -> m v)
  -> AVarMap k v
  -> m v
modifyM k f = modifyM' k $ f >>> map \v -> { result: v, value: v }

-- | Atomically modify the AVar at the given key with a pure computation.
-- | This call is blocking if the AVar is empty. `put` is guaranteed to be
-- | called before returning, and the AVar will not be left empty. The updated
-- | value is returned.
modify
  :: forall m k v. Ord k => MonadAff m => k -> (v -> v) -> AVarMap k v -> m v
modify k f = modify' k $ f >>> \v -> { result: v, value: v }

-- | A version of `modifyM` that allows one to atomically update an AVar and
-- | derive a result from the value in an effectful context. `put` is guaranteed
-- | to be called on the AVar, and the result will be returned.
modifyM'
  :: forall m k v a
   . Ord k
  => MonadUnliftAff m
  => k
  -> (v -> m { value :: v, result :: a })
  -> AVarMap k v
  -> m a
modifyM' k f avarMap = do
  u <- askUnliftAff
  { result } <- liftAff $ bracket
    (take k avarMap)
    (\bc -> flip (put k) avarMap <<< resolveFinalResourceWith _.value bc)
    (unliftAff u <<< f)
  pure result

-- | A version of `modifyM` that allows one to atomically update an AVar and
-- | derive a result from the value in a pure context. `put` is guaranteed
-- | to be called on the AVar, and the result will be returned.
modify'
  :: forall m k v a
   . Ord k
  => MonadAff m
  => k
  -> (v -> { value :: v, result :: a })
  -> AVarMap k v
  -> m a
modify' k f avarMap = do
  { result } <- liftAff $ bracket
    (take k avarMap)
    (\bc -> flip (put k) avarMap <<< resolveFinalResourceWith _.value bc)
    (pure <<< f)
  pure result

-- | Atomically modify the AVar at the given key with an effectful computation.
-- | This call is blocking if the AVar is empty. `put` is guaranteed to be
-- | called before returning, and the AVar will not be left empty.
modifyM_
  :: forall m k v
   . Ord k
  => MonadUnliftAff m
  => k
  -> (v -> m v)
  -> AVarMap k v
  -> m Unit
modifyM_ k f = void <<< modifyM k f

-- | Atomically modify the AVar at the given key with a pure computation.
-- | This call is blocking if the AVar is empty. `put` is guaranteed to be
-- | called before returning, and the AVar will not be left empty.
modify_
  :: forall m k v. Ord k => MonadAff m => k -> (v -> v) -> AVarMap k v -> m Unit
modify_ k f = void <<< modify k f

-- | Check if a key is present in the AVar map.
member
  :: forall m k v. Ord k => MonadAff m => k -> AVarMap k v -> m Boolean
member k (AVarMap avarsA) = liftAff do
  avars <- AVar.read avarsA
  pure $ Map.member k avars

-- | Get the keys in the AVarMap.
keys
  :: forall m k v. Ord k => MonadAff m => AVarMap k v -> m (Set k)
keys (AVarMap avarsA) = liftAff do
  avars <- AVar.read avarsA
  pure $ Map.keys avars

-- | Kills the AVar at the given key with an exception. All pending and future
-- | actions will resolve immediately with the provided exception.
kill :: forall m k v. Ord k => MonadAff m => k -> Error -> AVarMap k v -> m Unit
kill k error (AVarMap avarsA) = liftAff do
  void $ avarsA # modifyAVarM' \avars -> do
    avar <- maybe AVar.empty pure $ Map.lookup k avars
    AVar.kill error avar
    pure { result: avar, value: Map.insert k avar avars }

modifyAVarM'
  :: forall a r
   . (a -> Aff { value :: a, result :: r })
  -> AVar a
  -> Aff r
modifyAVarM' f avar = do
  { result } <- bracket
    (AVar.take avar)
    (\bc -> flip AVar.put avar <<< resolveFinalResourceWith _.value bc)
    f
  pure result

resolveFinalResourceWith
  :: forall e a b. (b -> a) -> BracketCondition e b -> a -> a
resolveFinalResourceWith f (Completed b) _ = f b
resolveFinalResourceWith _ _ a = a
