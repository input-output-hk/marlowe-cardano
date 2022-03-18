module Control.Concurrent.AVarMap
  ( AVarMap
  , new
  , empty
  , status
  , member
  , modify
  , modify'
  , modifyM
  , modifyM'
  , modifyM_
  , modify_
  , put
  , read
  , take
  , tryPut
  , tryRead
  , tryTake
  , kill
  ) where

import Prologue

import Control.Monad.Fork.Class (BracketCondition(..), bracket)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Traversable (traverse)
import Effect.Aff (Aff, Error, supervise)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, askUnliftAff, unliftAff)

-- A mapping of keys to AVars with an interface similar to AVar.
newtype AVarMap k v = AVarMap (AVar (Map k (AVar v)))

-- | Creates a fresh AVarMap initial values.
new :: forall m k v. MonadAff m => Map k v -> Aff (AVarMap k v)
new = liftAff <<< map AVarMap <<< AVar.new <=< traverse AVar.new

-- | Creates a fresh AVarMap.
empty :: forall m k v. MonadAff m => Aff (AVarMap k v)
empty = liftAff $ AVarMap <$> AVar.new Map.empty

-- | Synchronously checks the status of a key in an AVarMap
status
  :: forall m k v
   . Ord k
  => MonadAff m
  => k
  -> AVarMap k v
  -> m (AVar.AVarStatus v)
status k (AVarMap avarsA) = liftAff do
  avars <- AVar.read avarsA
  maybe (pure AVar.Empty) AVar.status $ Map.lookup k avars

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
