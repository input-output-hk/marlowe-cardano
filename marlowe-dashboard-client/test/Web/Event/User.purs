module Test.Web.Event.User where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Bind (bindFlipped)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow, try)
import Control.Monad.Error.Extra (toMonadThrow)
import Control.Monad.Reader (ReaderT(..), asks, mapReaderT)
import Control.Monad.Reader.Class
  ( class MonadAsk
  , class MonadReader
  , ask
  , local
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Control.Promise (Promise, toAffE)
import Data.Distributive (class Distributive)
import Data.Maybe (Maybe)
import Data.Undefinable (Undefinable, toUndefinable)
import Effect (Effect)
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Test.Web.DOM.Element (class IsElement, toElement)
import Test.Web.Event.User.Api
  ( class IsClipboardData
  , class IsFileOrFiles
  , class IsPointerInput
  , class IsSelectOptions
  , TypeOptions
  , UserApi
  , toClipboardData
  , toFileOrFiles
  , toPointerInput
  , toSelectOptions
  )
import Test.Web.Event.User.Monad (class MonadUser, api)
import Test.Web.Event.User.Options (UserOptions)
import Test.Web.HTML.HTMLElement (class IsHTMLElement, toHTMLElement)
import Test.Web.Monad (class MonadTest)
import Unsafe.Coerce (unsafeCoerce)

newtype UserM (m :: Type -> Type) a = UserM (ReaderT UserEvent m a)

derive instance Functor m => Functor (UserM m)
derive newtype instance Apply m => Apply (UserM m)
derive newtype instance Applicative m => Applicative (UserM m)
derive newtype instance Bind m => Bind (UserM m)
derive newtype instance Alt m => Alt (UserM m)
derive newtype instance Plus m => Plus (UserM m)
derive newtype instance Alternative m => Alternative (UserM m)
derive newtype instance Monad m => Monad (UserM m)
derive newtype instance MonadZero m => MonadZero (UserM m)
derive newtype instance (Apply m, Semigroup a) => Semigroup (UserM m a)
derive newtype instance (Applicative m, Monoid a) => Monoid (UserM m a)
derive newtype instance MonadPlus m => MonadPlus (UserM m)
derive newtype instance MonadTrans (UserM)
derive newtype instance MonadAff m => MonadAff (UserM m)
derive newtype instance MonadEffect m => MonadEffect (UserM m)
derive newtype instance MonadCont m => MonadCont (UserM m)
derive newtype instance MonadThrow e m => MonadThrow e (UserM m)
derive newtype instance MonadError e m => MonadError e (UserM m)
instance MonadAsk r m => MonadAsk r (UserM m) where
  ask = UserM $ lift ask

instance MonadReader r m => MonadReader r (UserM m) where
  local f (UserM r) = UserM $ mapReaderT (local f) r

derive newtype instance MonadState s m => MonadState s (UserM m)
derive newtype instance MonadTell w m => MonadTell w (UserM m)
derive newtype instance MonadWriter w m => MonadWriter w (UserM m)
derive newtype instance Distributive g => Distributive (UserM g)
derive newtype instance MonadRec m => MonadRec (UserM m)
derive newtype instance MonadTest m => MonadTest (UserM m)

liftPromise
  :: forall a m. MonadAff m => MonadError Error m => Effect (Promise a) -> m a
liftPromise p = toMonadThrow =<< liftAff (try $ toAffE p)

liftForeignEffect
  :: forall a m. MonadEffect m => MonadError Error m => Effect a -> m a
liftForeignEffect e = toMonadThrow =<< liftEffect (try e)

instance (MonadError Error m, MonadEffect m) => MonadUser (UserM m) where
  api = UserM $ asks userEventApi
  setup options (UserM r) = UserM do
    user <- ask
    user' <- liftForeignEffect $ runEffectFn1 (userEventSetup user) options
    local (const user') r

foreign import data UserEvent :: Type

foreign import _setup :: EffectFn1 (Undefinable UserOptions) UserEvent

userEventApi :: UserEvent -> UserApi
userEventApi = unsafeCoerce

userEventSetup :: UserEvent -> EffectFn1 UserOptions UserEvent
userEventSetup = _.setup <<< unsafeCoerce

runUserM
  :: forall m a
   . MonadEffect m
  => MonadError Error m
  => Maybe UserOptions
  -> UserM m a
  -> m a
runUserM options (UserM (ReaderT f)) = do
  user <- liftForeignEffect $ runEffectFn1 _setup (toUndefinable options)
  f user

withUserApi
  :: forall m a
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => (UserApi -> Effect (Promise a))
  -> m a
withUserApi f = liftPromise <<< f =<< api

click
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> m Unit
click element = withUserApi \api -> runEffectFn1 api.click $ toElement element

clickM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> m Unit
clickM = bindFlipped click

dblClick
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> m Unit
dblClick element = withUserApi \api ->
  runEffectFn1 api.dblClick $ toElement element

dblClickM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> m Unit
dblClickM = bindFlipped dblClick

tripleClick
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> m Unit
tripleClick element = withUserApi \api ->
  runEffectFn1 api.tripleClick $ toElement element

tripleClickM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> m Unit
tripleClickM = bindFlipped tripleClick

hover
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> m Unit
hover element = withUserApi \api -> runEffectFn1 api.hover $ toElement element

hoverM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> m Unit
hoverM = bindFlipped hover

unhover
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> m Unit
unhover element = withUserApi \api ->
  runEffectFn1 api.unhover $ toElement element

unhoverM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> m Unit
unhoverM = bindFlipped unhover

data ShiftState = ShiftPressed | ShiftNotPressed

tab
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => ShiftState
  -> m Unit
tab ShiftPressed = withUserApi \api -> runEffectFn1 api.tab { shift: true }
tab ShiftNotPressed = withUserApi \api -> runEffectFn1 api.tab { shift: false }

keyboard
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => String
  -> m Unit
keyboard text = withUserApi \api -> runEffectFn1 api.keyboard text

copy :: forall m. MonadAff m => MonadUser m => MonadError Error m => m Unit
copy = withUserApi \api -> api.copy

cut :: forall m. MonadAff m => MonadUser m => MonadError Error m => m Unit
cut = withUserApi \api -> api.cut

paste
  :: forall clipboard m
   . IsClipboardData clipboard
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => clipboard
  -> m Unit
paste clipboard =
  withUserApi \api -> runEffectFn1 api.paste $ toClipboardData clipboard

pointer
  :: forall pointer m
   . IsPointerInput pointer
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => pointer
  -> m Unit
pointer p = withUserApi \api -> runEffectFn1 api.pointer $ toPointerInput p

clear
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> m Unit
clear element = withUserApi \api -> runEffectFn1 api.clear $ toElement element

clearM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> m Unit
clearM = bindFlipped clear

deselectOptions
  :: forall options element m
   . IsElement element
  => IsSelectOptions options
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> options
  -> m Unit
deselectOptions element options = withUserApi \api ->
  runEffectFn2 api.selectOptions (toElement element) $ toSelectOptions options

deselectOptionsM
  :: forall options element m
   . IsElement element
  => IsSelectOptions options
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> options
  -> m Unit
deselectOptionsM element options = flip deselectOptions options =<< element

selectOptions
  :: forall options element m
   . IsElement element
  => IsSelectOptions options
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> options
  -> m Unit
selectOptions element options = withUserApi \api ->
  runEffectFn2 api.selectOptions (toElement element) $ toSelectOptions options

selectOptionsM
  :: forall options element m
   . IsElement element
  => IsSelectOptions options
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> options
  -> m Unit
selectOptionsM element options = flip selectOptions options =<< element

type_
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> String
  -> Maybe TypeOptions
  -> m Unit
type_ element text options = withUserApi \api ->
  runEffectFn3 api.type (toElement element) text $ toUndefinable options

typeM
  :: forall element m
   . IsElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> String
  -> Maybe TypeOptions
  -> m Unit
typeM mElement text options = do
  element <- mElement
  type_ element text options

upload
  :: forall files element m
   . IsFileOrFiles files
  => IsHTMLElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => element
  -> files
  -> m Unit
upload element files = withUserApi \api ->
  runEffectFn2 api.upload (toHTMLElement element) $ toFileOrFiles files

uploadM
  :: forall files element m
   . IsFileOrFiles files
  => IsHTMLElement element
  => MonadAff m
  => MonadUser m
  => MonadError Error m
  => m element
  -> files
  -> m Unit
uploadM element files = flip upload files =<< element
