module Test.Web.Event.User where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
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
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
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
import Test.Web.Monad (class MonadTest)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)
import Web.HTML (HTMLElement)

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

instance MonadEffect m => MonadUser (UserM m) where
  api = UserM $ asks userEventApi
  setup options (UserM r) = UserM do
    user <- ask
    user' <- liftEffect $ runEffectFn1 (userEventSetup user) options
    local (const user') r

foreign import data UserEvent :: Type

foreign import _setup :: EffectFn1 (Undefinable UserOptions) UserEvent

userEventApi :: UserEvent -> UserApi
userEventApi = unsafeCoerce

userEventSetup :: UserEvent -> EffectFn1 UserOptions UserEvent
userEventSetup = _.setup <<< unsafeCoerce

runUserM :: forall m a. MonadEffect m => Maybe UserOptions -> UserM m a -> m a
runUserM options (UserM (ReaderT f)) = do
  user <- liftEffect $ runEffectFn1 _setup (toUndefinable options)
  f user

withUserApi
  :: forall m a
   . MonadAff m
  => MonadUser m
  => (UserApi -> Effect (Promise a))
  -> m a
withUserApi f = liftAff <<< toAffE <<< f =<< api

click :: forall m. MonadAff m => MonadUser m => Element -> m Unit
click element = withUserApi \api -> runEffectFn1 api.click element

dblClick :: forall m. MonadAff m => MonadUser m => Element -> m Unit
dblClick element = withUserApi \api -> runEffectFn1 api.dblClick element

tripleClick :: forall m. MonadAff m => MonadUser m => Element -> m Unit
tripleClick element = withUserApi \api -> runEffectFn1 api.tripleClick element

hover :: forall m. MonadAff m => MonadUser m => Element -> m Unit
hover element = withUserApi \api -> runEffectFn1 api.hover element

unhover :: forall m. MonadAff m => MonadUser m => Element -> m Unit
unhover element = withUserApi \api -> runEffectFn1 api.unhover element

data ShiftState = ShiftPressed | ShiftNotPressed

tab :: forall m. MonadAff m => MonadUser m => ShiftState -> m Unit
tab ShiftPressed = withUserApi \api -> runEffectFn1 api.tab { shift: true }
tab ShiftNotPressed = withUserApi \api -> runEffectFn1 api.tab { shift: false }

keyboard :: forall m. MonadAff m => MonadUser m => String -> m Unit
keyboard text = withUserApi \api -> runEffectFn1 api.keyboard text

copy :: forall m. MonadAff m => MonadUser m => m Unit
copy = withUserApi \api -> api.copy

cut :: forall m. MonadAff m => MonadUser m => m Unit
cut = withUserApi \api -> api.cut

paste
  :: forall clipboard m
   . IsClipboardData clipboard
  => MonadAff m
  => MonadUser m
  => clipboard
  -> m Unit
paste clipboard =
  withUserApi \api -> runEffectFn1 api.paste $ toClipboardData clipboard

pointer
  :: forall pointer m
   . IsPointerInput pointer
  => MonadAff m
  => MonadUser m
  => pointer
  -> m Unit
pointer p = withUserApi \api -> runEffectFn1 api.pointer $ toPointerInput p

clear :: forall m. MonadAff m => MonadUser m => Element -> m Unit
clear element = withUserApi \api -> runEffectFn1 api.clear element

deselectOptions
  :: forall options m
   . IsSelectOptions options
  => MonadAff m
  => MonadUser m
  => Element
  -> options
  -> m Unit
deselectOptions element options = withUserApi \api ->
  runEffectFn2 api.selectOptions element $ toSelectOptions options

selectOptions
  :: forall options m
   . IsSelectOptions options
  => MonadAff m
  => MonadUser m
  => Element
  -> options
  -> m Unit
selectOptions element options = withUserApi \api ->
  runEffectFn2 api.selectOptions element $ toSelectOptions options

type_
  :: forall m
   . MonadAff m
  => MonadUser m
  => Element
  -> String
  -> Maybe TypeOptions
  -> m Unit
type_ element text options =
  withUserApi \api -> runEffectFn3 api.type element text $ toUndefinable options

upload
  :: forall files m
   . IsFileOrFiles files
  => MonadAff m
  => MonadUser m
  => HTMLElement
  -> files
  -> m Unit
upload element files =
  withUserApi \api -> runEffectFn2 api.upload element $ toFileOrFiles files
