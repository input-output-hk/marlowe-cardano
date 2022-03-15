module Test.Halogen where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Monad.Cont (ContT)
import Control.Monad.Cont.Class (class MonadCont)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT(..), asks, mapReaderT, runReaderT)
import Control.Monad.Reader.Class
  ( class MonadAsk
  , class MonadReader
  , ask
  , local
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (StateT)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Class (class MonadTell, class MonadWriter)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Array as Array
import Data.Coyoneda (liftCoyoneda, unCoyoneda)
import Data.Distributive (class Distributive)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, bracket, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen
  ( Component
  , Request
  , Tell
  , mkComponent
  , mkRequest
  , mkTell
  , unComponent
  )
import Halogen.Aff (awaitBody)
import Halogen.Query.HalogenQ as HQ
import Halogen.Subscription as HS
import Halogen.VDom.Driver (HalogenIO, runUI)
import Test.Spec.Assertions (shouldEqual)
import Test.Web (TestM, runTestM)
import Test.Web.Event.User (UserM, runUserM)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Web.DOM.Document as Document
import Web.DOM.Node as Node
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

class
  ( MonadUser m
  , MonadTest m
  ) <=
  MonadHalogenTest query input output m
  | m -> query input output where
  getDriver :: m (HalogenIO query output m)
  getMessages :: m (Ref (Array output))

  sendInput :: input -> m Unit

hoistDriver :: forall q o m n. m ~> n -> HalogenIO q o m -> HalogenIO q o n
hoistDriver phi { query, messages, dispose } =
  { query: \q -> phi (query q), messages, dispose: phi dispose }

instance MonadHalogenTest q i o m => MonadHalogenTest q i o (ReaderT r m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance
  ( Monoid w
  , MonadHalogenTest q i o m
  ) =>
  MonadHalogenTest q i o (WriterT w m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance MonadHalogenTest q i o m => MonadHalogenTest q i o (StateT s m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance MonadHalogenTest q i o m => MonadHalogenTest q i o (ContT r m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance MonadHalogenTest q i o m => MonadHalogenTest q i o (ExceptT e m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance MonadHalogenTest q i o m => MonadHalogenTest q i o (MaybeT m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance
  ( MonadError Error m
  , MonadHalogenTest q i o m
  ) =>
  MonadHalogenTest q i o (UserM m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance MonadHalogenTest q i o m => MonadHalogenTest q i o (TestM m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

instance
  ( Monoid w
  , MonadHalogenTest q i o m
  ) =>
  MonadHalogenTest q i o (RWST r w s m) where
  getDriver = map (hoistDriver lift) $ lift getDriver
  getMessages = lift getMessages
  sendInput = map lift sendInput

newtype HalogenTestEnv query input output = HalogenTestEnv
  { driver :: HalogenIO (Query query input) output Aff
  , messages :: Ref (Array output)
  }

newtype HalogenTestM query input output (m :: Type -> Type) a = HalogenTestM
  (ReaderT (HalogenTestEnv query input output) m a)

data Query query input a
  = SendInput input a
  | Query (query a)

derive newtype instance Functor m => Functor (HalogenTestM q i o m)
derive newtype instance Apply m => Apply (HalogenTestM q i o m)
derive newtype instance Applicative m => Applicative (HalogenTestM q i o m)
derive newtype instance Bind m => Bind (HalogenTestM q i o m)
derive newtype instance Alt m => Alt (HalogenTestM q i o m)
derive newtype instance Plus m => Plus (HalogenTestM q i o m)
derive newtype instance Alternative m => Alternative (HalogenTestM q i o m)
derive newtype instance Monad m => Monad (HalogenTestM q i o m)
derive newtype instance MonadZero m => MonadZero (HalogenTestM q i o m)
derive newtype instance
  ( Apply m
  , Semigroup a
  ) =>
  Semigroup (HalogenTestM q i o m a)

derive newtype instance
  ( Applicative m
  , Monoid a
  ) =>
  Monoid (HalogenTestM q i o m a)

derive newtype instance MonadPlus m => MonadPlus (HalogenTestM q i o m)
derive newtype instance MonadTrans (HalogenTestM q i o)
derive newtype instance MonadAff m => MonadAff (HalogenTestM q i o m)
derive newtype instance MonadEffect m => MonadEffect (HalogenTestM q i o m)
derive newtype instance MonadCont m => MonadCont (HalogenTestM q i o m)
derive newtype instance MonadThrow e m => MonadThrow e (HalogenTestM q i o m)
derive newtype instance MonadError e m => MonadError e (HalogenTestM q i o m)
instance MonadAsk r m => MonadAsk r (HalogenTestM q i o m) where
  ask = HalogenTestM (lift ask)

instance MonadReader r m => MonadReader r (HalogenTestM q i o m) where
  local f (HalogenTestM r) = HalogenTestM (mapReaderT (local f) r)

derive newtype instance MonadState s m => MonadState s (HalogenTestM q i o m)
derive newtype instance MonadTell w m => MonadTell w (HalogenTestM q i o m)
derive newtype instance MonadWriter w m => MonadWriter w (HalogenTestM q i o m)
derive newtype instance Distributive g => Distributive (HalogenTestM q i o g)
derive newtype instance MonadRec m => MonadRec (HalogenTestM q i o m)
derive newtype instance MonadTest m => MonadTest (HalogenTestM q i o m)
derive newtype instance MonadUser m => MonadUser (HalogenTestM q i o m)

envMessages
  :: forall q i o
   . HalogenTestEnv q i o
  -> Ref (Array o)
envMessages (HalogenTestEnv { messages }) = messages

envDriver
  :: forall q i o
   . HalogenTestEnv q i o
  -> HalogenIO (Query q i) o Aff
envDriver (HalogenTestEnv { driver }) = driver

envQuery
  :: forall q i o
   . HalogenTestEnv q i o
  -> forall a
   . (Query q i a)
  -> Aff (Maybe a)
envQuery env = (envDriver env).query

instance
  ( MonadUser m
  , MonadTest m
  ) =>
  MonadHalogenTest query
    input
    output
    (HalogenTestM query input output m) where
  getDriver = HalogenTestM
    ( ReaderT \(HalogenTestEnv { driver: { messages, dispose, query } }) ->
        pure $ (hoistDriver liftAff)
          { messages
          , query: \q -> (query (Query q))
          , dispose
          }
    )
  getMessages = HalogenTestM $ asks envMessages
  sendInput input = HalogenTestM do
    query <- asks envQuery
    void $ liftAff $ query $ mkTell $ SendInput input

runHalogenTestM
  :: forall query input output m a
   . HalogenTestM query input output m a
  -> HalogenTestEnv query input output
  -> m a
runHalogenTestM (HalogenTestM r) = runReaderT r

request
  :: forall query input output m a
   . Bind m
  => MonadHalogenTest query input output m
  => Request query a
  -> m (Maybe a)
request req = do
  { query } <- getDriver
  query (mkRequest req)

tell
  :: forall query input output m
   . Bind m
  => MonadHalogenTest query input output m
  => Tell query
  -> m Unit
tell req = do
  { query } <- getDriver
  void $ query (mkTell req)

expectMessages
  :: forall query input output m
   . Show output
  => Eq output
  => MonadThrow Error m
  => MonadHalogenTest query input output m
  => Array output
  -> m Unit
expectMessages expected = do
  messages <- getMessages
  actual <- liftEffect do
    ms <- Ref.read messages
    Ref.write [] messages
    pure ms
  actual `shouldEqual` expected

runUITest
  :: forall query input output
   . Component query input output Aff
  -> input
  -> ( forall m
        . MonadError Error m
       => MonadHalogenTest query input output m
       => m Unit
     )
  -> Aff Unit
runUITest component input test =
  bracket acquire release
    \(Tuple _ (Tuple container driver)) -> do
      messages <- liftEffect do
        ref <- Ref.new []
        void
          $ HS.subscribe driver.messages
          $ flip Ref.modify_ ref <<< flip Array.snoc
        pure ref
      runTestM container
        $ runUserM Nothing
        $ runHalogenTestM test
        $ HalogenTestEnv { messages, driver }
  where
  acquire = do
    document <-
      liftEffect $ map HTMLDocument.toDocument $ Window.document =<< window
    body <- awaitBody
    container <-
      maybe (throwError $ error "Failed to cast Element to HTMLElement") pure
        <<< HTMLElement.fromElement
        =<< liftEffect (Document.createElement "div" document)
    liftEffect $ Node.appendChild
      (HTMLElement.toNode container)
      (HTMLElement.toNode body)
    driver <- runUI (wrap component) input container
    pure $ Tuple body (Tuple container driver)
  release (Tuple body (Tuple container driver)) = do
    driver.dispose
    liftEffect $ Node.removeChild
      (HTMLElement.toNode container)
      (HTMLElement.toNode body)

  -- In order to send input via a query, we need to perform a little component
  -- surgery and adapt the eval of the provided component.
  wrap
    :: Component query input output Aff
    -> Component (Query query input) input output Aff
  wrap = unComponent \{ initialState, render, eval } -> mkComponent
    { initialState: \i -> initialState i
    , render: \s -> render s
    , eval: \q -> eval case q of
        HQ.Initialize a -> HQ.Initialize a
        HQ.Finalize a -> HQ.Finalize a
        HQ.Receive i a -> HQ.Receive i a
        HQ.Action action a -> HQ.Action action a
        HQ.Query req f -> unCoyoneda
          ( \g -> case _ of
              Query q' -> HQ.Query (map g (liftCoyoneda q')) f
              SendInput i b -> HQ.Receive i (g b)
          )
          req
    }
