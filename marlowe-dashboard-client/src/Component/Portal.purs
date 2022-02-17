module Component.Portal where

import Prologue

import Data.Either (either)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Subscription as HS
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data ComponentBox (m :: Type -> Type)

data ComponentBox' query input output m = ComponentBox'
  (H.Component query input output m)
  input
  (HS.Listener output)

mkComponentBox
  :: forall query input output m
   . H.Component query input output m
  -> input
  -> HS.Listener output
  -> ComponentBox m
mkComponentBox cmp input = unsafeCoerce <<< ComponentBox' cmp
  input

unComponentBox
  :: forall m r
   . ( forall query input output
        . H.Component query input output m
       -> input
       -> HS.Listener output
       -> r
     )
  -> ComponentBox m
  -> r
unComponentBox f component =
  let
    ComponentBox' cmp input listener = unsafeCoerce component
  in
    f cmp input listener

data PortalQuery (a :: Type)

instance Functor PortalQuery where
  map f = map f

data PortalMsg

type PortalComponent m =
  H.Component PortalQuery (Maybe (ComponentBox m)) PortalMsg m

outPortal :: forall m. MonadEffect m => PortalComponent m
outPortal =
  H.mkComponent
    { initialState: identity
    , render: \component ->
        case component of
          Nothing -> HH.text ""
          Just m -> m # unComponentBox \cmp input listener ->
            HH.slot _modal unit cmp input $ Right <<< HS.notify listener
    , eval: H.mkEval H.defaultEval
        { receive = Just <<< Left
        , handleAction = either H.put H.liftEffect
        }
    }
  where
  _modal = Proxy :: _ "component"
