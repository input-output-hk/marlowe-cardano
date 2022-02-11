module Test.Web.DOM.Debug
  ( logTestingPlaygroundURL
  , debugElement
  , debugElements
  ) where

import Prelude

import Debug (class DebugWarning)
import Effect.Aff.Compat (EffectFn1)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1)
import Test.Web.Monad (class MonadTest, getContainer)
import Web.DOM (Element)

foreign import _logTestingPlaygroundURL :: EffectFn1 Element Unit
foreign import _debugElement :: EffectFn1 Element Unit
foreign import _debugElements :: EffectFn1 (Array Element) Unit

logTestingPlaygroundURL :: forall m. DebugWarning => MonadTest m => m Unit
logTestingPlaygroundURL =
  liftEffect <<< runEffectFn1 _logTestingPlaygroundURL =<< getContainer

debugElement :: forall m. DebugWarning => MonadTest m => Element -> m Unit
debugElement = liftEffect <<< runEffectFn1 _debugElement

debugElements
  :: forall m. DebugWarning => MonadTest m => Array Element -> m Unit
debugElements = liftEffect <<< runEffectFn1 _debugElements
