module Contrib.MermaidJS where

import Prologue
import Data.Maybe (maybe)
import Data.Undefined.NoProblem (Opt, opt, undefined)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, mkEffectFn2, runEffectFn1, runEffectFn4)
import Web.HTML (HTMLElement)

type SvgId
  = String

newtype Mermaid
  = Mermaid String

newtype SVGMarkup
  = SVGMarkup String

type BindFunctions
  = HTMLElement -> Effect Unit

render :: String -> Mermaid -> (SVGMarkup -> BindFunctions -> Effect Unit) -> Maybe HTMLElement -> Effect String
render svgId mermaid cb container = runEffectFn4 renderImpl svgId mermaid (mkEffectFn2 cb') (maybe undefined opt container)
  where
  cb' svg bindFunctionsImpl = cb svg (runEffectFn1 bindFunctionsImpl)

foreign import renderImpl :: EffectFn4 String Mermaid (EffectFn2 SVGMarkup (EffectFn1 HTMLElement Unit) Unit) (Opt HTMLElement) String
