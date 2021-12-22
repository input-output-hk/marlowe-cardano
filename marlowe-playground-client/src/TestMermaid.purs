module TestMermaid where

import Prologue
import Contrib.MermaidJS (Mermaid(..), SVGMarkup(..), render)
import Data.Either (hush)
import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Data.String (joinWith) as String
import Debug (traceM)
import Effect (Effect)
import Effect.Class.Console (log)
import Marlowe.ContractTests (filledEscrow)
import Marlowe.Extended (Contract) as EM
import Marlowe.Holes (Contract) as H
import Marlowe.Holes (Term(..))
import Marlowe.Mermaid (toMermaid)
import Marlowe.Parser (parseContract)
import Partial.Unsafe (unsafePartial)
import Web.DOM (Element)
import Web.DOM.Document (getElementsByTagName, toNonElementParentNode)
import Web.DOM.Element (setAttribute)
import Web.DOM.HTMLCollection (toArray) as HTMLCollection
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)

foreign import setInnerHTML :: Element -> String -> Effect Unit

setBodyHTML :: String -> Effect Unit
setBodyHTML innerHTML =
  window >>= document >>= toDocument >>> getElementsByTagName "body" >>= HTMLCollection.toArray
    >>= case _ of
        [ body ] -> setInnerHTML body innerHTML
        _ -> pure unit

main :: Effect Unit
main = do
  let
    cb (SVGMarkup svg) _ = do
      log svg
      setBodyHTML svg

    m =
      Mermaid
        $ String.joinWith "\n"
            [ "flowchart LR"
            , "  firstVertext[This is the text in the box]"
            ]

    m' = toMermaid filledEscrow
  void $ render "test" (Mermaid m') cb Nothing
  log m'
  log "TEST Mermaid"
