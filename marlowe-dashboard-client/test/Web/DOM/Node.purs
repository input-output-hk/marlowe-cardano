module Test.Web.DOM.Node where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Error, error)
import Test.Web.DOM.DomType (class DOMType, typeName)
import Type.Proxy (Proxy(..))
import Web.DOM as DOM
import Web.DOM.CharacterData as CharacterData
import Web.DOM.Comment as Comment
import Web.DOM.Document as Document
import Web.DOM.DocumentFragment as DocumentFragment
import Web.DOM.DocumentType as DocumentType
import Web.DOM.Element as Element
import Web.DOM.ProcessingInstruction as ProcessingInstruction
import Web.DOM.Text as Text
import Web.HTML as HTML
import Web.HTML.HTMLAnchorElement as HTMLAnchorElement
import Web.HTML.HTMLAreaElement as HTMLAreaElement
import Web.HTML.HTMLAudioElement as HTMLAudioElement
import Web.HTML.HTMLBRElement as HTMLBRElement
import Web.HTML.HTMLBaseElement as HTMLBaseElement
import Web.HTML.HTMLBodyElement as HTMLBodyElement
import Web.HTML.HTMLButtonElement as HTMLButtonElement
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.HTMLDListElement as HTMLDListElement
import Web.HTML.HTMLDataElement as HTMLDataElement
import Web.HTML.HTMLDataListElement as HTMLDataListElement
import Web.HTML.HTMLDivElement as HTMLDivElement
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLEmbedElement as HTMLEmbedElement
import Web.HTML.HTMLFieldSetElement as HTMLFieldSetElement
import Web.HTML.HTMLFormElement as HTMLFormElement
import Web.HTML.HTMLHRElement as HTMLHRElement
import Web.HTML.HTMLHeadElement as HTMLHeadElement
import Web.HTML.HTMLHeadingElement as HTMLHeadingElement
import Web.HTML.HTMLIFrameElement as HTMLIFrameElement
import Web.HTML.HTMLImageElement as HTMLImageElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLKeygenElement as HTMLKeygenElement
import Web.HTML.HTMLLIElement as HTMLLIElement
import Web.HTML.HTMLLabelElement as HTMLLabelElement
import Web.HTML.HTMLLegendElement as HTMLLegendElement
import Web.HTML.HTMLLinkElement as HTMLLinkElement
import Web.HTML.HTMLMapElement as HTMLMapElement
import Web.HTML.HTMLMediaElement as HTMLMediaElement
import Web.HTML.HTMLMetaElement as HTMLMetaElement
import Web.HTML.HTMLMeterElement as HTMLMeterElement
import Web.HTML.HTMLModElement as HTMLModElement
import Web.HTML.HTMLOListElement as HTMLOListElement
import Web.HTML.HTMLObjectElement as HTMLObjectElement
import Web.HTML.HTMLOptGroupElement as HTMLOptGroupElement
import Web.HTML.HTMLOptionElement as HTMLOptionElement
import Web.HTML.HTMLOutputElement as HTMLOutputElement
import Web.HTML.HTMLParagraphElement as HTMLParagraphElement
import Web.HTML.HTMLParamElement as HTMLParamElement
import Web.HTML.HTMLPreElement as HTMLPreElement
import Web.HTML.HTMLProgressElement as HTMLProgressElement
import Web.HTML.HTMLQuoteElement as HTMLQuoteElement
import Web.HTML.HTMLScriptElement as HTMLScriptElement
import Web.HTML.HTMLSelectElement as HTMLSelectElement
import Web.HTML.HTMLSourceElement as HTMLSourceElement
import Web.HTML.HTMLSpanElement as HTMLSpanElement
import Web.HTML.HTMLStyleElement as HTMLStyleElement
import Web.HTML.HTMLTableCaptionElement as HTMLTableCaptionElement
import Web.HTML.HTMLTableCellElement as HTMLTableCellElement
import Web.HTML.HTMLTableColElement as HTMLTableColElement
import Web.HTML.HTMLTableDataCellElement as HTMLTableDataCellElement
import Web.HTML.HTMLTableElement as HTMLTableElement
import Web.HTML.HTMLTableHeaderCellElement as HTMLTableHeaderCellElement
import Web.HTML.HTMLTableRowElement as HTMLTableRowElement
import Web.HTML.HTMLTableSectionElement as HTMLTableSectionElement
import Web.HTML.HTMLTemplateElement as HTMLTemplateElement
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement
import Web.HTML.HTMLTimeElement as HTMLTimeElement
import Web.HTML.HTMLTitleElement as HTMLTitleElement
import Web.HTML.HTMLTrackElement as HTMLTrackElement
import Web.HTML.HTMLUListElement as HTMLUListElement
import Web.HTML.HTMLVideoElement as HTMLVideoElement

expectFromNode
  :: forall a m. MonadError Error m => DOMType a => IsNode a => DOM.Node -> m a
expectFromNode =
  maybe
    ( throwError
        $ error
        $ "Unable to downcast Node to " <> typeName (Proxy :: _ a)
    )
    pure <<< fromNode

class IsNode a where
  toNode :: a -> DOM.Node
  fromNode :: DOM.Node -> Maybe a

instance IsNode DOM.Node where
  toNode = identity
  fromNode = Just

instance IsNode DOM.CharacterData where
  toNode = CharacterData.toNode
  fromNode = CharacterData.fromNode

instance IsNode DOM.Comment where
  toNode = Comment.toNode
  fromNode = Comment.fromNode

instance IsNode DOM.DocumentFragment where
  toNode = DocumentFragment.toNode
  fromNode = DocumentFragment.fromNode

instance IsNode DOM.Document where
  toNode = Document.toNode
  fromNode = Document.fromNode

instance IsNode DOM.DocumentType where
  toNode = DocumentType.toNode
  fromNode = DocumentType.fromNode

instance IsNode DOM.Element where
  toNode = Element.toNode
  fromNode = Element.fromNode

instance IsNode DOM.ProcessingInstruction where
  toNode = ProcessingInstruction.toNode
  fromNode = ProcessingInstruction.fromNode

instance IsNode DOM.Text where
  toNode = Text.toNode
  fromNode = Text.fromNode

instance IsNode HTML.HTMLAnchorElement where
  toNode = HTMLAnchorElement.toNode
  fromNode = HTMLAnchorElement.fromNode

instance IsNode HTML.HTMLAreaElement where
  toNode = HTMLAreaElement.toNode
  fromNode = HTMLAreaElement.fromNode

instance IsNode HTML.HTMLAudioElement where
  toNode = HTMLAudioElement.toNode
  fromNode = HTMLAudioElement.fromNode

instance IsNode HTML.HTMLBaseElement where
  toNode = HTMLBaseElement.toNode
  fromNode = HTMLBaseElement.fromNode

instance IsNode HTML.HTMLBodyElement where
  toNode = HTMLBodyElement.toNode
  fromNode = HTMLBodyElement.fromNode

instance IsNode HTML.HTMLBRElement where
  toNode = HTMLBRElement.toNode
  fromNode = HTMLBRElement.fromNode

instance IsNode HTML.HTMLButtonElement where
  toNode = HTMLButtonElement.toNode
  fromNode = HTMLButtonElement.fromNode

instance IsNode HTML.HTMLCanvasElement where
  toNode = HTMLCanvasElement.toNode
  fromNode = HTMLCanvasElement.fromNode

instance IsNode HTML.HTMLDataElement where
  toNode = HTMLDataElement.toNode
  fromNode = HTMLDataElement.fromNode

instance IsNode HTML.HTMLDataListElement where
  toNode = HTMLDataListElement.toNode
  fromNode = HTMLDataListElement.fromNode

instance IsNode HTML.HTMLDivElement where
  toNode = HTMLDivElement.toNode
  fromNode = HTMLDivElement.fromNode

instance IsNode HTML.HTMLDListElement where
  toNode = HTMLDListElement.toNode
  fromNode = HTMLDListElement.fromNode

instance IsNode HTML.HTMLDocument where
  toNode = HTMLDocument.toNode
  fromNode = HTMLDocument.fromNode

instance IsNode HTML.HTMLElement where
  toNode = HTMLElement.toNode
  fromNode = HTMLElement.fromNode

instance IsNode HTML.HTMLEmbedElement where
  toNode = HTMLEmbedElement.toNode
  fromNode = HTMLEmbedElement.fromNode

instance IsNode HTML.HTMLFieldSetElement where
  toNode = HTMLFieldSetElement.toNode
  fromNode = HTMLFieldSetElement.fromNode

instance IsNode HTML.HTMLFormElement where
  toNode = HTMLFormElement.toNode
  fromNode = HTMLFormElement.fromNode

instance IsNode HTML.HTMLHeadElement where
  toNode = HTMLHeadElement.toNode
  fromNode = HTMLHeadElement.fromNode

instance IsNode HTML.HTMLHeadingElement where
  toNode = HTMLHeadingElement.toNode
  fromNode = HTMLHeadingElement.fromNode

instance IsNode HTML.HTMLHRElement where
  toNode = HTMLHRElement.toNode
  fromNode = HTMLHRElement.fromNode

instance IsNode HTML.HTMLIFrameElement where
  toNode = HTMLIFrameElement.toNode
  fromNode = HTMLIFrameElement.fromNode

instance IsNode HTML.HTMLImageElement where
  toNode = HTMLImageElement.toNode
  fromNode = HTMLImageElement.fromNode

instance IsNode HTML.HTMLInputElement where
  toNode = HTMLInputElement.toNode
  fromNode = HTMLInputElement.fromNode

instance IsNode HTML.HTMLKeygenElement where
  toNode = HTMLKeygenElement.toNode
  fromNode = HTMLKeygenElement.fromNode

instance IsNode HTML.HTMLLabelElement where
  toNode = HTMLLabelElement.toNode
  fromNode = HTMLLabelElement.fromNode

instance IsNode HTML.HTMLLegendElement where
  toNode = HTMLLegendElement.toNode
  fromNode = HTMLLegendElement.fromNode

instance IsNode HTML.HTMLLIElement where
  toNode = HTMLLIElement.toNode
  fromNode = HTMLLIElement.fromNode

instance IsNode HTML.HTMLLinkElement where
  toNode = HTMLLinkElement.toNode
  fromNode = HTMLLinkElement.fromNode

instance IsNode HTML.HTMLMapElement where
  toNode = HTMLMapElement.toNode
  fromNode = HTMLMapElement.fromNode

instance IsNode HTML.HTMLMediaElement where
  toNode = HTMLMediaElement.toNode
  fromNode = HTMLMediaElement.fromNode

instance IsNode HTML.HTMLMetaElement where
  toNode = HTMLMetaElement.toNode
  fromNode = HTMLMetaElement.fromNode

instance IsNode HTML.HTMLMeterElement where
  toNode = HTMLMeterElement.toNode
  fromNode = HTMLMeterElement.fromNode

instance IsNode HTML.HTMLModElement where
  toNode = HTMLModElement.toNode
  fromNode = HTMLModElement.fromNode

instance IsNode HTML.HTMLObjectElement where
  toNode = HTMLObjectElement.toNode
  fromNode = HTMLObjectElement.fromNode

instance IsNode HTML.HTMLOListElement where
  toNode = HTMLOListElement.toNode
  fromNode = HTMLOListElement.fromNode

instance IsNode HTML.HTMLOptGroupElement where
  toNode = HTMLOptGroupElement.toNode
  fromNode = HTMLOptGroupElement.fromNode

instance IsNode HTML.HTMLOptionElement where
  toNode = HTMLOptionElement.toNode
  fromNode = HTMLOptionElement.fromNode

instance IsNode HTML.HTMLOutputElement where
  toNode = HTMLOutputElement.toNode
  fromNode = HTMLOutputElement.fromNode

instance IsNode HTML.HTMLParagraphElement where
  toNode = HTMLParagraphElement.toNode
  fromNode = HTMLParagraphElement.fromNode

instance IsNode HTML.HTMLParamElement where
  toNode = HTMLParamElement.toNode
  fromNode = HTMLParamElement.fromNode

instance IsNode HTML.HTMLPreElement where
  toNode = HTMLPreElement.toNode
  fromNode = HTMLPreElement.fromNode

instance IsNode HTML.HTMLProgressElement where
  toNode = HTMLProgressElement.toNode
  fromNode = HTMLProgressElement.fromNode

instance IsNode HTML.HTMLQuoteElement where
  toNode = HTMLQuoteElement.toNode
  fromNode = HTMLQuoteElement.fromNode

instance IsNode HTML.HTMLScriptElement where
  toNode = HTMLScriptElement.toNode
  fromNode = HTMLScriptElement.fromNode

instance IsNode HTML.HTMLSelectElement where
  toNode = HTMLSelectElement.toNode
  fromNode = HTMLSelectElement.fromNode

instance IsNode HTML.HTMLSourceElement where
  toNode = HTMLSourceElement.toNode
  fromNode = HTMLSourceElement.fromNode

instance IsNode HTML.HTMLSpanElement where
  toNode = HTMLSpanElement.toNode
  fromNode = HTMLSpanElement.fromNode

instance IsNode HTML.HTMLStyleElement where
  toNode = HTMLStyleElement.toNode
  fromNode = HTMLStyleElement.fromNode

instance IsNode HTML.HTMLTableCaptionElement where
  toNode = HTMLTableCaptionElement.toNode
  fromNode = HTMLTableCaptionElement.fromNode

instance IsNode HTML.HTMLTableCellElement where
  toNode = HTMLTableCellElement.toNode
  fromNode = HTMLTableCellElement.fromNode

instance IsNode HTML.HTMLTableColElement where
  toNode = HTMLTableColElement.toNode
  fromNode = HTMLTableColElement.fromNode

instance IsNode HTML.HTMLTableDataCellElement where
  toNode = HTMLTableDataCellElement.toNode
  fromNode = HTMLTableDataCellElement.fromNode

instance IsNode HTML.HTMLTableElement where
  toNode = HTMLTableElement.toNode
  fromNode = HTMLTableElement.fromNode

instance IsNode HTML.HTMLTableHeaderCellElement where
  toNode = HTMLTableHeaderCellElement.toNode
  fromNode = HTMLTableHeaderCellElement.fromNode

instance IsNode HTML.HTMLTableRowElement where
  toNode = HTMLTableRowElement.toNode
  fromNode = HTMLTableRowElement.fromNode

instance IsNode HTML.HTMLTableSectionElement where
  toNode = HTMLTableSectionElement.toNode
  fromNode = HTMLTableSectionElement.fromNode

instance IsNode HTML.HTMLTemplateElement where
  toNode = HTMLTemplateElement.toNode
  fromNode = HTMLTemplateElement.fromNode

instance IsNode HTML.HTMLTextAreaElement where
  toNode = HTMLTextAreaElement.toNode
  fromNode = HTMLTextAreaElement.fromNode

instance IsNode HTML.HTMLTimeElement where
  toNode = HTMLTimeElement.toNode
  fromNode = HTMLTimeElement.fromNode

instance IsNode HTML.HTMLTitleElement where
  toNode = HTMLTitleElement.toNode
  fromNode = HTMLTitleElement.fromNode

instance IsNode HTML.HTMLTrackElement where
  toNode = HTMLTrackElement.toNode
  fromNode = HTMLTrackElement.fromNode

instance IsNode HTML.HTMLUListElement where
  toNode = HTMLUListElement.toNode
  fromNode = HTMLUListElement.fromNode

instance IsNode HTML.HTMLVideoElement where
  toNode = HTMLVideoElement.toNode
  fromNode = HTMLVideoElement.fromNode
