module Test.Web.Event.EventTarget where

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
import Web.Event.EventTarget (EventTarget)
import Web.File.FileReader (FileReader)
import Web.File.FileReader as FileReader
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
import Web.HTML.Window as Window
import Web.Socket.WebSocket (WebSocket)
import Web.Socket.WebSocket as WebSocket

expectFromEventTarget
  :: forall a m
   . MonadError Error m
  => DOMType a
  => IsEventTarget a
  => EventTarget
  -> m a
expectFromEventTarget =
  maybe
    ( throwError
        $ error
        $ "Unable to downcast Node to " <> typeName (Proxy :: _ a)
    )
    pure <<< fromEventTarget

class IsEventTarget a where
  toEventTarget :: a -> EventTarget
  fromEventTarget :: EventTarget -> Maybe a

instance IsEventTarget EventTarget where
  toEventTarget = identity
  fromEventTarget = Just

instance IsEventTarget DOM.CharacterData where
  toEventTarget = CharacterData.toEventTarget
  fromEventTarget = CharacterData.fromEventTarget

instance IsEventTarget DOM.Comment where
  toEventTarget = Comment.toEventTarget
  fromEventTarget = Comment.fromEventTarget

instance IsEventTarget DOM.DocumentFragment where
  toEventTarget = DocumentFragment.toEventTarget
  fromEventTarget = DocumentFragment.fromEventTarget

instance IsEventTarget DOM.Document where
  toEventTarget = Document.toEventTarget
  fromEventTarget = Document.fromEventTarget

instance IsEventTarget DOM.DocumentType where
  toEventTarget = DocumentType.toEventTarget
  fromEventTarget = DocumentType.fromEventTarget

instance IsEventTarget DOM.Element where
  toEventTarget = Element.toEventTarget
  fromEventTarget = Element.fromEventTarget

instance IsEventTarget DOM.ProcessingInstruction where
  toEventTarget = ProcessingInstruction.toEventTarget
  fromEventTarget = ProcessingInstruction.fromEventTarget

instance IsEventTarget DOM.Text where
  toEventTarget = Text.toEventTarget
  fromEventTarget = Text.fromEventTarget

instance IsEventTarget FileReader where
  toEventTarget = FileReader.toEventTarget
  fromEventTarget = FileReader.fromEventTarget

instance IsEventTarget HTML.HTMLAnchorElement where
  toEventTarget = HTMLAnchorElement.toEventTarget
  fromEventTarget = HTMLAnchorElement.fromEventTarget

instance IsEventTarget HTML.HTMLAreaElement where
  toEventTarget = HTMLAreaElement.toEventTarget
  fromEventTarget = HTMLAreaElement.fromEventTarget

instance IsEventTarget HTML.HTMLAudioElement where
  toEventTarget = HTMLAudioElement.toEventTarget
  fromEventTarget = HTMLAudioElement.fromEventTarget

instance IsEventTarget HTML.HTMLBaseElement where
  toEventTarget = HTMLBaseElement.toEventTarget
  fromEventTarget = HTMLBaseElement.fromEventTarget

instance IsEventTarget HTML.HTMLBodyElement where
  toEventTarget = HTMLBodyElement.toEventTarget
  fromEventTarget = HTMLBodyElement.fromEventTarget

instance IsEventTarget HTML.HTMLBRElement where
  toEventTarget = HTMLBRElement.toEventTarget
  fromEventTarget = HTMLBRElement.fromEventTarget

instance IsEventTarget HTML.HTMLButtonElement where
  toEventTarget = HTMLButtonElement.toEventTarget
  fromEventTarget = HTMLButtonElement.fromEventTarget

instance IsEventTarget HTML.HTMLCanvasElement where
  toEventTarget = HTMLCanvasElement.toEventTarget
  fromEventTarget = HTMLCanvasElement.fromEventTarget

instance IsEventTarget HTML.HTMLDataElement where
  toEventTarget = HTMLDataElement.toEventTarget
  fromEventTarget = HTMLDataElement.fromEventTarget

instance IsEventTarget HTML.HTMLDataListElement where
  toEventTarget = HTMLDataListElement.toEventTarget
  fromEventTarget = HTMLDataListElement.fromEventTarget

instance IsEventTarget HTML.HTMLDivElement where
  toEventTarget = HTMLDivElement.toEventTarget
  fromEventTarget = HTMLDivElement.fromEventTarget

instance IsEventTarget HTML.HTMLDListElement where
  toEventTarget = HTMLDListElement.toEventTarget
  fromEventTarget = HTMLDListElement.fromEventTarget

instance IsEventTarget HTML.HTMLDocument where
  toEventTarget = HTMLDocument.toEventTarget
  fromEventTarget = HTMLDocument.fromEventTarget

instance IsEventTarget HTML.HTMLElement where
  toEventTarget = HTMLElement.toEventTarget
  fromEventTarget = HTMLElement.fromEventTarget

instance IsEventTarget HTML.HTMLEmbedElement where
  toEventTarget = HTMLEmbedElement.toEventTarget
  fromEventTarget = HTMLEmbedElement.fromEventTarget

instance IsEventTarget HTML.HTMLFieldSetElement where
  toEventTarget = HTMLFieldSetElement.toEventTarget
  fromEventTarget = HTMLFieldSetElement.fromEventTarget

instance IsEventTarget HTML.HTMLFormElement where
  toEventTarget = HTMLFormElement.toEventTarget
  fromEventTarget = HTMLFormElement.fromEventTarget

instance IsEventTarget HTML.HTMLHeadElement where
  toEventTarget = HTMLHeadElement.toEventTarget
  fromEventTarget = HTMLHeadElement.fromEventTarget

instance IsEventTarget HTML.HTMLHeadingElement where
  toEventTarget = HTMLHeadingElement.toEventTarget
  fromEventTarget = HTMLHeadingElement.fromEventTarget

instance IsEventTarget HTML.HTMLHRElement where
  toEventTarget = HTMLHRElement.toEventTarget
  fromEventTarget = HTMLHRElement.fromEventTarget

instance IsEventTarget HTML.HTMLIFrameElement where
  toEventTarget = HTMLIFrameElement.toEventTarget
  fromEventTarget = HTMLIFrameElement.fromEventTarget

instance IsEventTarget HTML.HTMLImageElement where
  toEventTarget = HTMLImageElement.toEventTarget
  fromEventTarget = HTMLImageElement.fromEventTarget

instance IsEventTarget HTML.HTMLInputElement where
  toEventTarget = HTMLInputElement.toEventTarget
  fromEventTarget = HTMLInputElement.fromEventTarget

instance IsEventTarget HTML.HTMLKeygenElement where
  toEventTarget = HTMLKeygenElement.toEventTarget
  fromEventTarget = HTMLKeygenElement.fromEventTarget

instance IsEventTarget HTML.HTMLLabelElement where
  toEventTarget = HTMLLabelElement.toEventTarget
  fromEventTarget = HTMLLabelElement.fromEventTarget

instance IsEventTarget HTML.HTMLLegendElement where
  toEventTarget = HTMLLegendElement.toEventTarget
  fromEventTarget = HTMLLegendElement.fromEventTarget

instance IsEventTarget HTML.HTMLLIElement where
  toEventTarget = HTMLLIElement.toEventTarget
  fromEventTarget = HTMLLIElement.fromEventTarget

instance IsEventTarget HTML.HTMLLinkElement where
  toEventTarget = HTMLLinkElement.toEventTarget
  fromEventTarget = HTMLLinkElement.fromEventTarget

instance IsEventTarget HTML.HTMLMapElement where
  toEventTarget = HTMLMapElement.toEventTarget
  fromEventTarget = HTMLMapElement.fromEventTarget

instance IsEventTarget HTML.HTMLMediaElement where
  toEventTarget = HTMLMediaElement.toEventTarget
  fromEventTarget = HTMLMediaElement.fromEventTarget

instance IsEventTarget HTML.HTMLMetaElement where
  toEventTarget = HTMLMetaElement.toEventTarget
  fromEventTarget = HTMLMetaElement.fromEventTarget

instance IsEventTarget HTML.HTMLMeterElement where
  toEventTarget = HTMLMeterElement.toEventTarget
  fromEventTarget = HTMLMeterElement.fromEventTarget

instance IsEventTarget HTML.HTMLModElement where
  toEventTarget = HTMLModElement.toEventTarget
  fromEventTarget = HTMLModElement.fromEventTarget

instance IsEventTarget HTML.HTMLObjectElement where
  toEventTarget = HTMLObjectElement.toEventTarget
  fromEventTarget = HTMLObjectElement.fromEventTarget

instance IsEventTarget HTML.HTMLOListElement where
  toEventTarget = HTMLOListElement.toEventTarget
  fromEventTarget = HTMLOListElement.fromEventTarget

instance IsEventTarget HTML.HTMLOptGroupElement where
  toEventTarget = HTMLOptGroupElement.toEventTarget
  fromEventTarget = HTMLOptGroupElement.fromEventTarget

instance IsEventTarget HTML.HTMLOptionElement where
  toEventTarget = HTMLOptionElement.toEventTarget
  fromEventTarget = HTMLOptionElement.fromEventTarget

instance IsEventTarget HTML.HTMLOutputElement where
  toEventTarget = HTMLOutputElement.toEventTarget
  fromEventTarget = HTMLOutputElement.fromEventTarget

instance IsEventTarget HTML.HTMLParagraphElement where
  toEventTarget = HTMLParagraphElement.toEventTarget
  fromEventTarget = HTMLParagraphElement.fromEventTarget

instance IsEventTarget HTML.HTMLParamElement where
  toEventTarget = HTMLParamElement.toEventTarget
  fromEventTarget = HTMLParamElement.fromEventTarget

instance IsEventTarget HTML.HTMLPreElement where
  toEventTarget = HTMLPreElement.toEventTarget
  fromEventTarget = HTMLPreElement.fromEventTarget

instance IsEventTarget HTML.HTMLProgressElement where
  toEventTarget = HTMLProgressElement.toEventTarget
  fromEventTarget = HTMLProgressElement.fromEventTarget

instance IsEventTarget HTML.HTMLQuoteElement where
  toEventTarget = HTMLQuoteElement.toEventTarget
  fromEventTarget = HTMLQuoteElement.fromEventTarget

instance IsEventTarget HTML.HTMLScriptElement where
  toEventTarget = HTMLScriptElement.toEventTarget
  fromEventTarget = HTMLScriptElement.fromEventTarget

instance IsEventTarget HTML.HTMLSelectElement where
  toEventTarget = HTMLSelectElement.toEventTarget
  fromEventTarget = HTMLSelectElement.fromEventTarget

instance IsEventTarget HTML.HTMLSourceElement where
  toEventTarget = HTMLSourceElement.toEventTarget
  fromEventTarget = HTMLSourceElement.fromEventTarget

instance IsEventTarget HTML.HTMLSpanElement where
  toEventTarget = HTMLSpanElement.toEventTarget
  fromEventTarget = HTMLSpanElement.fromEventTarget

instance IsEventTarget HTML.HTMLStyleElement where
  toEventTarget = HTMLStyleElement.toEventTarget
  fromEventTarget = HTMLStyleElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableCaptionElement where
  toEventTarget = HTMLTableCaptionElement.toEventTarget
  fromEventTarget = HTMLTableCaptionElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableCellElement where
  toEventTarget = HTMLTableCellElement.toEventTarget
  fromEventTarget = HTMLTableCellElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableColElement where
  toEventTarget = HTMLTableColElement.toEventTarget
  fromEventTarget = HTMLTableColElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableDataCellElement where
  toEventTarget = HTMLTableDataCellElement.toEventTarget
  fromEventTarget = HTMLTableDataCellElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableElement where
  toEventTarget = HTMLTableElement.toEventTarget
  fromEventTarget = HTMLTableElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableHeaderCellElement where
  toEventTarget = HTMLTableHeaderCellElement.toEventTarget
  fromEventTarget = HTMLTableHeaderCellElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableRowElement where
  toEventTarget = HTMLTableRowElement.toEventTarget
  fromEventTarget = HTMLTableRowElement.fromEventTarget

instance IsEventTarget HTML.HTMLTableSectionElement where
  toEventTarget = HTMLTableSectionElement.toEventTarget
  fromEventTarget = HTMLTableSectionElement.fromEventTarget

instance IsEventTarget HTML.HTMLTemplateElement where
  toEventTarget = HTMLTemplateElement.toEventTarget
  fromEventTarget = HTMLTemplateElement.fromEventTarget

instance IsEventTarget HTML.HTMLTextAreaElement where
  toEventTarget = HTMLTextAreaElement.toEventTarget
  fromEventTarget = HTMLTextAreaElement.fromEventTarget

instance IsEventTarget HTML.HTMLTimeElement where
  toEventTarget = HTMLTimeElement.toEventTarget
  fromEventTarget = HTMLTimeElement.fromEventTarget

instance IsEventTarget HTML.HTMLTitleElement where
  toEventTarget = HTMLTitleElement.toEventTarget
  fromEventTarget = HTMLTitleElement.fromEventTarget

instance IsEventTarget HTML.HTMLTrackElement where
  toEventTarget = HTMLTrackElement.toEventTarget
  fromEventTarget = HTMLTrackElement.fromEventTarget

instance IsEventTarget HTML.HTMLUListElement where
  toEventTarget = HTMLUListElement.toEventTarget
  fromEventTarget = HTMLUListElement.fromEventTarget

instance IsEventTarget HTML.HTMLVideoElement where
  toEventTarget = HTMLVideoElement.toEventTarget
  fromEventTarget = HTMLVideoElement.fromEventTarget

instance IsEventTarget HTML.Window where
  toEventTarget = Window.toEventTarget
  fromEventTarget = Window.fromEventTarget

instance IsEventTarget WebSocket where
  toEventTarget = WebSocket.toEventTarget
  fromEventTarget = WebSocket.fromEventTarget
