module Test.Web.DOM.Element where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Error, error)
import Test.Web.DOM.DomType (class DOMType, typeName)
import Type.Proxy (Proxy(..))
import Web.DOM as DOM
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

expectFromElement
  :: forall a m
   . MonadError Error m
  => DOMType a
  => IsElement a
  => DOM.Element
  -> m a
expectFromElement =
  maybe
    ( throwError
        $ error
        $ "Unable to downcast Element to " <> typeName (Proxy :: _ a)
    )
    pure <<< fromElement

class IsElement a where
  toElement :: a -> DOM.Element
  fromElement :: DOM.Element -> Maybe a

instance IsElement DOM.Element where
  toElement = identity
  fromElement = Just

instance IsElement HTML.HTMLAnchorElement where
  toElement = HTMLAnchorElement.toElement
  fromElement = HTMLAnchorElement.fromElement

instance IsElement HTML.HTMLAreaElement where
  toElement = HTMLAreaElement.toElement
  fromElement = HTMLAreaElement.fromElement

instance IsElement HTML.HTMLAudioElement where
  toElement = HTMLAudioElement.toElement
  fromElement = HTMLAudioElement.fromElement

instance IsElement HTML.HTMLBaseElement where
  toElement = HTMLBaseElement.toElement
  fromElement = HTMLBaseElement.fromElement

instance IsElement HTML.HTMLBodyElement where
  toElement = HTMLBodyElement.toElement
  fromElement = HTMLBodyElement.fromElement

instance IsElement HTML.HTMLBRElement where
  toElement = HTMLBRElement.toElement
  fromElement = HTMLBRElement.fromElement

instance IsElement HTML.HTMLButtonElement where
  toElement = HTMLButtonElement.toElement
  fromElement = HTMLButtonElement.fromElement

instance IsElement HTML.HTMLCanvasElement where
  toElement = HTMLCanvasElement.toElement
  fromElement = HTMLCanvasElement.fromElement

instance IsElement HTML.HTMLDataElement where
  toElement = HTMLDataElement.toElement
  fromElement = HTMLDataElement.fromElement

instance IsElement HTML.HTMLDataListElement where
  toElement = HTMLDataListElement.toElement
  fromElement = HTMLDataListElement.fromElement

instance IsElement HTML.HTMLDivElement where
  toElement = HTMLDivElement.toElement
  fromElement = HTMLDivElement.fromElement

instance IsElement HTML.HTMLDListElement where
  toElement = HTMLDListElement.toElement
  fromElement = HTMLDListElement.fromElement

instance IsElement HTML.HTMLElement where
  toElement = HTMLElement.toElement
  fromElement = HTMLElement.fromElement

instance IsElement HTML.HTMLEmbedElement where
  toElement = HTMLEmbedElement.toElement
  fromElement = HTMLEmbedElement.fromElement

instance IsElement HTML.HTMLFieldSetElement where
  toElement = HTMLFieldSetElement.toElement
  fromElement = HTMLFieldSetElement.fromElement

instance IsElement HTML.HTMLFormElement where
  toElement = HTMLFormElement.toElement
  fromElement = HTMLFormElement.fromElement

instance IsElement HTML.HTMLHeadElement where
  toElement = HTMLHeadElement.toElement
  fromElement = HTMLHeadElement.fromElement

instance IsElement HTML.HTMLHeadingElement where
  toElement = HTMLHeadingElement.toElement
  fromElement = HTMLHeadingElement.fromElement

instance IsElement HTML.HTMLHRElement where
  toElement = HTMLHRElement.toElement
  fromElement = HTMLHRElement.fromElement

instance IsElement HTML.HTMLIFrameElement where
  toElement = HTMLIFrameElement.toElement
  fromElement = HTMLIFrameElement.fromElement

instance IsElement HTML.HTMLImageElement where
  toElement = HTMLImageElement.toElement
  fromElement = HTMLImageElement.fromElement

instance IsElement HTML.HTMLInputElement where
  toElement = HTMLInputElement.toElement
  fromElement = HTMLInputElement.fromElement

instance IsElement HTML.HTMLKeygenElement where
  toElement = HTMLKeygenElement.toElement
  fromElement = HTMLKeygenElement.fromElement

instance IsElement HTML.HTMLLabelElement where
  toElement = HTMLLabelElement.toElement
  fromElement = HTMLLabelElement.fromElement

instance IsElement HTML.HTMLLegendElement where
  toElement = HTMLLegendElement.toElement
  fromElement = HTMLLegendElement.fromElement

instance IsElement HTML.HTMLLIElement where
  toElement = HTMLLIElement.toElement
  fromElement = HTMLLIElement.fromElement

instance IsElement HTML.HTMLLinkElement where
  toElement = HTMLLinkElement.toElement
  fromElement = HTMLLinkElement.fromElement

instance IsElement HTML.HTMLMapElement where
  toElement = HTMLMapElement.toElement
  fromElement = HTMLMapElement.fromElement

instance IsElement HTML.HTMLMediaElement where
  toElement = HTMLMediaElement.toElement
  fromElement = HTMLMediaElement.fromElement

instance IsElement HTML.HTMLMetaElement where
  toElement = HTMLMetaElement.toElement
  fromElement = HTMLMetaElement.fromElement

instance IsElement HTML.HTMLMeterElement where
  toElement = HTMLMeterElement.toElement
  fromElement = HTMLMeterElement.fromElement

instance IsElement HTML.HTMLModElement where
  toElement = HTMLModElement.toElement
  fromElement = HTMLModElement.fromElement

instance IsElement HTML.HTMLObjectElement where
  toElement = HTMLObjectElement.toElement
  fromElement = HTMLObjectElement.fromElement

instance IsElement HTML.HTMLOListElement where
  toElement = HTMLOListElement.toElement
  fromElement = HTMLOListElement.fromElement

instance IsElement HTML.HTMLOptGroupElement where
  toElement = HTMLOptGroupElement.toElement
  fromElement = HTMLOptGroupElement.fromElement

instance IsElement HTML.HTMLOptionElement where
  toElement = HTMLOptionElement.toElement
  fromElement = HTMLOptionElement.fromElement

instance IsElement HTML.HTMLOutputElement where
  toElement = HTMLOutputElement.toElement
  fromElement = HTMLOutputElement.fromElement

instance IsElement HTML.HTMLParagraphElement where
  toElement = HTMLParagraphElement.toElement
  fromElement = HTMLParagraphElement.fromElement

instance IsElement HTML.HTMLParamElement where
  toElement = HTMLParamElement.toElement
  fromElement = HTMLParamElement.fromElement

instance IsElement HTML.HTMLPreElement where
  toElement = HTMLPreElement.toElement
  fromElement = HTMLPreElement.fromElement

instance IsElement HTML.HTMLProgressElement where
  toElement = HTMLProgressElement.toElement
  fromElement = HTMLProgressElement.fromElement

instance IsElement HTML.HTMLQuoteElement where
  toElement = HTMLQuoteElement.toElement
  fromElement = HTMLQuoteElement.fromElement

instance IsElement HTML.HTMLScriptElement where
  toElement = HTMLScriptElement.toElement
  fromElement = HTMLScriptElement.fromElement

instance IsElement HTML.HTMLSelectElement where
  toElement = HTMLSelectElement.toElement
  fromElement = HTMLSelectElement.fromElement

instance IsElement HTML.HTMLSourceElement where
  toElement = HTMLSourceElement.toElement
  fromElement = HTMLSourceElement.fromElement

instance IsElement HTML.HTMLSpanElement where
  toElement = HTMLSpanElement.toElement
  fromElement = HTMLSpanElement.fromElement

instance IsElement HTML.HTMLStyleElement where
  toElement = HTMLStyleElement.toElement
  fromElement = HTMLStyleElement.fromElement

instance IsElement HTML.HTMLTableCaptionElement where
  toElement = HTMLTableCaptionElement.toElement
  fromElement = HTMLTableCaptionElement.fromElement

instance IsElement HTML.HTMLTableCellElement where
  toElement = HTMLTableCellElement.toElement
  fromElement = HTMLTableCellElement.fromElement

instance IsElement HTML.HTMLTableColElement where
  toElement = HTMLTableColElement.toElement
  fromElement = HTMLTableColElement.fromElement

instance IsElement HTML.HTMLTableDataCellElement where
  toElement = HTMLTableDataCellElement.toElement
  fromElement = HTMLTableDataCellElement.fromElement

instance IsElement HTML.HTMLTableElement where
  toElement = HTMLTableElement.toElement
  fromElement = HTMLTableElement.fromElement

instance IsElement HTML.HTMLTableHeaderCellElement where
  toElement = HTMLTableHeaderCellElement.toElement
  fromElement = HTMLTableHeaderCellElement.fromElement

instance IsElement HTML.HTMLTableRowElement where
  toElement = HTMLTableRowElement.toElement
  fromElement = HTMLTableRowElement.fromElement

instance IsElement HTML.HTMLTableSectionElement where
  toElement = HTMLTableSectionElement.toElement
  fromElement = HTMLTableSectionElement.fromElement

instance IsElement HTML.HTMLTemplateElement where
  toElement = HTMLTemplateElement.toElement
  fromElement = HTMLTemplateElement.fromElement

instance IsElement HTML.HTMLTextAreaElement where
  toElement = HTMLTextAreaElement.toElement
  fromElement = HTMLTextAreaElement.fromElement

instance IsElement HTML.HTMLTimeElement where
  toElement = HTMLTimeElement.toElement
  fromElement = HTMLTimeElement.fromElement

instance IsElement HTML.HTMLTitleElement where
  toElement = HTMLTitleElement.toElement
  fromElement = HTMLTitleElement.fromElement

instance IsElement HTML.HTMLTrackElement where
  toElement = HTMLTrackElement.toElement
  fromElement = HTMLTrackElement.fromElement

instance IsElement HTML.HTMLUListElement where
  toElement = HTMLUListElement.toElement
  fromElement = HTMLUListElement.fromElement

instance IsElement HTML.HTMLVideoElement where
  toElement = HTMLVideoElement.toElement
  fromElement = HTMLVideoElement.fromElement
