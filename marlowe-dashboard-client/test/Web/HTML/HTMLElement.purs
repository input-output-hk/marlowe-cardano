module Test.Web.HTML.HTMLElement where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Error, error)
import Test.Web.DOM.DomType (class DOMType, typeName)
import Type.Proxy (Proxy(..))
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
  => IsHTMLElement a
  => HTML.HTMLElement
  -> m a
expectFromElement =
  maybe
    ( throwError
        $ error
        $ "Unable to downcast Element to " <> typeName (Proxy :: _ a)
    )
    pure <<< fromHTMLElement

class IsHTMLElement a where
  toHTMLElement :: a -> HTML.HTMLElement
  fromHTMLElement :: HTML.HTMLElement -> Maybe a

instance IsHTMLElement HTML.HTMLElement where
  toHTMLElement = identity
  fromHTMLElement = Just

instance IsHTMLElement HTML.HTMLAnchorElement where
  toHTMLElement = HTMLAnchorElement.toHTMLElement
  fromHTMLElement = HTMLAnchorElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLAreaElement where
  toHTMLElement = HTMLAreaElement.toHTMLElement
  fromHTMLElement = HTMLAreaElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLAudioElement where
  toHTMLElement = HTMLAudioElement.toHTMLElement
  fromHTMLElement = HTMLAudioElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLBaseElement where
  toHTMLElement = HTMLBaseElement.toHTMLElement
  fromHTMLElement = HTMLBaseElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLBodyElement where
  toHTMLElement = HTMLBodyElement.toHTMLElement
  fromHTMLElement = HTMLBodyElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLBRElement where
  toHTMLElement = HTMLBRElement.toHTMLElement
  fromHTMLElement = HTMLBRElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLButtonElement where
  toHTMLElement = HTMLButtonElement.toHTMLElement
  fromHTMLElement = HTMLButtonElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLCanvasElement where
  toHTMLElement = HTMLCanvasElement.toHTMLElement
  fromHTMLElement = HTMLCanvasElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLDataElement where
  toHTMLElement = HTMLDataElement.toHTMLElement
  fromHTMLElement = HTMLDataElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLDataListElement where
  toHTMLElement = HTMLDataListElement.toHTMLElement
  fromHTMLElement = HTMLDataListElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLDivElement where
  toHTMLElement = HTMLDivElement.toHTMLElement
  fromHTMLElement = HTMLDivElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLDListElement where
  toHTMLElement = HTMLDListElement.toHTMLElement
  fromHTMLElement = HTMLDListElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLEmbedElement where
  toHTMLElement = HTMLEmbedElement.toHTMLElement
  fromHTMLElement = HTMLEmbedElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLFieldSetElement where
  toHTMLElement = HTMLFieldSetElement.toHTMLElement
  fromHTMLElement = HTMLFieldSetElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLFormElement where
  toHTMLElement = HTMLFormElement.toHTMLElement
  fromHTMLElement = HTMLFormElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLHeadElement where
  toHTMLElement = HTMLHeadElement.toHTMLElement
  fromHTMLElement = HTMLHeadElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLHeadingElement where
  toHTMLElement = HTMLHeadingElement.toHTMLElement
  fromHTMLElement = HTMLHeadingElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLHRElement where
  toHTMLElement = HTMLHRElement.toHTMLElement
  fromHTMLElement = HTMLHRElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLIFrameElement where
  toHTMLElement = HTMLIFrameElement.toHTMLElement
  fromHTMLElement = HTMLIFrameElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLImageElement where
  toHTMLElement = HTMLImageElement.toHTMLElement
  fromHTMLElement = HTMLImageElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLInputElement where
  toHTMLElement = HTMLInputElement.toHTMLElement
  fromHTMLElement = HTMLInputElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLKeygenElement where
  toHTMLElement = HTMLKeygenElement.toHTMLElement
  fromHTMLElement = HTMLKeygenElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLLabelElement where
  toHTMLElement = HTMLLabelElement.toHTMLElement
  fromHTMLElement = HTMLLabelElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLLegendElement where
  toHTMLElement = HTMLLegendElement.toHTMLElement
  fromHTMLElement = HTMLLegendElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLLIElement where
  toHTMLElement = HTMLLIElement.toHTMLElement
  fromHTMLElement = HTMLLIElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLLinkElement where
  toHTMLElement = HTMLLinkElement.toHTMLElement
  fromHTMLElement = HTMLLinkElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLMapElement where
  toHTMLElement = HTMLMapElement.toHTMLElement
  fromHTMLElement = HTMLMapElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLMediaElement where
  toHTMLElement = HTMLMediaElement.toHTMLElement
  fromHTMLElement = HTMLMediaElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLMetaElement where
  toHTMLElement = HTMLMetaElement.toHTMLElement
  fromHTMLElement = HTMLMetaElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLMeterElement where
  toHTMLElement = HTMLMeterElement.toHTMLElement
  fromHTMLElement = HTMLMeterElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLModElement where
  toHTMLElement = HTMLModElement.toHTMLElement
  fromHTMLElement = HTMLModElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLObjectElement where
  toHTMLElement = HTMLObjectElement.toHTMLElement
  fromHTMLElement = HTMLObjectElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLOListElement where
  toHTMLElement = HTMLOListElement.toHTMLElement
  fromHTMLElement = HTMLOListElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLOptGroupElement where
  toHTMLElement = HTMLOptGroupElement.toHTMLElement
  fromHTMLElement = HTMLOptGroupElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLOptionElement where
  toHTMLElement = HTMLOptionElement.toHTMLElement
  fromHTMLElement = HTMLOptionElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLOutputElement where
  toHTMLElement = HTMLOutputElement.toHTMLElement
  fromHTMLElement = HTMLOutputElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLParagraphElement where
  toHTMLElement = HTMLParagraphElement.toHTMLElement
  fromHTMLElement = HTMLParagraphElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLParamElement where
  toHTMLElement = HTMLParamElement.toHTMLElement
  fromHTMLElement = HTMLParamElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLPreElement where
  toHTMLElement = HTMLPreElement.toHTMLElement
  fromHTMLElement = HTMLPreElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLProgressElement where
  toHTMLElement = HTMLProgressElement.toHTMLElement
  fromHTMLElement = HTMLProgressElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLQuoteElement where
  toHTMLElement = HTMLQuoteElement.toHTMLElement
  fromHTMLElement = HTMLQuoteElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLScriptElement where
  toHTMLElement = HTMLScriptElement.toHTMLElement
  fromHTMLElement = HTMLScriptElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLSelectElement where
  toHTMLElement = HTMLSelectElement.toHTMLElement
  fromHTMLElement = HTMLSelectElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLSourceElement where
  toHTMLElement = HTMLSourceElement.toHTMLElement
  fromHTMLElement = HTMLSourceElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLSpanElement where
  toHTMLElement = HTMLSpanElement.toHTMLElement
  fromHTMLElement = HTMLSpanElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLStyleElement where
  toHTMLElement = HTMLStyleElement.toHTMLElement
  fromHTMLElement = HTMLStyleElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableCaptionElement where
  toHTMLElement = HTMLTableCaptionElement.toHTMLElement
  fromHTMLElement = HTMLTableCaptionElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableCellElement where
  toHTMLElement = HTMLTableCellElement.toHTMLElement
  fromHTMLElement = HTMLTableCellElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableColElement where
  toHTMLElement = HTMLTableColElement.toHTMLElement
  fromHTMLElement = HTMLTableColElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableDataCellElement where
  toHTMLElement = HTMLTableDataCellElement.toHTMLElement
  fromHTMLElement = HTMLTableDataCellElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableElement where
  toHTMLElement = HTMLTableElement.toHTMLElement
  fromHTMLElement = HTMLTableElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableHeaderCellElement where
  toHTMLElement = HTMLTableHeaderCellElement.toHTMLElement
  fromHTMLElement = HTMLTableHeaderCellElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableRowElement where
  toHTMLElement = HTMLTableRowElement.toHTMLElement
  fromHTMLElement = HTMLTableRowElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTableSectionElement where
  toHTMLElement = HTMLTableSectionElement.toHTMLElement
  fromHTMLElement = HTMLTableSectionElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTemplateElement where
  toHTMLElement = HTMLTemplateElement.toHTMLElement
  fromHTMLElement = HTMLTemplateElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTextAreaElement where
  toHTMLElement = HTMLTextAreaElement.toHTMLElement
  fromHTMLElement = HTMLTextAreaElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTimeElement where
  toHTMLElement = HTMLTimeElement.toHTMLElement
  fromHTMLElement = HTMLTimeElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTitleElement where
  toHTMLElement = HTMLTitleElement.toHTMLElement
  fromHTMLElement = HTMLTitleElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLTrackElement where
  toHTMLElement = HTMLTrackElement.toHTMLElement
  fromHTMLElement = HTMLTrackElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLUListElement where
  toHTMLElement = HTMLUListElement.toHTMLElement
  fromHTMLElement = HTMLUListElement.fromHTMLElement

instance IsHTMLElement HTML.HTMLVideoElement where
  toHTMLElement = HTMLVideoElement.toHTMLElement
  fromHTMLElement = HTMLVideoElement.fromHTMLElement
