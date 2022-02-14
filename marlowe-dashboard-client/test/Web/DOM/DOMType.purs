module Test.Web.DOM.DomType where

import Type.Proxy (Proxy)
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.DOM as DOM
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.Event (Event)
import Web.File.FileReader (FileReader)
import Web.HTML as HTML
import Web.HTML.Event.BeforeUnloadEvent (BeforeUnloadEvent)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.ErrorEvent (ErrorEvent)
import Web.HTML.Event.HashChangeEvent (HashChangeEvent)
import Web.HTML.Event.PageTransitionEvent (PageTransitionEvent)
import Web.HTML.Event.PopStateEvent (PopStateEvent)
import Web.HTML.Event.TrackEvent (TrackEvent)
import Web.Socket.Event.CloseEvent (CloseEvent)
import Web.Socket.Event.MessageEvent (MessageEvent)
import Web.Socket.WebSocket (WebSocket)
import Web.Storage.Event.StorageEvent (StorageEvent)
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.UIEvent.CompositionEvent (CompositionEvent)
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.XHR.ProgressEvent (ProgressEvent)

class DOMType (a :: Type) where
  typeName :: Proxy a -> String

instance DOMType DOM.Node where
  typeName _ = "Node"

instance DOMType DOM.CharacterData where
  typeName _ = "CharacterData"

instance DOMType DOM.Comment where
  typeName _ = "Comment"

instance DOMType DOM.DocumentFragment where
  typeName _ = "DocumentFragment"

instance DOMType DOM.Document where
  typeName _ = "Document"

instance DOMType DOM.DocumentType where
  typeName _ = "DocumentType"

instance DOMType DOM.Element where
  typeName _ = "Element"

instance DOMType DOM.ProcessingInstruction where
  typeName _ = "ProcessingInstruction"

instance DOMType DOM.Text where
  typeName _ = "Text"

instance DOMType FileReader where
  typeName _ = "FileReader"

instance DOMType HTML.HTMLAnchorElement where
  typeName _ = "HTMLAnchorElement"

instance DOMType HTML.HTMLAreaElement where
  typeName _ = "HTMLAreaElement"

instance DOMType HTML.HTMLAudioElement where
  typeName _ = "HTMLAudioElement"

instance DOMType HTML.HTMLBaseElement where
  typeName _ = "HTMLBaseElement"

instance DOMType HTML.HTMLBodyElement where
  typeName _ = "HTMLBodyElement"

instance DOMType HTML.HTMLBRElement where
  typeName _ = "HTMLBRElement"

instance DOMType HTML.HTMLButtonElement where
  typeName _ = "HTMLButtonElement"

instance DOMType HTML.HTMLCanvasElement where
  typeName _ = "HTMLCanvasElement"

instance DOMType HTML.HTMLDataElement where
  typeName _ = "HTMLDataElement"

instance DOMType HTML.HTMLDataListElement where
  typeName _ = "HTMLDataListElement"

instance DOMType HTML.HTMLDivElement where
  typeName _ = "HTMLDivElement"

instance DOMType HTML.HTMLDListElement where
  typeName _ = "HTMLDListElement"

instance DOMType HTML.HTMLDocument where
  typeName _ = "HTMLDocument"

instance DOMType HTML.HTMLElement where
  typeName _ = "HTMLElement"

instance DOMType HTML.HTMLEmbedElement where
  typeName _ = "HTMLEmbedElement"

instance DOMType HTML.HTMLFieldSetElement where
  typeName _ = "HTMLFieldSetElement"

instance DOMType HTML.HTMLFormElement where
  typeName _ = "HTMLFormElement"

instance DOMType HTML.HTMLHeadElement where
  typeName _ = "HTMLHeadElement"

instance DOMType HTML.HTMLHeadingElement where
  typeName _ = "HTMLHeadingElement"

instance DOMType HTML.HTMLHRElement where
  typeName _ = "HTMLHRElement"

instance DOMType HTML.HTMLIFrameElement where
  typeName _ = "HTMLIFrameElement"

instance DOMType HTML.HTMLImageElement where
  typeName _ = "HTMLImageElement"

instance DOMType HTML.HTMLInputElement where
  typeName _ = "HTMLInputElement"

instance DOMType HTML.HTMLKeygenElement where
  typeName _ = "HTMLKeygenElement"

instance DOMType HTML.HTMLLabelElement where
  typeName _ = "HTMLLabelElement"

instance DOMType HTML.HTMLLegendElement where
  typeName _ = "HTMLLegendElement"

instance DOMType HTML.HTMLLIElement where
  typeName _ = "HTMLLIElement"

instance DOMType HTML.HTMLLinkElement where
  typeName _ = "HTMLLinkElement"

instance DOMType HTML.HTMLMapElement where
  typeName _ = "HTMLMapElement"

instance DOMType HTML.HTMLMediaElement where
  typeName _ = "HTMLMediaElement"

instance DOMType HTML.HTMLMetaElement where
  typeName _ = "HTMLMetaElement"

instance DOMType HTML.HTMLMeterElement where
  typeName _ = "HTMLMeterElement"

instance DOMType HTML.HTMLModElement where
  typeName _ = "HTMLModElement"

instance DOMType HTML.HTMLObjectElement where
  typeName _ = "HTMLObjectElement"

instance DOMType HTML.HTMLOListElement where
  typeName _ = "HTMLOListElement"

instance DOMType HTML.HTMLOptGroupElement where
  typeName _ = "HTMLOptGroupElement"

instance DOMType HTML.HTMLOptionElement where
  typeName _ = "HTMLOptionElement"

instance DOMType HTML.HTMLOutputElement where
  typeName _ = "HTMLOutputElement"

instance DOMType HTML.HTMLParagraphElement where
  typeName _ = "HTMLParagraphElement"

instance DOMType HTML.HTMLParamElement where
  typeName _ = "HTMLParamElement"

instance DOMType HTML.HTMLPreElement where
  typeName _ = "HTMLPreElement"

instance DOMType HTML.HTMLProgressElement where
  typeName _ = "HTMLProgressElement"

instance DOMType HTML.HTMLQuoteElement where
  typeName _ = "HTMLQuoteElement"

instance DOMType HTML.HTMLScriptElement where
  typeName _ = "HTMLScriptElement"

instance DOMType HTML.HTMLSelectElement where
  typeName _ = "HTMLSelectElement"

instance DOMType HTML.HTMLSourceElement where
  typeName _ = "HTMLSourceElement"

instance DOMType HTML.HTMLSpanElement where
  typeName _ = "HTMLSpanElement"

instance DOMType HTML.HTMLStyleElement where
  typeName _ = "HTMLStyleElement"

instance DOMType HTML.HTMLTableCaptionElement where
  typeName _ = "HTMLTableCaptionElement"

instance DOMType HTML.HTMLTableCellElement where
  typeName _ = "HTMLTableCellElement"

instance DOMType HTML.HTMLTableColElement where
  typeName _ = "HTMLTableColElement"

instance DOMType HTML.HTMLTableDataCellElement where
  typeName _ = "HTMLTableDataCellElement"

instance DOMType HTML.HTMLTableElement where
  typeName _ = "HTMLTableElement"

instance DOMType HTML.HTMLTableHeaderCellElement where
  typeName _ = "HTMLTableHeaderCellElement"

instance DOMType HTML.HTMLTableRowElement where
  typeName _ = "HTMLTableRowElement"

instance DOMType HTML.HTMLTableSectionElement where
  typeName _ = "HTMLTableSectionElement"

instance DOMType HTML.HTMLTemplateElement where
  typeName _ = "HTMLTemplateElement"

instance DOMType HTML.HTMLTextAreaElement where
  typeName _ = "HTMLTextAreaElement"

instance DOMType HTML.HTMLTimeElement where
  typeName _ = "HTMLTimeElement"

instance DOMType HTML.HTMLTitleElement where
  typeName _ = "HTMLTitleElement"

instance DOMType HTML.HTMLTrackElement where
  typeName _ = "HTMLTrackElement"

instance DOMType HTML.HTMLUListElement where
  typeName _ = "HTMLUListElement"

instance DOMType HTML.HTMLVideoElement where
  typeName _ = "HTMLVideoElement"

instance DOMType HTML.Window where
  typeName _ = "Window"

instance DOMType WebSocket where
  typeName _ = "WebSocket"

instance DOMType Event where
  typeName _ = "Event"

instance DOMType ClipboardEvent where
  typeName _ = "ClipboardEvent"

instance DOMType CustomEvent where
  typeName _ = "CustomEvent"

instance DOMType BeforeUnloadEvent where
  typeName _ = "BeforeUnloadEvent"

instance DOMType DragEvent where
  typeName _ = "DragEvent"

instance DOMType ErrorEvent where
  typeName _ = "ErrorEvent"

instance DOMType HashChangeEvent where
  typeName _ = "HashChangeEvent"

instance DOMType PageTransitionEvent where
  typeName _ = "PageTransitionEvent"

instance DOMType PopStateEvent where
  typeName _ = "PopStateEvent"

instance DOMType TrackEvent where
  typeName _ = "TrackEvent"

instance DOMType CloseEvent where
  typeName _ = "CloseEvent"

instance DOMType MessageEvent where
  typeName _ = "MessageEvent"

instance DOMType StorageEvent where
  typeName _ = "StorageEvent"

instance DOMType TouchEvent where
  typeName _ = "TouchEvent"

instance DOMType CompositionEvent where
  typeName _ = "CompositionEvent"

instance DOMType FocusEvent where
  typeName _ = "FocusEvent"

instance DOMType InputEvent where
  typeName _ = "InputEvent"

instance DOMType KeyboardEvent where
  typeName _ = "KeyboardEvent"

instance DOMType MouseEvent where
  typeName _ = "MouseEvent"

instance DOMType UIEvent where
  typeName _ = "UIEvent"

instance DOMType WheelEvent where
  typeName _ = "WheelEvent"

instance DOMType ProgressEvent where
  typeName _ = "ProgressEvent"
