module Test.Web.DOM.Document where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Error, error)
import Test.Web.DOM.DomType (class DOMType, typeName)
import Type.Proxy (Proxy(..))
import Web.DOM as DOM
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument

expectFromDocument
  :: forall a m
   . MonadError Error m
  => DOMType a
  => IsDocument a
  => DOM.Document
  -> m a
expectFromDocument =
  maybe
    ( throwError
        $ error
        $ "Unable to downcast Document to " <> typeName (Proxy :: _ a)
    )
    pure <<< fromDocument

class IsDocument a where
  toDocument :: a -> DOM.Document
  fromDocument :: DOM.Document -> Maybe a

instance IsDocument DOM.Document where
  toDocument = identity
  fromDocument = Just

instance IsDocument HTML.HTMLDocument where
  toDocument = HTMLDocument.toDocument
  fromDocument = HTMLDocument.fromDocument
