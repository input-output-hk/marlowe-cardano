module Errors
  ( class Debuggable
  , debuggable
  , debuggableString
  , class DebuggableRecordFields
  , debuggableRecordFields
  , class Explain
  , explain
  , explainString
  , class VariantDebuggable
  , variantDebuggables
  , class VariantExplain
  , variantExplains
  ) where

import Prologue

import Data.Argonaut (Json, JsonDecodeError)
import Data.Argonaut as Json
import Data.Array (cons)
import Data.Int as Int
import Data.List as L
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Internal as V
import Foreign.Object as Object
import Prim.RowList as RL
import Record.Unsafe (unsafeGet)
import Servant.PureScript (AjaxError, printAjaxError)
import Text.Pretty (Doc, text)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- This typeclass is used to produce user facing explanation of errors. It is intended to be
-- shown in error Toast messages or in other feedback elements. We use Doc instead of string to
-- eventually be able to add verbose explanations with newlines and indentantions and be able
-- to transform it to a string or to HTML
class Explain a where
  explain :: a -> Doc

instance Explain (AjaxError a b) where
  explain _ = text
    "A request was made to the server, but the expected response was not returned."

instance Explain JsonDecodeError where
  explain _ = text "We received some information that we couldn't recognize."

class VariantExplain :: RL.RowList Type -> Constraint
class VariantExplain rl where
  variantExplains :: forall proxy. proxy rl -> L.List (V.VariantCase -> Doc)

instance explainVariantNil :: VariantExplain RL.Nil where
  variantExplains _ = L.Nil

instance explainVariantCons ::
  ( VariantExplain rs
  , Explain a
  ) =>
  VariantExplain (RL.Cons sym a rs) where
  variantExplains _ =
    L.Cons (coerceExplain explain) (variantExplains (Proxy :: Proxy rs))
    where
    coerceExplain :: (a -> Doc) -> V.VariantCase -> Doc
    coerceExplain = unsafeCoerce

instance explainVariant ::
  ( RL.RowToList r rl
  , V.VariantTags rl
  , VariantExplain rl
  ) =>
  Explain (Variant r) where
  explain v1 =
    let
      V.VariantRep v = unsafeCoerce v1 :: V.VariantRep V.VariantCase
      tags = V.variantTags (Proxy :: Proxy rl)
      explains = variantExplains (Proxy :: Proxy rl)
      body = V.lookup "explain" v.type tags explains v.value
    in
      body

explainString :: forall a. Explain a => a -> String
explainString = show <<< explain

----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------

-- This typeclass is intended to extract context for an error that should help a developer
-- find the root cause of a problem. It is intended to be displayed in the Console or send
-- to a log service like getsentry.
class Debuggable a where
  debuggable :: a -> Json

instance Debuggable Json where
  debuggable = identity

instance Debuggable String where
  debuggable = Json.fromString

instance Debuggable Int where
  debuggable = Json.fromNumber <<< Int.toNumber

instance Debuggable Number where
  debuggable = Json.fromNumber

instance debuggableArray :: Debuggable a => Debuggable (Array a) where
  debuggable = Json.fromArray <<< map debuggable

instance Debuggable (AjaxError JsonDecodeError Json) where
  debuggable error = Json.fromString $ printAjaxError Json.stringify
    Json.printJsonDecodeError
    error

instance Debuggable JsonDecodeError where
  debuggable = Json.fromString <<< show

instance debuggableRecord ::
  ( RL.RowToList row list
  , DebuggableRecordFields list row
  ) =>
  Debuggable (Record row) where
  debuggable record =
    Json.fromObject
      $ Object.fromFoldable
          (debuggableRecordFields (Proxy :: Proxy list) record)

class DebuggableRecordFields :: RL.RowList Type -> Row Type -> Constraint
class DebuggableRecordFields rowlist row where
  debuggableRecordFields
    :: forall rlproxy. rlproxy rowlist -> Record row -> Array (String /\ Json)

instance debuggableRecordFieldsNil :: DebuggableRecordFields RL.Nil row where
  debuggableRecordFields _ _ = []

instance debuggableRecordFieldsCons ::
  ( IsSymbol key
  , DebuggableRecordFields rowlistTail row
  , Debuggable focus
  ) =>
  DebuggableRecordFields (RL.Cons key focus rowlistTail) row where
  debuggableRecordFields _ record = cons (key /\ debuggable focus) tail
    where
    key = reflectSymbol (Proxy :: Proxy key)
    focus = unsafeGet key record :: focus
    tail = debuggableRecordFields (Proxy :: Proxy rowlistTail) record

class VariantDebuggable :: RL.RowList Type -> Constraint
class VariantDebuggable rl where
  variantDebuggables :: forall proxy. proxy rl -> L.List (V.VariantCase -> Json)

instance debuggableVariantNil :: VariantDebuggable RL.Nil where
  variantDebuggables _ = L.Nil

instance debuggableVariantCons ::
  ( VariantDebuggable rs
  , Debuggable a
  ) =>
  VariantDebuggable (RL.Cons sym a rs) where
  variantDebuggables _ =
    L.Cons (coerceDebuggable debuggable)
      (variantDebuggables (Proxy :: Proxy rs))
    where
    coerceDebuggable :: (a -> Json) -> V.VariantCase -> Json
    coerceDebuggable = unsafeCoerce

instance debuggableVariant ::
  ( RL.RowToList r rl
  , V.VariantTags rl
  , VariantDebuggable rl
  ) =>
  Debuggable (Variant r) where
  debuggable v1 =
    let
      V.VariantRep v = unsafeCoerce v1 :: V.VariantRep V.VariantCase
      tags = V.variantTags (Proxy :: Proxy rl)
      explains = variantDebuggables (Proxy :: Proxy rl)
      body = V.lookup "debuggable" v.type tags explains v.value
    in
      body

debuggableString :: forall a. Debuggable a => a -> String
debuggableString = Json.stringify <<< debuggable
