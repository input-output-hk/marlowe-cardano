module Errors.Debuggable
  ( class Debuggable
  , debuggable
  , class DebuggableRecordFields
  , debuggableRecordFields
  , class VariantDebuggable
  , variantDebuggables
  ) where

import Prologue

import Data.Argonaut (Json, JsonDecodeError, encodeJson)
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
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

instance debuggableMaybe :: Debuggable a => Debuggable (Maybe a) where
  debuggable Nothing = Json.fromString "Debug information not available"
  debuggable (Just a) = debuggable a

instance Debuggable (AjaxError JsonDecodeError Json) where
  debuggable e = encodeJson
    { type: "AjaxError"
    , error: Json.fromString $ printAjaxError e
    }

instance Debuggable JsonDecodeError where
  debuggable e = encodeJson
    { type: "JsonDecodeError"
    , error: Json.fromString $ show e
    }

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
