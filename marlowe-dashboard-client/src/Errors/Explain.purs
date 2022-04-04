module Errors.Explain
  ( class Explain
  , explain
  , explainString
  , class VariantExplain
  , variantExplains
  ) where

import Prologue

import Data.Argonaut (JsonDecodeError)
import Data.List as L
import Data.Variant (Variant)
import Data.Variant.Internal as V
import Marlowe.Semantics (TransactionError(..))
import Prim.RowList as RL
import Servant.PureScript (AjaxError)
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

instance Explain TransactionError where
  explain TEAmbiguousTimeIntervalError = text
    "The transaction was submitted with a time interval which contains a timeout of the contract. This means that the interpretation of the contract is ambiguos, and this transaction should not be accepted on chain."
  explain TEApplyNoMatchError = text
    "The transaction is not allowed by the contract"
  explain (TEIntervalError err) = text $ show err
  explain TEUselessTransaction = text "The transaction is useless"

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
