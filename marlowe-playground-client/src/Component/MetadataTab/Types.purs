module Component.MetadataTab.Types where

import Prologue
import Contrib.Halogen.State.Record (Update)
import Contrib.Type.Proxy.Generic (fromRowLabels) as Proxies
import Contrib.Variant (tag) as Contrib.Variant
import Data.Map.Ordered.OMap (OMap)
import Data.Newtype (class Newtype, un)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Marlowe.Extended (ContractType)
import Marlowe.Extended.Metadata (NumberFormat)
import Marlowe.Semantics (TokenName) as S
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, Proxy(..))

-- | Let's leave this action type as it is so
-- | it can be an example of a `Variant` based action type
-- | and usage of different generic and non generic handlers
-- | for `Record` based state.
-- |
-- | There are three types of actions here:
-- |
-- | - `set*` - handled generically. Actions which have a clear semantic and just
-- | set a particular value in a state record.
-- |
-- | - `update*` - handled generically. Actions which have probably to broad semantic
-- | and can modify a particular field value. In this particular case we could possibly
-- | replace all actions with just a bunch of `update*` and move handlers logic
-- | entirely to template. I'm just not sure if such a move is a good idea.
-- |
-- | - Others. I've left these "custom" actions to show how to
-- | handle them in our final state handler together with generic handlers.
-- |
-- |
-- | `Variant` usage notes:
-- |
-- | * We close the row here and wrap the type in a `newtype` so actually we
-- | are building a closed sum type here. We do this because there is no
-- | obvious benefit of keeping the row polymorphic. In theory if we would have
-- | multiple components with shared subset of actions then possibly we could
-- | reuse some templates / handlers etc. This scenario seems a bit unrealistic.
-- | Regarding the `MetadataAction` it just prevents cluttering compiler messages
-- | with huge rows (compiler expands type aliases).
-- |
-- | * Thanks to `Variant` we gain ability to "compose handlers". With closed sum
-- | types there is no way to provide partial handler for a type and compose it with another
-- | partial handler and be sure that the final function is total. When we use
-- | `Variant` we can do this so we can provide "generic" handlers
-- | for some simple "atomic" actions over a `Record`.
-- |
-- | * I try here to improve the ergonomics of `Variant` usage by providing
-- | label proxy generator and building up `actionLabels`.
-- |
-- | Summary:
-- | By using `Variant` we gain generic handlers so we can focus on more
-- | meaningful actions when providing action interpreter.
-- | With `update*` actions we move one step further and add the ability to
-- | "remove" / "move" the interpretation of some parts of the action
-- | algebra and embed update functions directly in the template. This can reduce
-- | some interpretation code but the question remains if this
-- | kind of "trivial" interpretation is a boilerplate or rather
-- | a sane separation of concerns.
type MetadataActionRow
  = ( deleteChoiceInfo :: String
    , deleteRoleDescription :: S.TokenName
    , deleteValueParameterInfo :: String
    , moveUpValueParameterDescription :: String
    , moveDownValueParameterDescription :: String
    , setChoiceDescription :: String /\ String
    , setChoiceFormat :: String /\ NumberFormat
    , setContractLongDescription :: String
    , setContractName :: String
    , setContractShortDescription :: String
    , setContractType :: ContractType
    , setRoleDescription :: S.TokenName /\ String
    , setValueParameterDescription :: String /\ String
    , setValueParameterFormat :: String /\ NumberFormat
    , updateSlotParameterDescriptions :: Update (OMap String String)
    )

actionLabels ::
  { deleteChoiceInfo :: Proxy "deleteChoiceInfo"
  , deleteRoleDescription :: Proxy "deleteRoleDescription"
  , deleteValueParameterInfo :: Proxy "deleteValueParameterInfo"
  , moveUpValueParameterDescription :: Proxy "moveUpValueParameterDescription"
  , moveDownValueParameterDescription :: Proxy "moveDownValueParameterDescription"
  , setChoiceDescription :: Proxy "setChoiceDescription"
  , setChoiceFormat :: Proxy "setChoiceFormat"
  , setContractLongDescription :: Proxy "setContractLongDescription"
  , setContractName :: Proxy "setContractName"
  , setContractShortDescription :: Proxy "setContractShortDescription"
  , setContractType :: Proxy "setContractType"
  , setRoleDescription :: Proxy "setRoleDescription"
  , setValueParameterDescription :: Proxy "setValueParameterDescription"
  , setValueParameterFormat :: Proxy "setValueParameterFormat"
  , updateSlotParameterDescriptions :: Proxy "updateSlotParameterDescriptions"
  }
actionLabels = Proxies.fromRowLabels (Proxy :: Proxy MetadataActionRow)

newtype MetadataAction
  = MetadataAction (Variant MetadataActionRow)

derive instance purtyProblem :: Newtype MetadataAction _

showConstructor :: MetadataAction -> String
showConstructor = Contrib.Variant.tag <<< un MetadataAction

metadataAction :: forall a l proxy r_. Row.Cons l a r_ MetadataActionRow => IsSymbol l => proxy l -> a -> MetadataAction
metadataAction l = MetadataAction <<< Variant.inj l
