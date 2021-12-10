module Component.MetadataTab.Types where

import Prologue
import Contrib.Halogen (Update)
import Contrib.Variant (tag) as Contrib.Variant
import Data.Map (Map)
import Data.Map.Ordered.OMap (OMap)
import Data.Newtype (class Newtype, un)
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Marlowe.Extended (ContractType)
import Marlowe.Extended.Metadata (ChoiceInfo, ValueParameterInfo)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)

type MetadataActionRow
  = ( choiceInfo :: Update (Map String ChoiceInfo)
    , contractLongDescription :: Update String
    , contractName :: Update String
    , contractShortDescription :: Update String
    , contractType :: Update ContractType
    , roleDescriptions :: Update (Map String String)
    , slotParameterDescriptions :: Update (OMap String String)
    , valueParameterInfo :: Update (OMap String ValueParameterInfo)
    )

newtype MetadataAction
  = MetadataAction (Variant MetadataActionRow)

derive instance purtyProblem :: Newtype MetadataAction _

showConstructor :: MetadataAction -> String
showConstructor = Contrib.Variant.tag <<< un MetadataAction

metadataAction :: forall a l proxy r_. Row.Cons l a r_ MetadataActionRow => IsSymbol l => proxy l -> a -> MetadataAction
metadataAction l = MetadataAction <<< Variant.inj l
