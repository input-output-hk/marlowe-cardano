module Component.MetadataTab.Types where

import Prologue
import Contrib.Type.Proxy.Generic (fromRowLabels) as Proxies
import Contrib.Variant (tag) as Contrib.Variant
import Data.Newtype (class Newtype, un)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Marlowe.Extended (ContractType)
import Marlowe.Extended.Metadata (NumberFormat)
import Marlowe.Semantics (TokenName) as S
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, Proxy(..))

type MetadataActionRow
  = ( "setContractName" :: String
    , "setContractType" :: ContractType
    , "setContractShortDescription" :: String
    , "setContractLongDescription" :: String
    , "setRoleDescription" :: S.TokenName /\ String
    , "deleteRoleDescription" :: S.TokenName
    , "setSlotParameterDescription" :: String /\ String
    , "deleteSlotParameterDescription" :: String
    , "setValueParameterDescription" :: String /\ String
    , "setValueParameterFormat" :: String /\ NumberFormat
    , "deleteValueParameterInfo" :: String
    , "setChoiceDescription" :: String /\ String
    , "setChoiceFormat" :: String /\ NumberFormat
    , "deleteChoiceInfo" :: String
    )

labels ::
  { deleteChoiceInfo :: Proxy "deleteChoiceInfo"
  , deleteRoleDescription :: Proxy "deleteRoleDescription"
  , deleteSlotParameterDescription :: Proxy "deleteSlotParameterDescription"
  , deleteValueParameterInfo :: Proxy "deleteValueParameterInfo"
  , setChoiceDescription :: Proxy "setChoiceDescription"
  , setChoiceFormat :: Proxy "setChoiceFormat"
  , setContractLongDescription :: Proxy "setContractLongDescription"
  , setContractName :: Proxy "setContractName"
  , setContractShortDescription :: Proxy "setContractShortDescription"
  , setContractType :: Proxy "setContractType"
  , setRoleDescription :: Proxy "setRoleDescription"
  , setSlotParameterDescription :: Proxy "setSlotParameterDescription"
  , setValueParameterDescription :: Proxy "setValueParameterDescription"
  , setValueParameterFormat :: Proxy "setValueParameterFormat"
  }
labels = Proxies.fromRowLabels (Proxy :: Proxy MetadataActionRow)

newtype MetadataAction
  = MetadataAction (Variant MetadataActionRow)

derive instance purtyProblem :: Newtype MetadataAction _

showConstructor :: MetadataAction -> String
showConstructor = Contrib.Variant.tag <<< un MetadataAction

metadataAction :: forall a l proxy r_. Row.Cons l a r_ MetadataActionRow => IsSymbol l => proxy l -> a -> MetadataAction
metadataAction l = MetadataAction <<< Variant.inj l
