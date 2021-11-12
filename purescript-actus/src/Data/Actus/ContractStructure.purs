module Data.Actus.ContractStructure
  ( ReferenceRole
  , ReferenceType
  , ContractReference
  , ContractStructure
  ) where

import Data.Map (SemigroupMap)
import Data.Semigroup.Last (Last)
import Data.Unit (Unit)
import Data.Variant (Variant)

type ReferenceRole = Variant
  ( underlying :: Unit
  , firstLeg :: Unit
  , secondLeg :: Unit
  , coveredContract :: Unit
  , coveringContract :: Unit
  )

type ReferenceType = Variant
  ( contract :: Unit
  , contractIdentifier :: Unit
  , marketObjectIdentifier :: Unit
  , legalEntityIdentifier :: Unit
  , contractStructure :: Unit
  )

type ContractReference =
  { object :: String
  , type :: ReferenceType
  , role :: ReferenceRole
  }

newtype ContractStructure = ContractStructe
  (SemigroupMap String (Last ContractReference))
