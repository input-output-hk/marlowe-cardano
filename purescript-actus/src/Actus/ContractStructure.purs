module Actus.ContractStructure
  ( ReferenceRole
  , ReferenceType
  , ContractReference
  , ContractStructure
  ) where

import Prelude

import Data.Map (SemigroupMap)
import Data.Semigroup.Last (Last)
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

newtype ContractStructure = ContractStructure
  (SemigroupMap String (Last ContractReference))

derive instance eqContractStructure :: Eq ContractStructure
instance showContractStructure :: Show ContractStructure where
  show (ContractStructure map) = show map
