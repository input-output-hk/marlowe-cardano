{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Builder.Types
  where

import Data.Nat (type (+))
import Data.Vec (Vec)
import Language.Marlowe.Core.V1.Semantics.Types
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol

newtype MarloweBuilder = StBuilder [N]

instance HasSignature MarloweBuilder where
  signature _ = "MarloweBuilder"

type family Built (ns :: [N]) :: [N] where
  Built '[] = '[]
  Built ('Z ': ns) = 'Z ': ns
  Built ('S n ': ns) = n ': ns

instance Protocol MarloweBuilder where
  data Message MarloweBuilder st st' where
    MsgPush :: ContractTemplate n -> Message MarloweBuilder
      ('StBuilder ('S n' ': ns))
      ('StBuilder (n ': 'S n' ': ns))
    MsgBuilt :: Contract -> Message MarloweBuilder
      ('StBuilder ('Z ': ns))
      ('StBuilder (Built ns))

  data ClientHasAgency st where
    TokBuilderClient :: ClientHasAgency ('StBuilder ('S n ': ns))

  data ServerHasAgency st where
    TokBuilderServer :: ServerHasAgency ('StBuilder ('Z ': ns))

  data NobodyHasAgency st where
    TokBuilderNobody :: NobodyHasAgency ('StBuilder '[])

  exclusionLemma_ClientAndServerHaveAgency TokBuilderClient = \case
  exclusionLemma_NobodyAndClientHaveAgency TokBuilderNobody = \case
  exclusionLemma_NobodyAndServerHaveAgency TokBuilderNobody = \case

data ContractTemplate (n :: N) where
  CloseT :: ContractTemplate 'Z
  PayT :: AccountId -> Payee  -> Token -> Value Observation -> ContractTemplate n -> ContractTemplate n
  IfT :: Observation -> ContractTemplate n -> ContractTemplate m -> ContractTemplate (n + m)
  WhenT :: Vec n Action -> Timeout -> ContractTemplate m -> ContractTemplate (n + m)
  LetT :: ValueId -> Value Observation -> ContractTemplate n -> ContractTemplate n
  AssertT :: Observation -> ContractTemplate n -> ContractTemplate n
