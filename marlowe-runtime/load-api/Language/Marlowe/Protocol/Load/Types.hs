{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Types
  where

import Data.ContractFragment (ContractFragment)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol

newtype MarloweLoad = StLoad [N]

instance HasSignature MarloweLoad where
  signature _ = "MarloweLoad"

type family Fill (ns :: [N]) :: [N] where
  Fill '[] = '[]
  Fill ('Z ': ns) = 'Z ': ns
  Fill ('S n ': ns) = n ': ns

instance Protocol MarloweLoad where
  data Message MarloweLoad st st' where
    MsgPush :: ContractFragment n -> Message MarloweLoad
      -- Client can push when the current contract has more than zero holes to
      -- fill.
      ('StLoad ('S n' ': ns))
      -- Push the number of holes in the new contract fragment onto the stack.
      ('StLoad (n ': 'S n' ': ns))
    MsgPop :: DatumHash -> Contract -> Message MarloweLoad
      -- Server will pop when the current contract has no more holes to fill.
      ('StLoad ('Z ': ns))
      -- Fill the first hole in the previous contract with the current
      -- contract.
      ('StLoad (Fill ns))

  data ClientHasAgency st where
    TokLoadClient :: ClientHasAgency ('StLoad ('S n ': ns))

  data ServerHasAgency st where
    TokLoadServer :: ServerHasAgency ('StLoad ('Z ': ns))

  data NobodyHasAgency st where
    TokLoadNobody :: NobodyHasAgency ('StLoad '[])

  exclusionLemma_ClientAndServerHaveAgency TokLoadClient = \case
  exclusionLemma_NobodyAndClientHaveAgency TokLoadNobody = \case
  exclusionLemma_NobodyAndServerHaveAgency TokLoadNobody = \case
