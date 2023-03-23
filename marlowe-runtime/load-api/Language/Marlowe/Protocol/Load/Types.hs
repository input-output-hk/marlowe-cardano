{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Types
  where

import Data.Binary (Binary(..))
import Data.ContractFragment (ContractFragment, SomeContractFragment(..), getContractFragment, putContractFragment)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash, fromDatum, toDatum)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol

-- The protocol state of the MarloweLoad protocol. Consists of a stack of
-- natural numbers, representing the number of unfilled holes in successive
-- levels of the contract hierarchy for the current branch of the contract.
newtype MarloweLoad = StLoad [N]

instance HasSignature MarloweLoad where
  signature _ = "MarloweLoad"

-- A helper for computing the target state of the Pop message.
type family Fill (ns :: [N]) :: [N] where
  -- We have popped the root contract, do nothing.
  Fill '[] = '[]
  -- The parent contract is already filled, do nothing (TODO prove to GHC that
  -- this case is impossible in practice).
  Fill ('Z ': ns) = 'Z ': ns
  -- The parent contract has at least one hole, fill the hole.
  Fill ('S n ': ns) = n ': ns

instance Protocol MarloweLoad where
  data Message MarloweLoad st st' where
    -- | Push a contract fragment to fill the first hole of the current
    -- contract.
    MsgPush :: ContractFragment n -> Message MarloweLoad
      -- Client can push when the current contract has more than zero holes to
      -- fill.
      ('StLoad ('S n' ': ns))
      -- Push the number of holes in the new contract fragment onto the stack.
      ('StLoad (n ': 'S n' ': ns))
    -- | Pop a filled contract off the stack.
    MsgPop :: DatumHash -> Contract -> Message MarloweLoad
      -- Server will pop when the current contract has no more holes to fill.
      ('StLoad ('Z ': ns))
      -- Fill the first hole in the previous contract with the current
      -- contract.
      ('StLoad (Fill ns))

  data ClientHasAgency st where
    -- Client has agency when there are holes in the current contract.
    TokLoadClient :: ClientHasAgency ('StLoad ('S n ': ns))

  data ServerHasAgency st where
    -- Server has agency when there are no holes in the current contract.
    TokLoadServer :: ServerHasAgency ('StLoad ('Z ': ns))

  data NobodyHasAgency st where
    -- Nobody has agency when the stack is empty.
    TokLoadNobody :: NobodyHasAgency ('StLoad '[])

  exclusionLemma_ClientAndServerHaveAgency TokLoadClient = \case
  exclusionLemma_NobodyAndClientHaveAgency TokLoadNobody = \case
  exclusionLemma_NobodyAndServerHaveAgency TokLoadNobody = \case

instance BinaryMessage MarloweLoad where
  putMessage = \case
    ClientAgency TokLoadClient -> \case
      MsgPush fragment -> putContractFragment fragment
    ServerAgency TokLoadServer -> \case
      MsgPop hash contract -> do
        put hash
        put $ toDatum contract
  getMessage = \case
    ClientAgency TokLoadClient -> do
      SomeContractFragment fragment <- getContractFragment
      pure $ SomeMessage $ MsgPush fragment
    ServerAgency TokLoadServer -> do
      hash <- get
      contract <- get >>= \datum -> case fromDatum datum of
        Nothing -> fail "Invalid contract bytes"
        Just contract -> pure contract
      pure $ SomeMessage $ MsgPop hash contract
