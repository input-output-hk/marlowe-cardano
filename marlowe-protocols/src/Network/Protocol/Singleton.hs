{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Network.Protocol.Singleton where

import Data.Kind (Constraint)
import Network.TypedProtocol hiding (FlipAgency, TheyHaveAgency)

type SingWeHaveAgency :: forall ps. PeerRole -> ps -> Constraint
class SingWeHaveAgency pr st where
  singWeHaveAgency :: WeHaveAgency pr st

type family FlipAgency (pr :: PeerRole) = (r :: PeerRole) | r -> pr where
  FlipAgency 'AsClient = 'AsServer
  FlipAgency 'AsServer = 'AsClient

type TheyHaveAgency pr = PeerHasAgency (FlipAgency pr)

type SingTheyHaveAgency :: forall ps. PeerRole -> ps -> Constraint
class SingTheyHaveAgency pr st where
  singTheyHaveAgency :: TheyHaveAgency pr st

type SingNobodyHasAgency :: forall ps. ps -> Constraint
class SingNobodyHasAgency st where
  singNobodyHasAgency :: NobodyHasAgency st
