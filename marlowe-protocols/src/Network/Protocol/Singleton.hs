{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Network.Protocol.Singleton where

import Data.Kind (Constraint)
import Network.TypedProtocol hiding (FlipAgency, TheyHaveAgency)

type SingClientHasAgency :: forall ps. ps -> Constraint
class SingClientHasAgency st where
  singClientHasAgency :: ClientHasAgency st

type SingServerHasAgency :: forall ps. ps -> Constraint
class SingServerHasAgency st where
  singServerHasAgency :: ServerHasAgency st

type SingNobodyHasAgency :: forall ps. ps -> Constraint
class SingNobodyHasAgency st where
  singNobodyHasAgency :: NobodyHasAgency st

data SPeerRole (pr :: PeerRole) where
  SAsClient :: SPeerRole 'AsClient
  SAsServer :: SPeerRole 'AsServer

class SingPeerRole (pr :: PeerRole) where
  singPeerRole :: SPeerRole pr

class SingWeHaveAgency pr st where
  singWeHaveAgency :: WeHaveAgency pr st

instance (SingClientHasAgency st) => SingWeHaveAgency 'AsClient st where
  singWeHaveAgency = ClientAgency singClientHasAgency

instance (SingServerHasAgency st) => SingWeHaveAgency 'AsServer st where
  singWeHaveAgency = ServerAgency singServerHasAgency

type family FlipAgency (pr :: PeerRole) = (r :: PeerRole) | r -> pr where
  FlipAgency 'AsClient = 'AsServer
  FlipAgency 'AsServer = 'AsClient

type TheyHaveAgency pr = PeerHasAgency (FlipAgency pr)

class SingTheyHaveAgency pr st where
  singTheyHaveAgency :: TheyHaveAgency pr st

instance (SingServerHasAgency st) => SingTheyHaveAgency 'AsClient st where
  singTheyHaveAgency = ServerAgency singServerHasAgency

instance (SingClientHasAgency st) => SingTheyHaveAgency 'AsServer st where
  singTheyHaveAgency = ClientAgency singClientHasAgency
