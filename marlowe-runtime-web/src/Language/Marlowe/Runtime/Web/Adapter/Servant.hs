{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Marlowe.Runtime.Web.Adapter.Servant (
  WithRuntimeStatus,
  OperationId,
  RenameResponseSchema,
  RenameSchema,
  ListObject (..),
  AddRenameSchema,
) where

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (
  NamedSchema (..),
  ToSchema,
  allOperations,
  declareNamedSchema,
  operationId,
 )
import qualified Data.Text as T
import GHC.Base (Symbol)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Servant (
  HasServer (..),
  Headers,
  IsElem,
  IsElem',
  Proxy (..),
  Stream,
  Verb,
  type (:<|>),
  type (:>),
 )
import Servant.Client (HasClient (..))
import Servant.OpenApi (HasOpenApi (toOpenApi))

data WithRuntimeStatus api

data OperationId (name :: Symbol)

data RenameResponseSchema (name :: Symbol)

data RenameSchema (name :: Symbol) a

type family AddRenameSchema name api where
  AddRenameSchema name (path :> api) = path :> AddRenameSchema name api
  AddRenameSchema name (a :<|> b) = AddRenameSchema name a :<|> AddRenameSchema name b
  AddRenameSchema name (Verb method cTypes status (Headers hs a)) =
    Verb method cTypes status (Headers hs (RenameSchema name a))
  AddRenameSchema name (Verb method cTypes status a) = Verb method cTypes status (RenameSchema name a)
  AddRenameSchema name (Stream method status framing ct (Headers hs a)) =
    Stream method status framing ct (Headers hs (RenameSchema name a))
  AddRenameSchema name (Stream cTypes status framing ct a) = Stream cTypes status framing ct (RenameSchema name a)

instance (KnownSymbol name, ToSchema a) => ToSchema (RenameSchema name a) where
  declareNamedSchema _ = do
    NamedSchema _ schema <- declareNamedSchema $ Proxy @a
    pure $ NamedSchema (Just $ T.pack $ symbolVal $ Proxy @name) schema

instance (HasServer sub ctx) => HasServer (OperationId name :> sub) ctx where
  type ServerT (OperationId name :> sub) m = ServerT sub m
  route _ = route $ Proxy @sub
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @sub

instance (HasClient m api) => HasClient m (OperationId name :> api) where
  type Client m (OperationId name :> api) = Client m api
  clientWithRoute m _ = clientWithRoute m $ Proxy @api
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @api

instance (KnownSymbol name, HasOpenApi api) => HasOpenApi (OperationId name :> api) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      & allOperations . operationId ?~ T.pack (symbolVal $ Proxy @name)

instance (HasServer sub ctx) => HasServer (RenameResponseSchema name :> sub) ctx where
  type ServerT (RenameResponseSchema name :> sub) m = ServerT sub m
  route _ = route $ Proxy @sub
  hoistServerWithContext _ = hoistServerWithContext $ Proxy @sub

instance (HasClient m api) => HasClient m (RenameResponseSchema name :> api) where
  type Client m (RenameResponseSchema name :> api) = Client m api
  clientWithRoute m _ = clientWithRoute m $ Proxy @api
  hoistClientMonad m _ = hoistClientMonad m $ Proxy @api

instance (KnownSymbol name, HasOpenApi (AddRenameSchema name api)) => HasOpenApi (RenameResponseSchema name :> api) where
  toOpenApi _ = toOpenApi $ Proxy @(AddRenameSchema name api)

type instance IsElem' e (WithRuntimeStatus api) = IsElem e api

-- | A wrapper for a list of objects.
newtype ListObject a = ListObject {results :: [a]}
  deriving (Eq, Show, Ord, Functor, GHC.Generics.Generic)

instance (ToJSON a) => ToJSON (ListObject a)
instance (FromJSON a) => FromJSON (ListObject a)
instance (ToSchema a) => ToSchema (ListObject a)
