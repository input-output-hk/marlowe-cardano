{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Contrib.Data.Aeson.Generic
  where

import Data.Data (Proxy(Proxy))
import Data.Reflection (reflect)
import GHC.Generics (C1, Constructor(conName), D1, Generic(Rep, from), M1(M1), Meta(MetaData), type (:+:)(L1, R1))
import GHC.TypeLits (KnownSymbol)

-- | No exported by Aeson lib.
class GetConName f where
    getConName :: f a -> String

instance (GetConName a, GetConName b) => GetConName (a :+: b) where
    getConName (L1 x) = getConName x
    getConName (R1 x) = getConName x

instance (Constructor c) => GetConName (C1 c a) where
    getConName = conName

-- For genericToJSONKey
instance GetConName a => GetConName (D1 d a) where
    getConName (M1 x) = getConName x


constructorName :: forall a. (Generic a, GetConName (Rep a)) => a -> String
constructorName = getConName . from

class GetConNames (rep :: * -> *) where
    getConNames :: [String]

