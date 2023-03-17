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


-- marlowe-cli/src/Language/Marlowe/CLI/Test/Wallet/Types.hs|256 col 12| • No instance for (GetConNames                     (GHC.Generics.M1                        GHC.Generics.C                        ('GHC.Generics.MetaCons "BurnAll" 'GHC.Generics.PrefixI 'True)                        (GHC.Generics.S1                           ('GHC.Generics.MetaSel                              ('Just "soCurrencyNickname")                              'GHC.Generics.NoSourceUnpackedness                              'GHC.Generics.NoSourceStrictness                              'GHC.Generics.DecidedLazy)                           (GHC.Generics.Rec0 CurrencyNickname)                         GHC.Generics.:*: GHC.Generics.S1                                            ('GHC.Generics.MetaSel                                               ('Just "soMetadata")                                               'GHC.Generics.NoSourceUnpackedness                                               'GHC.Generics.NoSourceStrictness
-- instance (GetConNames c) => GetConNames (M1 i c f p) where
--     getConNames = getConNames @c
--
-- instance (GetConNames a) => GetConNames (D1 d a) where
--     getConNames = getConNames @a
--
-- instance (GetConNames a, GetConNames b) => GetConNames (a :+: b) where
--     getConNames = getConNames @a <> getConNames @b
--
-- instance (KnownSymbol s) => GetConNames (C1 ('MetaData s x y z) a) where
--     getConNames = [ reflect (Proxy :: Proxy s) ]
--
