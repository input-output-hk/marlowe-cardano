{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}

module Data.Vec
  where

import Data.Binary (Binary(..), Get, Put)
import Data.Nat (N(..), Nat(..), SingNat(..), type (+), withSingNat)
import Prelude (Applicative(..), Eq, Foldable, Functor, Ord, Show, Traversable, ($), (.), (<$>))

data Vec (n :: N) a where
  Nil :: Vec 'Z a
  Cons :: a -> Vec n a -> Vec ('S n) a

deriving instance Show a => Show (Vec n a)
deriving instance Eq a => Eq (Vec n a)
deriving instance Ord a => Ord (Vec n a)
deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)
deriving instance Traversable (Vec n)

instance SingNat n => Applicative (Vec n) where
  pure = replicate (singNat @n)
  Nil <*> Nil = Nil
  Cons f vf <*> Cons a va = Cons (f a) case singNat @n of
    Succ n -> withSingNat n $ vf <*> va

length :: Vec n a -> Nat n
length Nil = Zero
length (Cons _ vec) = Succ $ length vec

head :: Vec ('S n) a -> a
head (Cons a _) = a

last :: Vec ('S n) a -> a
last (Cons a Nil) = a
last (Cons _ v@Cons{}) = last v

tail :: Vec ('S n) a -> Vec n a
tail (Cons _ vec) = vec

init :: Vec ('S n) a -> Vec n a
init (Cons _ Nil) = Nil
init (Cons a v@Cons{}) = Cons a $ init v

snoc :: Vec n a -> a -> Vec ('S n) a
snoc Nil a = Cons a Nil
snoc (Cons a' v) a = Cons a' $ snoc v a

(%++) :: Vec n a -> Vec m a -> Vec (n + m) a
(%++) Nil v2 = v2
(%++) (Cons a v1) v2 = Cons a $ v1 %++ v2

reverse :: Vec n a -> Vec n a
reverse Nil = Nil
reverse (Cons a v) = snoc (reverse v) a

replicate :: Nat n -> a -> Vec n a
replicate Zero _ = Nil
replicate (Succ n) a = Cons a $ replicate n a

toList :: forall n a. Vec n a -> [a]
toList = \case
  Nil -> []
  Cons a vec -> a : toList vec

fromList :: [a] -> SomeVec a
fromList = \case
  [] -> SomeVec Nil
  a : as -> case fromList as of
    SomeVec vec -> SomeVec $ Cons a vec

data SomeVec a = forall n. SomeVec (Vec n a)

putVec :: Binary a => Vec n a -> Put
putVec = put . toList

getVec :: Binary a => Get (SomeVec a)
getVec = fromList <$> get
