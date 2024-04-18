{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for safety analysis for Marlowe contracts.
module Language.Marlowe.Analysis.Safety.Types.Validator (
  -- * Types
  Validator (..),

  -- * Runner
  runValidator,

  -- * Combinators
  validatorSequence,
  validatorTraverse,

  -- * Constructors
  fromFn,
  fromFnEither,
  fromFnValidation,
  fromValidation,

  -- * Helpers
  validationErrors,
) where

import Control.Category (Category (id))
import qualified Control.Category as Category
import Data.Either.Validation (Validation (..))
import Data.Profunctor (Choice, Profunctor, Star (Star), Strong)

-- TODO: This could be extended to `Validator m err i o` and separated into a library in similar fashion to this:
-- https://github.com/purescript-polyform/polyform/blob/master/src/Polyform/Validator.purs#L43
newtype Validator err i o = Validator (Star (Validation err) i o)
deriving newtype instance Functor (Validator err i)
deriving newtype instance (Semigroup err) => Applicative (Validator err i)
deriving newtype instance Profunctor (Validator err)
deriving newtype instance (Semigroup err) => Choice (Validator err)
deriving newtype instance Strong (Validator err)

-- Use `Category` instead of `Monad` when you are working with dependent validation.
-- We can't provide `Monad` because we want to accumulate errors and not short circuit.
instance (Semigroup err) => Category (Validator err) where
  (.) (Validator (Star v2)) (Validator (Star v1)) =
    Validator $ Star $ \a -> case v1 a of
      (Success b) -> v2 b
      (Failure err) -> Failure err
  id = Validator . Star $ pure

runValidator :: Validator err a b -> a -> Validation err b
runValidator (Validator (Star v)) = v

validatorTraverse :: (Semigroup err, Traversable t) => Validator err a b -> Validator err (t a) (t b)
validatorTraverse v = Validator $ Star $ traverse (runValidator v)

validatorSequence :: (Semigroup err, Applicative t, Traversable t) => t (Validator err a b) -> Validator err (t a) (t b)
validatorSequence tva = Validator $ Star \ta -> sequenceA (runValidator <$> tva <*> ta)

fromFn :: (Semigroup err) => (a -> b) -> Validator err a b
fromFn f = Validator $ Star $ pure . f

fromFnEither :: (a -> Either err b) -> Validator err a b
fromFnEither f = Validator $ Star $ \a -> case f a of
  (Right b) -> Success b
  (Left err) -> Failure err

fromFnValidation :: (a -> Validation err b) -> Validator err a b
fromFnValidation f = Validator $ Star f

fromValidation :: Validation err b -> Validator err a b
fromValidation = fromFnValidation . const

validationErrors :: (Monoid err) => Validation err b -> err
validationErrors (Success _) = mempty
validationErrors (Failure err) = err
