-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | This suite tests the Json serialization of the Marlowe core module
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}


module Spec.Marlowe.Serialization.CoreJson
  ( tests
  ) where


import Control.Arrow (Arrow((***)))
import Data.Aeson (decode, encode)
import Data.ByteString (ByteString)
import Language.Marlowe (IntervalError(..), MarloweParams(MarloweParams), POSIXTime(..))
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Plutus.V1.Ledger.Api (CurrencySymbol(CurrencySymbol), toBuiltin)
import Spec.Marlowe.Common (contractGen, shrinkContract)
import Test.QuickCheck (Gen, Property, arbitrary, (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Property (forAll, forAllShrink, withMaxSuccess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)


tests :: TestTree
tests = testGroup "Core Contract Serialization"
    [ testProperty "Serialise deserialise Contract loops" prop_contractJsonLoops
    , testProperty "Serialise deserialise MarloweParams loops" prop_marloweParamsJsonLoops
    , testProperty "Serialise deserialise IntervalError loops" prop_intervalErrorJsonLoops
    ]


-- | Test that JSON decoding inverts encoding for a contract.
contractJsonLoops :: Contract -> Property
contractJsonLoops cont = decode (encode cont) === Just cont


-- | Test that JSON decoding inverts encoding for contracts.
prop_contractJsonLoops :: Property
prop_contractJsonLoops = withMaxSuccess 1000 $ forAllShrink contractGen shrinkContract contractJsonLoops



-- | Test that JSON decoding inverts encoding for Marlowe parameters.
marloweParamsJsonLoops :: MarloweParams -> Property
marloweParamsJsonLoops mp = decode (encode mp) === Just mp


-- | Test that JSON decoding inverts encoding for Marlowe parameters.
prop_marloweParamsJsonLoops :: Property
prop_marloweParamsJsonLoops = withMaxSuccess 1000 $ forAll gen marloweParamsJsonLoops
  where
    gen = do
      c <- toBuiltin <$> (arbitrary :: Gen ByteString)
      return $ MarloweParams (CurrencySymbol c)


-- | Test that JSON decoding inverts encoding for an interval error.
intervalErrorJsonLoops :: IntervalError -> Property
intervalErrorJsonLoops ie = decode (encode ie) === Just ie


-- | Test that JSON decoding inverts encoding for interval errors.
prop_intervalErrorJsonLoops :: Property
prop_intervalErrorJsonLoops = withMaxSuccess 1000 $ forAll gen intervalErrorJsonLoops
  where
    gen = do
      b <- arbitrary
      if b
      then do
        t <- (POSIXTime *** POSIXTime) <$> arbitrary
        return $ InvalidInterval t
      else do
        s <- POSIXTime <$> arbitrary
        t <- (POSIXTime *** POSIXTime) <$> arbitrary
        return $ IntervalInPastError s t
