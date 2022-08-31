module Marlowe.DeinstantiatorTests where

import Prologue

import Data.BigInt.Argonaut (fromInt)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Examples.PureScript.Escrow as Escrow
import Examples.PureScript.ZeroCouponBond as ZeroCouponBond
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Extended.V1 (Module(..), toCore)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Time (unsafeInstantFromInt)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

all :: Spec Unit
all =
  describe "Deinstantiator Tests" do
    it "Escrow" do
      let
        Module { contract: extendedContract } = Escrow.contractModule

        mFilledEscrow :: Maybe Contract
        mFilledEscrow =
          toCore
            ( fillTemplate
                ( TemplateContent
                    { timeContent:
                        Map.fromFoldable
                          [ "Payment deadline" /\ unsafeInstantFromInt 600
                          , "Complaint deadline" /\ unsafeInstantFromInt 1800
                          , "Complaint response deadline" /\
                              unsafeInstantFromInt 2400
                          , "Mediation deadline" /\ unsafeInstantFromInt 3600
                          ]
                    , valueContent:
                        Map.fromFoldable
                          [ "Price" /\ fromInt 450
                          ]
                    }
                )
                extendedContract
            )
      shouldSatisfy (mFilledEscrow == Nothing) not
      shouldEqual
        (Just Escrow.contractModule)
        (maybe Nothing findTemplate mFilledEscrow)
    it "Zero Coupon Bond" do
      let
        Module { contract: extendedContract } = ZeroCouponBond.contractModule

        mFilledZeroCouponBond :: Maybe Contract
        mFilledZeroCouponBond =
          toCore
            ( fillTemplate
                ( TemplateContent
                    { timeContent:
                        Map.fromFoldable
                          [ "Loan deadline" /\ unsafeInstantFromInt 600
                          , "Payback deadline" /\ unsafeInstantFromInt 1500
                          ]
                    , valueContent:
                        Map.fromFoldable
                          [ "Interest" /\ fromInt 50
                          , "Amount" /\ fromInt 100
                          ]
                    }
                )
                extendedContract
            )
      shouldSatisfy (mFilledZeroCouponBond == Nothing) not
      shouldEqual
        (Just ZeroCouponBond.contractModule)
        (maybe Nothing findTemplate mFilledZeroCouponBond)
