module Marlowe.DeinstantiatorTests where

import Prologue

import Data.BigInt.Argonaut (fromInt)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Tuple.Nested ((/\))
import Examples.PureScript.Escrow as Escrow
import Examples.PureScript.ZeroCouponBond as ZeroCouponBond
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.Extended (toCore)
import Marlowe.Semantics (Contract)
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

all :: Spec Unit
all =
  describe "Deinstantiator Tests" do
    it "Escrow" do
      let
        mFilledEscrow :: Maybe Contract
        mFilledEscrow =
          toCore
            ( fillTemplate
                ( TemplateContent
                    { slotContent:
                        Map.fromFoldable
                          [ "Payment deadline" /\ fromInt 600
                          , "Complaint response deadline" /\ fromInt 1800
                          , "Complaint deadline" /\ fromInt 2400
                          , "Mediation deadline" /\ fromInt 3600
                          ]
                    , valueContent:
                        Map.fromFoldable
                          [ "Price" /\ fromInt 450
                          ]
                    }
                )
                Escrow.contractTemplate.extendedContract
            )
      shouldSatisfy (mFilledEscrow == Nothing) not
      shouldEqual
        (Just Escrow.contractTemplate)
        (maybe Nothing findTemplate mFilledEscrow)
    it "Zero Coupon Bond" do
      let
        mFilledZeroCouponBond :: Maybe Contract
        mFilledZeroCouponBond =
          toCore
            ( fillTemplate
                ( TemplateContent
                    { slotContent:
                        Map.fromFoldable
                          [ "Loan deadline" /\ fromInt 600
                          , "Payback deadline" /\ fromInt 1500
                          ]
                    , valueContent:
                        Map.fromFoldable
                          [ "Interest" /\ fromInt 50
                          , "Amount" /\ fromInt 100
                          ]
                    }
                )
                ZeroCouponBond.contractTemplate.extendedContract
            )
      shouldSatisfy (mFilledZeroCouponBond == Nothing) not
      shouldEqual
        (Just ZeroCouponBond.contractTemplate)
        (maybe Nothing findTemplate mFilledZeroCouponBond)
