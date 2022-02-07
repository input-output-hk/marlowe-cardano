{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marlowe.Contracts.Futures where

import Data.String (IsString (..))
import Language.Marlowe
import Marlowe.Contracts.Common

-- = Futures

-- |Future on the exchange rate of USD/ADA
future ::
     Party             -- ^ Buyer
  -> Party             -- ^ Seller
  -> Value Observation -- ^ Forward price
  -> Value Observation -- ^ Initial margin requirements
  -> Timeout           -- ^ Initial margin setup timout
  -> [Timeout]         -- ^ Margin call dates
  -> Timeout           -- ^ Delivery date
  -> Contract          -- ^ Future contract
future buyer seller forwardPrice initialMargin initialFixing callDates deliveryDate =
    depositInitialMargin buyer seller initialMargin initialFixing
  $ maintenanceMarginCalls buyer seller forwardPrice callDates
  $ settlement buyer seller forwardPrice deliveryDate
    Close

-- |Initial deposits into margin accounts
depositInitialMargin ::
     Party             -- ^ Buyer
  -> Party             -- ^ Seller
  -> Value Observation -- ^ Forward price
  -> Timeout           -- ^ Initial margin call
  -> Contract          -- ^ Continuation contract
  -> Contract          -- ^ Composed contract
depositInitialMargin buyer seller initalMargin initialFixing continuation =
    deposit buyer buyer (ada, initalMargin) initialFixing Close
  $ deposit seller seller (ada, initalMargin) initialFixing Close continuation

-- |Maintenance of the margin accounts
maintenanceMarginCalls ::
     Party             -- ^ Buyer
  -> Party             -- ^ Seller
  -> Value Observation -- ^ Forward price
  -> [Timeout]         -- ^ Call dates
  -> Contract          -- ^ Continuation contract
  -> Contract          -- ^ Composed contract
maintenanceMarginCalls buyer seller forwardPrice = flip $ foldl updateMarginAccounts
  where
    updateMarginAccounts :: Contract -> Timeout -> Contract
    updateMarginAccounts continuation timeout =
      let invId = toValueId "inv-spot" timeout
          dirId = toValueId "dir-spot" timeout
          amount = DivValue
                    (DivValue
                      (MulValue
                        (UseValue dirId)
                        (SubValue
                          (UseValue invId)
                          (MulValue scale forwardPrice)))
                      (MulValue scale scale))
                   contractSize
       in oracleInput invRate timeout Close
        $ oracleInput dirRate timeout Close
        $ Let invId (ChoiceValue invRate)
        $ Let dirId (ChoiceValue dirRate)
        $ If (ValueGE (UseValue invId) (MulValue scale forwardPrice))
             (updateMarginAccount seller amount timeout continuation)
             (updateMarginAccount buyer (NegValue amount) timeout continuation)

    updateMarginAccount :: Party -> Value Observation -> Timeout -> Contract -> Contract
    updateMarginAccount party value timeout continuation =
      If (ValueGT (AvailableMoney party ada) value)
        continuation
        (deposit party party (ada, value) timeout Close continuation)

toValueId :: String -> Slot -> ValueId
toValueId label timeout = fromString $ label ++ "@" ++ (show $ getSlot timeout)

-- |Settlement of the Future contract
-- At delivery, if spot price is bigger than forward the seller transfers
-- the difference to the buyer and vice versa
settlement ::
     Party             -- ^ Buyer
  -> Party             -- ^ Seller
  -> Value Observation -- ^ Forward price
  -> Timeout           -- ^ Delivery date
  -> Contract          -- ^ Continuation contract
  -> Contract          -- ^ Composed contract
settlement buyer seller forwardPrice deliveryDate continuation =
  let invId = toValueId "inv-spot" deliveryDate
      dirId = toValueId "dir-spot" deliveryDate
      amount = DivValue
                (DivValue
                  (MulValue
                    (UseValue dirId)
                    (SubValue
                      (UseValue invId)
                      (MulValue scale forwardPrice)))
                  (MulValue scale scale))
               contractSize
  in oracleInput dirRate deliveryDate Close
   $ oracleInput invRate deliveryDate Close
   $ Let invId (ChoiceValue invRate)
   $ Let dirId (ChoiceValue dirRate)
   $ If (ValueGE (UseValue invId) (MulValue scale forwardPrice))
       (pay seller buyer (ada, amount) continuation)
       (pay buyer seller (ada, NegValue amount) continuation)

-- |Constants
scale, contractSize :: Value Observation
scale        = Constant 1_000_000
contractSize = Constant 100
