{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Futures where

import Data.String (IsString (..))
import Language.Marlowe

import Common

-- = Futures

-- |Future on the exchange rate of USC/ADA
future ::
     Value Observation -- ^ Forward price
  -> Value Observation -- ^ Initial margin requirements
  -> Timeout           -- ^ Initial margin setup timout
  -> [Timeout]         -- ^ Margin call dates
  -> Timeout           -- ^ Delivery date
  -> Contract          -- ^ Future contract
future forwardPrice initialMargin initialFixing callDates deliveryDate =
    depositInitialMargin initialMargin initialFixing
  $ maintenanceMarginCalls forwardPrice callDates
  $ settlement forwardPrice deliveryDate
    Close

-- |Initial deposits into margin accounts
depositInitialMargin ::
     Value Observation -- ^ Forward price
  -> Timeout           -- ^ Initial margin call
  -> Contract          -- ^ Continuation contract
  -> Contract          -- ^ Composed contract
depositInitialMargin initalMargin initialFixing continuation =
    deposit buyer buyer (ada, initalMargin) initialFixing Close
  $ deposit seller seller (ada, initalMargin) initialFixing Close continuation

-- |Maintenance of the margin accounts
maintenanceMarginCalls ::
     Value Observation -- ^ Forward price
  -> [Timeout]         -- ^ Call dates
  -> Contract          -- ^ Continuation contract
  -> Contract          -- ^ Composed contract
maintenanceMarginCalls forwardPrice = flip $ foldl updateMarginAccounts
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
     Value Observation -- ^ Forward price
  -> Timeout           -- ^ Delivery date
  -> Contract          -- ^ Continuation contract
  -> Contract          -- ^ Composed contract
settlement forwardPrice deliveryDate continuation =
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

-- |Exchange rates
dirRate, invRate :: ChoiceId
dirRate = ChoiceId "dir-adausd" oracle -- USC/ADA
invRate = ChoiceId "inv-adausd" oracle -- ADA/UCS

-- |Oracle input
oracleInput ::
     ChoiceId  -- ^ Oracle selector
  -> Timeout   -- ^ Timeout for oracle input
  -> Contract  -- ^ Continuation in case of timeout
  -> Contract  -- ^ Continuation contract
  -> Contract  -- ^ Composed contract
oracleInput choiceId timeout timeoutContinuation continuation =
  When
    [Case (Choice choiceId [Bound 0 100_000_000_000]) continuation]
    timeout
    timeoutContinuation

-- |Buyer and Seller are roles in the Future contract
-- as well as the Oracle
buyer, seller, oracle :: Party
buyer  = Role "Buyer"
seller = Role "Seller"
oracle = Role "kraken"

-- |Constants
scale, contractSize :: Value Observation
scale        = Constant 1_000_000
contractSize = Constant 100

-- |main
main :: IO ()
main = print . pretty $ contract
  where
    contract =
      future
        (Constant forwardPrice)
        (Constant initialMargin)
        (Slot setup)
        (reverse $ map Slot [start, start + step .. delivery])
        (Slot delivery)
    forwardPrice  = 90 -- Ada
    initialMargin = 20 -- Ada
    setup         = 1
    start         = 10
    step          = 10
    delivery      = 100
