{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.Futures
  ( future
  ) where

import Data.String (IsString(..))
import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common

-- |Future on the exchange rate of ADA/USD
--
-- A Future is an obligation for the parties involed in the
-- contract to exchange assets at maturity for the predefined
-- value.
--
-- The Future implemented here exchanges ADA for USD. The contract
-- is cash settled, i.e. USD is delivered in ADA resp. the difference
-- between the amount of the USD in ADA and the amount of ADA is due
-- at maturity.
--
-- The contract relies on /margin accounts/. The parties are required
-- to make payments into the margin account, in case the exchange
-- rate of ADA/USD changes considerably.
--
-- An oracle is used to get the exchange rate ADA/USD. As it is
-- implemented this currently works only in the Marlowe Playground.
future ::
     Party     -- ^ Buyer
  -> Party     -- ^ Seller
  -> Value     -- ^ Forward price for 100 (contract size) USD at maturity (in Lovelace)
  -> Value     -- ^ Initial margin requirements (in Lovelace)
  -> Timeout   -- ^ Initial margin setup timout
  -> [Timeout] -- ^ Margin call dates
  -> Timeout   -- ^ Delivery date
  -> Contract  -- ^ Future contract
future buyer seller forwardPrice initialMargin initialFixing callDates deliveryDate =
    depositInitialMargin buyer seller initialMargin initialFixing
  $ maintenanceMarginCalls buyer seller forwardPrice callDates
  $ settlement buyer seller forwardPrice deliveryDate
    Close

-- |Initial deposits into margin accounts
depositInitialMargin ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Value    -- ^ Forward price
  -> Timeout  -- ^ Initial margin call
  -> Contract -- ^ Continuation contract
  -> Contract -- ^ Composed contract
depositInitialMargin buyer seller initalMargin initialFixing continuation =
    deposit buyer buyer (ada, initalMargin) initialFixing Close
  $ deposit seller seller (ada, initalMargin) initialFixing Close continuation

-- |Maintenance of the margin accounts
maintenanceMarginCalls ::
     Party     -- ^ Buyer
  -> Party     -- ^ Seller
  -> Value     -- ^ Forward price
  -> [Timeout] -- ^ Call dates
  -> Contract  -- ^ Continuation contract
  -> Contract  -- ^ Composed contract
maintenanceMarginCalls buyer seller forwardPrice callDates cont =
    foldl updateMarginAccounts cont callDates
  where
    updateMarginAccounts :: Contract -> Timeout -> Contract
    updateMarginAccounts continuation timeout =
      let invId = toValueId "inv-spot" timeout
          dirId = toValueId "dir-spot" timeout
          amount = DivValue
                     (MulValue
                       (UseValue dirId)
                       (SubValue (UseValue invId) forwardPrice))
                     (MulValue contractSize scale)
          liquidation a b = pay a b (ada, AvailableMoney a ada) Close
       in oracleInput dirRate timeout Close
        $ oracleInput invRate timeout Close
        $ Let dirId (ChoiceValue dirRate)
        $ Let invId (ChoiceValue invRate)
        $ If (ValueGE (UseValue invId) forwardPrice)
             (updateMarginAccount seller amount timeout (liquidation seller buyer) continuation)
             (updateMarginAccount buyer (NegValue amount) timeout (liquidation buyer seller) continuation)

    updateMarginAccount :: Party -> Value -> Timeout -> Contract -> Contract -> Contract
    updateMarginAccount party value timeout liquidation continuation =
      If (ValueGT (AvailableMoney party ada) value)
        continuation
        (deposit party party (ada, value) timeout liquidation continuation)

toValueId :: String -> Timeout -> ValueId
toValueId label timeout = fromString $ label ++ "@" ++ show timeout

-- |Settlement of the Future contract
-- At delivery, if spot price is bigger than forward the seller transfers
-- the difference to the buyer and vice versa
settlement ::
     Party    -- ^ Buyer
  -> Party    -- ^ Seller
  -> Value    -- ^ Forward price
  -> Timeout  -- ^ Delivery date
  -> Contract -- ^ Continuation contract
  -> Contract -- ^ Composed contract
settlement buyer seller forwardPrice deliveryDate continuation =
  let invId = toValueId "inv-spot" deliveryDate
      dirId = toValueId "dir-spot" deliveryDate
      amount = DivValue
                (MulValue
                  (UseValue dirId)
                  (SubValue (UseValue invId) forwardPrice))
                (MulValue contractSize scale)
  in oracleInput dirRate deliveryDate Close
   $ oracleInput invRate deliveryDate Close
   $ Let dirId (ChoiceValue dirRate)
   $ Let invId (ChoiceValue invRate)
   $ If (ValueGE (UseValue invId) forwardPrice)
       (pay seller buyer (ada, amount) continuation)
       (pay buyer seller (ada, NegValue amount) continuation)

-- |Constants
scale, contractSize :: Value
scale        = Constant 1_000_000
contractSize = Constant 100
