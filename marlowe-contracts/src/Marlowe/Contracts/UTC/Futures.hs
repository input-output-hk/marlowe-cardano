module Marlowe.Contracts.UTC.Futures
  where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Futures as C
import Marlowe.Contracts.UTC.Common

future ::
     Party     -- ^ Buyer
  -> Party     -- ^ Seller
  -> Value     -- ^ Forward price for 100 (contract size) USD at maturity (in Lovelace)
  -> Value     -- ^ Initial margin requirements (in Lovelace)
  -> UTCTime   -- ^ Initial margin setup timout
  -> [UTCTime] -- ^ Margin call dates
  -> UTCTime   -- ^ Delivery date
  -> Contract  -- ^ Future contract
future buyer seller forwardPrice initialMargin initialFixing callDates deliveryDate =
  C.future buyer seller forwardPrice initialMargin (toTimeout initialFixing) (map toTimeout callDates) (toTimeout deliveryDate)
