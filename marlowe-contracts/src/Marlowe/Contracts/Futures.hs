{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Futures (
  future,
) where

import Control.Monad (foldM)
import Data.Functor (void)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text (pack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Object.Bundler
import Language.Marlowe.Object.Types

-- | Future on the exchange rate of ADA/USD
--
--  A Future is an obligation for the parties involved in the
--  contract to exchange assets at maturity for the predefined
--  value.
--
--  The Future implemented here exchanges ADA for USD. The contract
--  is cash settled, i.e. USD is delivered in ADA resp. the difference
--  between the amount of the USD in ADA and the amount of ADA is due
--  at maturity.
--
--  The contract relies on /margin accounts/. The parties are required
--  to make payments into the margin account, in case the exchange
--  rate of ADA/USD changes considerably.
--
--  An oracle is used to get the exchange rate ADA/USD. As it is
--  implemented this currently works only in the Marlowe Playground.
future
  :: Party
  -- ^ Buyer
  -> Party
  -- ^ Seller
  -> Value
  -- ^ Forward price for 100 (contract size) USD at maturity (in Lovelace)
  -> Value
  -- ^ Initial margin requirements (in Lovelace)
  -> Timeout
  -- ^ Initial margin setup timeout
  -> [Timeout]
  -- ^ Margin call dates
  -> Timeout
  -- ^ Delivery date
  -> Bundler ()
  -- ^ Future contract
future buyer seller forwardPrice initialMargin initialFixing callDates deliveryDate = do
  invRateAction <- defineAction "invRate" $ Choice invRate [Bound 0 100_000_000_000]
  void . defineContract "initialMarginDeposit" . depositInitialMargin buyer seller initialMargin initialFixing
    =<< maintenanceMarginCalls invRateAction buyer seller forwardPrice callDates
    =<< defineContract "settlement" (settlement invRateAction buyer seller forwardPrice deliveryDate Close)

-- | Initial deposits into margin accounts
depositInitialMargin
  :: Party
  -- ^ Buyer
  -> Party
  -- ^ Seller
  -> Value
  -- ^ Forward price
  -> Timeout
  -- ^ Initial margin call
  -> Contract
  -- ^ Continuation contract
  -> Contract
  -- ^ Composed contract
depositInitialMargin buyer seller initialMargin initialFixing continuation =
  deposit buyer buyer (ada, initialMargin) initialFixing Close $
    deposit seller seller (ada, initialMargin) initialFixing Close continuation

-- | Maintenance of the margin accounts
maintenanceMarginCalls
  :: Action
  -- ^ InvRateAction
  -> Party
  -- ^ Buyer
  -> Party
  -- ^ Seller
  -> Value
  -- ^ Forward price
  -> [Timeout]
  -- ^ Call dates
  -> Contract
  -- ^ Continuation contract
  -> Bundler Contract
  -- ^ Composed contract
maintenanceMarginCalls invRateAction buyer seller forwardPrice callDates cont =
  foldM updateMarginAccounts cont $ sortOn Down callDates
  where
    updateMarginAccounts :: Contract -> Timeout -> Bundler Contract
    updateMarginAccounts continuation timeout = do
      let amount = SubValue scaledContractSize (DivValue (MulValue scaledContractSize forwardPrice) (ChoiceValue invRate))
          liquidation a b = pay a b (ada, AvailableMoney a ada) Close
          newLabel = Label $ "updateMargin-" <> pack (show $ utcTimeToPOSIXSeconds $ unTimeout timeout)
      defineContract newLabel $
        oracle invRateAction timeout $
          Let (ValueId "amount") amount $
            If
              (ValueGE (ChoiceValue invRate) forwardPrice)
              (updateMarginAccount seller (UseValue "amount") timeout (liquidation seller buyer) continuation)
              (updateMarginAccount buyer (NegValue (UseValue "amount")) timeout (liquidation buyer seller) continuation)

    updateMarginAccount :: Party -> Value -> Timeout -> Contract -> Contract -> Contract
    updateMarginAccount party value timeout liquidation continuation =
      If
        (ValueGT (AvailableMoney party ada) value)
        continuation
        (deposit party party (ada, value) timeout liquidation continuation)

-- | Settlement of the Future contract
--  At delivery, if spot price is bigger than forward the seller transfers
--  the difference to the buyer and vice versa
settlement
  :: Action
  -- ^ Inv rate action
  -> Party
  -- ^ Buyer
  -> Party
  -- ^ Seller
  -> Value
  -- ^ Forward price
  -> Timeout
  -- ^ Delivery date
  -> Contract
  -- ^ Continuation contract
  -> Contract
  -- ^ Composed contract
settlement invRateAction buyer seller forwardPrice deliveryDate continuation =
  let amount = SubValue scaledContractSize (DivValue (MulValue scaledContractSize forwardPrice) (ChoiceValue invRate))
   in oracle invRateAction deliveryDate $
        Let (ValueId "amount") amount $
          If
            (ValueGE (ChoiceValue invRate) forwardPrice)
            (pay seller buyer (ada, UseValue "amount") continuation)
            (pay buyer seller (ada, NegValue (UseValue "amount")) continuation)

-- | Constants
scaledContractSize :: Value
scaledContractSize = 100_000_000

-- | Exchange rates
invRate :: ChoiceId
invRate = ChoiceId "ADAUSD" (Role "oracle")

ada :: Token
ada = Token "" ""

oracle :: Action -> Timeout -> Contract -> Contract
oracle action timeout cont = When [Case action cont] timeout Close

-- | Pay
pay
  :: Party
  -- ^ Payer
  -> Party
  -- ^ Payee
  -> (Token, Value)
  -- ^ Token and Value
  -> Contract
  -- ^ Continuation Contract
  -> Contract
  -- ^ Combined Contract
pay from to (token, value) =
  Pay
    from
    (Party to)
    token
    value

-- | Deposit
deposit
  :: Party
  -- ^ Party to receive the deposit
  -> Party
  -- ^ Party that deposits
  -> (Token, Value)
  -- ^ Token and Value
  -> Timeout
  -- ^ Timeout for deposit
  -> Contract
  -- ^ Continuation Contract in case of timeout of deposit
  -> Contract
  -- ^ Continuation Contract after deposit
  -> Contract
  -- ^ Combined Contract
deposit to from (token, value) timeout timeoutContinuation continuation =
  When
    [ Case
        (Deposit to from token value)
        continuation
    ]
    timeout
    timeoutContinuation
