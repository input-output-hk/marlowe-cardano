{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Futures (
  future,
) where

import Data.List (mapAccumL, sortBy)
import Data.String (IsString (..))
import Data.Text (pack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Object.Types

-- | Future on the exchange rate of ADA/USD
--
--  A Future is an obligation for the parties involed in the
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
  -- ^ Initial margin setup timout
  -> [Timeout]
  -- ^ Margin call dates
  -> Timeout
  -- ^ Delivery date
  -> ObjectBundle
  -- ^ Future contract
future buyer seller forwardPrice initialMargin initialFixing callDates deliveryDate =
  let firstMarginUpdate = Label $ "updateMargin-" <> pack (show $ utcTimeToPOSIXSeconds $ unTimeout $ minimum callDates)
      initialMarginAccounts = depositInitialMargin buyer seller initialMargin initialFixing (ContractRef firstMarginUpdate)
      updateMarginAccounts = maintenanceMarginCalls buyer seller forwardPrice callDates "settlement"
      contractSettlement = settlement buyer seller forwardPrice deliveryDate Close
   in ObjectBundle $
        [ LabelledObject "dirRate" ActionType $ Choice dirRate [Bound 0 100_000_000_000]
        , LabelledObject "invRate" ActionType $ Choice invRate [Bound 0 100_000_000_000]
        , LabelledObject "settlement" ContractType contractSettlement
        ]
          <> updateMarginAccounts
          <> [LabelledObject "initialMarginDeposit" ContractType initialMarginAccounts]

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
depositInitialMargin buyer seller initalMargin initialFixing continuation =
  deposit buyer buyer (ada, initalMargin) initialFixing Close $
    deposit seller seller (ada, initalMargin) initialFixing Close continuation

-- | Maintenance of the margin accounts
maintenanceMarginCalls
  :: Party
  -- ^ Buyer
  -> Party
  -- ^ Seller
  -> Value
  -- ^ Forward price
  -> [Timeout]
  -- ^ Call dates
  -> Label
  -- ^ Continuation contract
  -> [LabelledObject]
  -- ^ Composed contract
maintenanceMarginCalls buyer seller forwardPrice callDates cont =
  snd $ mapAccumL updateMarginAccounts cont (sortBy (flip compare) callDates)
  where
    updateMarginAccounts :: Label -> Timeout -> (Label, LabelledObject)
    updateMarginAccounts contLabel timeout =
      let invId = toValueId "inv-spot" timeout
          dirId = toValueId "dir-spot" timeout
          amount =
            DivValue
              ( MulValue
                  (UseValue dirId)
                  (SubValue (UseValue invId) forwardPrice)
              )
              (MulValue contractSize scale)
          liquidation a b = pay a b (ada, AvailableMoney a ada) Close
          newLabel = Label $ "updateMargin-" <> pack (show $ utcTimeToPOSIXSeconds $ unTimeout timeout)
       in ( newLabel
          , LabelledObject newLabel ContractType $
              dirOracle timeout $
                invOracle timeout $
                  Let dirId (ChoiceValue dirRate) $
                    Let invId (ChoiceValue invRate) $
                      If
                        (ValueGE (UseValue invId) forwardPrice)
                        (updateMarginAccount seller amount timeout (liquidation seller buyer) (ContractRef contLabel))
                        (updateMarginAccount buyer (NegValue amount) timeout (liquidation buyer seller) (ContractRef contLabel))
          )

    updateMarginAccount :: Party -> Value -> Timeout -> Contract -> Contract -> Contract
    updateMarginAccount party value timeout liquidation continuation =
      If
        (ValueGT (AvailableMoney party ada) value)
        continuation
        (deposit party party (ada, value) timeout liquidation continuation)

toValueId :: String -> Timeout -> ValueId
toValueId name timeout = fromString $ name ++ "@" ++ show timeout

-- | Settlement of the Future contract
--  At delivery, if spot price is bigger than forward the seller transfers
--  the difference to the buyer and vice versa
settlement
  :: Party
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
settlement buyer seller forwardPrice deliveryDate continuation =
  let invId = toValueId "inv-spot" deliveryDate
      dirId = toValueId "dir-spot" deliveryDate
      amount =
        DivValue
          ( MulValue
              (UseValue dirId)
              (SubValue (UseValue invId) forwardPrice)
          )
          (MulValue contractSize scale)
   in dirOracle deliveryDate $
        invOracle deliveryDate $
          Let dirId (ChoiceValue dirRate) $
            Let invId (ChoiceValue invRate) $
              If
                (ValueGE (UseValue invId) forwardPrice)
                (pay seller buyer (ada, amount) continuation)
                (pay buyer seller (ada, NegValue amount) continuation)

-- | Constants
scale, contractSize :: Value
scale = Constant 1_000_000
contractSize = Constant 100

-- | Role for oracle
kraken :: Party
kraken = Role "kraken"

-- | Exchange rates
dirRate, invRate :: ChoiceId
dirRate = ChoiceId "dir-adausd" kraken -- USD/ADA
invRate = ChoiceId "inv-adausd" kraken -- ADA/USD

ada :: Token
ada = Token "" ""

dirOracle :: Timeout -> Contract -> Contract
dirOracle = When [Case dirRateAction Close]

invOracle :: Timeout -> Contract -> Contract
invOracle = When [Case invRateAction Close]

dirRateAction :: Action
dirRateAction = ActionRef "dirRate"

invRateAction :: Action
invRateAction = ActionRef "invRate"

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
