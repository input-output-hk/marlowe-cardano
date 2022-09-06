{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
module Marlowe.Contracts.Valuation where

import Data.Number.Erf
import Data.Time.Clock.POSIX
import Language.Marlowe.Extended.V1

-- | Option premium given by the Black/Scholes model for a European Call Option
call ::
     Double -- ^ Price of Underlying
  -> Double -- ^ Time
  -> Double -- ^ Strike
  -> Double -- ^ Interest Rate
  -> Double -- ^ Volatility
  -> Double -- ^ Maturity
  -> Double -- ^ Option Premium
call s t k r σ m =
  let d1 = (log (s/k) + (r + 0.5*σ**2)*(m-t))/(σ*sqrt(m-t))
      d2 = d1 - σ*sqrt(m-t)
      n1 = 0.5*(1+ erf(d1/sqrt 2))
      n2 = 0.5*(1+ erf(d2/sqrt 2))
   in s*n1 - k*exp(-r*(m-t))*n2

-- | Option premium given by the Black/Scholes model for a European Put Option
put ::
     Double -- ^ Price of Underlying
  -> Double -- ^ Time
  -> Double -- ^ Strike
  -> Double -- ^ Interest Rate
  -> Double -- ^ Volatility
  -> Double -- ^ Maturity
  -> Double -- ^ Option Premium
put s t k r σ m =
  let d1 = (log (s/k) + (r + 0.5*σ**2)*(m-t))/(σ*sqrt(m-t))
      d2 = d1 - σ*sqrt(m-t)
      n1 = 0.5*(1+ erf(-d1/sqrt 2))
      n2 = 0.5*(1+ erf(-d2/sqrt 2))
   in k*exp(-r*(m-t))*n2 - s*n1

-- | Black/Scholes model for option pricing for European Call and Put Options
evalBS ::
     Timeout                          -- ^ Current time
  -> ((Token, Token) -> Maybe Double) -- ^ Price lookup function
  -> (Token -> Maybe Double)          -- ^ Volatility lookup function
  -> Double                           -- ^ Interest rate
  -> Contract                         -- ^ Contract to price
  -> Maybe Double                     -- ^ Price, if an analytical formula is available
evalBS -- European Call Option
  (POSIXTime t)
  price
  vola
  r
  ( When
      []
      (POSIXTime m)
      ( When
          [ Case
              (Choice (ChoiceId "Exercise Call" p0) [Bound 0 1])
              ( If
                  (ValueEQ (ChoiceValue (ChoiceId "Exercise Call" p1)) (Constant 0))
                  Close
                  ( When
                      [ Case
                          (Deposit p2 p3 t0 (Constant c0))
                          (Pay q0 (Party p4) u0 (Constant i) (Pay p5 (Party q1) t1 (Constant c1) Close))
                        ]
                      _
                      Close
                    )
                )
            ]
          _
          Close
        )
    ) | allEqual [p0, p1, p2, p3, p4, p5]
          && allEqual [q0, q1]
          && allEqual [c0, c1]
          && allEqual [t0, t1] =
    do
      p <- price (u0, t0)
      σ <- vola u0
      return $ fromInteger i * call p 0 (fromInteger $ c0 `div` i) r σ (yearFraction m t)
evalBS -- European Put Option
  (POSIXTime t)
  price
  vola
  r
  ( When
      []
      (POSIXTime m)
      ( When
          [ Case
              (Choice (ChoiceId "Exercise Put" p0) [Bound 0 1])
              ( If
                  (ValueEQ (ChoiceValue (ChoiceId "Exercise Put" p1)) (Constant 0))
                  Close
                  ( When
                      [ Case
                          (Deposit p2 p3 u0 (Constant c0))
                          (Pay q0 (Party p4) t0 (Constant i) (Pay p5 (Party q1) u1 (Constant c1) Close))
                        ]
                      _
                      Close
                    )
                )
            ]
          _
          Close
        )
    ) | allEqual [p0, p1, p2, p3, p4, p5]
          && allEqual [q0, q1]
          && allEqual [c0, c1]
          && allEqual [u0, u1] =
    do
      p <- price (u0, t0)
      σ <- vola u0
      return $ fromInteger c0 * put p 0 (fromInteger $ i `div` c0) r σ (yearFraction m t)
evalBS _ _ _ _ _ = Nothing

-- | Check, that all elements in a list are equal
allEqual :: (Eq a) => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

-- | Calculate the year fraction of a time period
yearFraction :: Integer -> Integer -> Double
yearFraction n m = fromInteger (n - m) / (365 * realToFrac posixDayLength)
