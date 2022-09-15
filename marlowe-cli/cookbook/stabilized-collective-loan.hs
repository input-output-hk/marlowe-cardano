{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson (encodeFile, object, (.=))
import Data.Default (Default(..))
import Data.Foldable (foldrM)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Language.Marlowe.CLI.Merkle (deepMerkleize)
import Language.Marlowe.CLI.Types (Continuations)
import Language.Marlowe.Core.V1.Semantics.Types
import Plutus.V1.Ledger.Api (BuiltinByteString, POSIXTime(..), TokenName)


data Scenario =
  Scenario
  {
    nSteps               :: Int
  , timeoutStart         :: Integer
  , timeoutDelta         :: Integer
  , timeoutFinal         :: POSIXTime
  , nStabilizers         :: Int
  , nInvestors           :: Int
  , nBorrowers           :: Int
  , administratorName    :: String
  , stabilizerPrefix     :: String
  , investorPrefix       :: String
  , borrowerPrefix       :: String
  , stabilizeBound       :: [Bound]
  , investBound          :: [Bound]
  , loanBound            :: [Bound]
  , percentInterest      :: Integer
  , percentStabilization :: Integer
  , djed                 :: Token
  }

instance Default Scenario where
  def =
    Scenario
    {
      nSteps               = 1
    , timeoutStart         = 0
    , timeoutDelta         = 3_600_000
    , timeoutFinal         = POSIXTime 1_000_000_000_0000_000
    , nStabilizers         = 1
    , nInvestors           = 1
    , nBorrowers           = 1
    , administratorName    = "A"
    , stabilizerPrefix     = "S"
    , investorPrefix       = "I"
    , borrowerPrefix       = "B"
    , stabilizeBound       = [Bound 500_000 5_000_000]
    , investBound          = [Bound 200_000 2_000_000]
    , loanBound            = [Bound 100_000 1_000_000]
    , percentInterest      = 10
    , percentStabilization = 40
    , djed                 = Token "00" "$DJED_NAME"
    }


main :: IO ()
main =
  let
    (contract, continuations) = buildContract def
  in
    encodeFile "$BASENAME.merkleization"
      $ object
        [
          "contract"      .= contract
        , "continuations" .= continuations
        ]


buildContract :: Scenario -> (Contract, Continuations)
buildContract scenario@Scenario{..} =
  let
    timeouts = [POSIXTime $ timeoutStart + toInteger i * timeoutDelta | i <- [1..nSteps]]
  in
    runWriter $ foldrM (makeStep scenario) Close timeouts


defaultBound :: [Bound]
defaultBound = [Bound 1 1_000_000_000_000]


makeParty :: String -> Int -> Party
makeParty name i = Role . fromString $ name <> show i


deposit :: Scenario
        -> String
        -> String
        -> Int
        -> Maybe Party
        -> [Bound]
        -> (Value Observation -> Contract -> Contract)
        -> Contract
        -> [Case Contract]
deposit Scenario{..} name prefix i recipient bound f continuation =
  let
    party = makeParty prefix i
    party' = fromMaybe party recipient
    choice = ChoiceId (fromString name) party
    value = ChoiceValue choice
  in
    [
      Case (Choice choice bound)
        continuation
    , Case (Deposit party' party djed value)
        $ f value continuation
    ]


stabilization :: ValueId
stabilization = ValueId "Stabilization"


stabilize :: Scenario -> Int -> Contract -> [Case Contract]
stabilize scenario@Scenario{..} i =
  deposit scenario "Stabilize" stabilizerPrefix i Nothing stabilizeBound
    (stabilization =+)


assets :: ValueId
assets = ValueId "Assets"


equity :: Int -> ValueId
equity = ValueId . ("Equity " <>) . fromString . show


invest :: Scenario -> Int -> Contract -> [Case Contract]
invest scenario@Scenario{..} i =
  deposit scenario "Invest" investorPrefix i Nothing investBound
    $ \value -> (assets   =+ value)
              . (equity i =+ value)


withdraw :: Scenario
         -> Int
         -> Contract
         -> [Case Contract]
withdraw Scenario{..} i continuation =
  let
    party = makeParty investorPrefix i
    choice = ChoiceId "Withdraw" party
    value = ChoiceValue choice
    funds = AvailableMoney party djed
  in
    [
      Case (Choice choice defaultBound)
        $ If (value @<= funds)
            (
              Pay party (Party party) djed value
                $ assets   =- value
                $ equity i =- value
                $ continuation
            )
            (
              Pay party (Party party) djed funds
                $ assets   =- funds
                $ equity i =- funds
                $ continuation
            )
    ]


lend :: Scenario
     -> Party
     -> Value Observation
     -> Value Observation
     -> Int
     -> Contract
     -> Contract
lend Scenario{..} borrower principal interest i =
  let
    party = makeParty investorPrefix i
    funds = AvailableMoney party djed
    assets' = UseValue assets
    principal' = (principal @* funds) @/ assets'
    interest'  = (interest  @* funds) @/ assets'
  in
    Pay party (Party borrower) djed principal'
      . (equity i =+ interest')


liabilities :: ValueId
liabilities = ValueId "Liabilities"

loan :: Scenario
     -> Int
     -> Contract
     -> [Case Contract]
loan scenario@Scenario{..} i continuation =
  let
    party = Role $ fromString administratorName
    borrower = makeParty borrowerPrefix i
    choice = ChoiceId "Loan" party
    principal = ChoiceValue choice
    assets' = UseValue assets
    stabilization' = UseValue stabilization
  in
    [
      Case (Choice choice loanBound)
        $ let
            interest = principal @% percentInterest
            debt = principal @+ interest
          in
            If ((principal @<= assets') @&& (stabilization' @>= (debt @% percentStabilization)))
              (
                flip (foldr $ lend scenario borrower principal interest) [1..nInvestors]
                  $ liabilities =+ debt
                  $ assets      =- principal
                  $ continuation
              )
              continuation
    ]


restore :: Scenario
        -> Value Observation
        -> Int
        -> Contract
        -> Contract
restore Scenario{..} repayment i =
  let
    administrator = Role $ fromString administratorName
    party = makeParty investorPrefix i
    equity' = UseValue $ equity i
    assets' = AvailableMoney party djed
    liabilities' = UseValue liabilities
    share = (repayment @* (equity' @- assets')) @/ liabilities'
  in
    Pay administrator (Account party) djed share


repay :: Scenario
      -> Int
      -> Contract
      -> [Case Contract]
repay scenario@Scenario{..} i =
  deposit scenario "Repay" borrowerPrefix i (Just . Role $ fromString administratorName) defaultBound
    $ \repayment -> flip (foldr $ restore scenario repayment) [1..nInvestors]
                      . (liabilities =- repayment)
                      . (assets      =+ repayment)


gather :: Scenario
       -> Value Observation
       -> ValueId
       -> String
       -> Int
       -> Contract
       -> Contract
gather Scenario{..} amount basis prefix i =
  let
    administrator = Role $ fromString administratorName
    party = makeParty prefix i
    funds = AvailableMoney party djed
    basis' = UseValue basis
    share = (amount @* funds) @/ basis'
  in
    Pay party (Account administrator) djed share


scatter :: Scenario
        -> Value Observation
        -> ValueId
        -> String
        -> Int
        -> Contract
        -> Contract
scatter Scenario{..} amount basis prefix i =
  let
    administrator = Role $ fromString administratorName
    party = makeParty prefix i
    funds = AvailableMoney party djed
    basis' = UseValue basis
    share = (amount @* funds) @/ basis'
  in
    Pay administrator (Account party) djed share


terminate :: Scenario
          -> [Case Contract]
terminate scenario@Scenario{..} =
  let
    party = Role $ fromString administratorName
    choice = ChoiceId "Terminate" party
    liabilities' = UseValue liabilities
  in
    [
      Case (Choice choice [Bound 1 1])
        $ If (liabilities' @>= Constant 0)
          (
              flip (foldr $ gather  scenario liabilities' stabilization stabilizerPrefix) [1..nStabilizers]
            $ flip (foldr $ scatter scenario liabilities' assets        investorPrefix  ) [1..nInvestors  ]
              Close
          )
          (
              flip (foldr $ gather  scenario (NegValue liabilities') assets        investorPrefix  ) [1..nInvestors  ]
            $ flip (foldr $ scatter scenario (NegValue liabilities') stabilization stabilizerPrefix) [1..nStabilizers]
              Close
          )
    ]


makeStep :: Scenario
         -> Timeout
         -> Contract
         -> Writer Continuations Contract
makeStep scenario@Scenario{..} timeout continuation =
  let
  in
    deepMerkleize
      . When
        (
          concat
            $  [stabilize scenario i continuation | i <- [1..nStabilizers]]
            <> [invest    scenario i continuation | i <- [1..nInvestors]  ]
            <> [withdraw  scenario i continuation | i <- [1..nInvestors]  ]
            <> [loan      scenario i continuation | i <- [1..nBorrowers]  ]
            <> [repay     scenario i continuation | i <- [1..nBorrowers]  ]
            <> [terminate scenario                                        ]
        )
      timeout
    =<< merkleizeTimeout timeoutFinal continuation


merkleizeTimeout :: Timeout
                 -> Contract
                 -> Writer Continuations Contract
merkleizeTimeout timeout continuation =
  deepMerkleize
    $ When
      [
        Case (Notify TrueObs )
        continuation
      ]
      timeout
      Close


(=+) :: ValueId -> Value Observation -> (Contract -> Contract)
variable =+ value = Let variable $ UseValue variable @+ value

(=-) :: ValueId -> Value Observation -> (Contract -> Contract)
variable =- value = Let variable $ UseValue variable @- value

(=:) :: ValueId -> Integer -> (Contract -> Contract)
variable =: value = Let variable $ Constant value

(@%) :: Value Observation -> Integer -> Value Observation
value @% rate = DivValue (MulValue value $ Constant rate) (Constant 100)

(@+%) :: Value Observation -> Integer -> Value Observation
value @+% rate = DivValue (MulValue value (AddValue (Constant rate) $ Constant 0)) (Constant 100)

(@+) :: Value Observation -> Value Observation -> Value Observation
x @+ y = AddValue x y

(@-) :: Value Observation -> Value Observation -> Value Observation
x @- y = SubValue x y

(@*) :: Value Observation -> Value Observation -> Value Observation
x @* y = MulValue x y

(@/) :: Value Observation -> Value Observation -> Value Observation
x @/ y = DivValue x y

(@<) :: Value Observation -> Value Observation -> Observation
x @< y = ValueLT x y

(@<=) :: Value Observation -> Value Observation -> Observation
x @<= y = ValueLE x y

(@>) :: Value Observation -> Value Observation -> Observation
x @> y = ValueGT x y

(@>=) :: Value Observation -> Value Observation -> Observation
x @>= y = ValueGE x y

(@==) :: Value Observation -> Value Observation -> Observation
x @== y = ValueEQ x y

(@&&) :: Observation -> Observation -> Observation
x @&& y = AndObs x y

(@||) :: Observation -> Observation -> Observation
x @|| y = OrObs x y
