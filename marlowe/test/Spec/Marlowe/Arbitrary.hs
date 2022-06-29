
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Arbitrary (
  genAccounts
, genAssocMap
, arbitraryChoiceName
, shrinkChoiceName
, genFromAccounts
, arbitraryTimeInterval
, shrinkTimeInterval
) where


import Control.Monad (replicateM)
import Data.Function (on)
import Data.List (nubBy)
import Language.Marlowe.Semantics.Types (AccountId, Accounts, Bound (..), ChoiceId (..), ChoiceName, Environment (..),
                                         Party (..), State (..), TimeInterval, Token (..), ValueId (..))
import Plutus.V1.Ledger.Api (CurrencySymbol (..), POSIXTime (..), PubKeyHash (..), TokenName (..), adaSymbol, adaToken)
import PlutusTx.Builtins (BuiltinByteString, lengthOfByteString)
import Test.Tasty.QuickCheck (Arbitrary (..), Gen, chooseInt, elements, frequency, suchThat)

import qualified PlutusTx.AssocMap as AM (Map, fromList, null, toList)


fibonaccis :: Num a => [a]
fibonaccis = [2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584]


fibonacciFrequencies :: Integral a => [a]
fibonacciFrequencies = (1000000 `div`) <$> fibonaccis


arbitraryFibonacci :: [a] -> Gen a
arbitraryFibonacci = frequency . zip fibonacciFrequencies . fmap pure


shrinkByteString :: (a -> BuiltinByteString) -> [a] -> a -> [a]
shrinkByteString f universe selected =
  filter
    (\candidate -> lengthOfByteString (f candidate) > 0 && lengthOfByteString (f candidate) < lengthOfByteString (f selected))
    universe


randomPubKeyHashes :: [PubKeyHash]
randomPubKeyHashes =
  [
    "03e718291f87b2b004caac168d55e81da688e4501ce560ae613fa7e7"
  , "1b4a1ddd07976d3eee561fdce46d70f2f4c03985195906c08f547249"
  , "2f7bbc9ac7345b557515f6313cd66730241f2b0300c46b12f083ef46"
  , "40db357de24517df3f94cda9b7cc8078a0031d1e4a42cea9127cc730"
  , "592790c9e6af421fff4ffd7ad91de5aed0a703a03445beb8efb17fbf"
  , "6487e89309a827dc94d0e4a9b509c218fb405749d7d4f3fce3ea03f7"
  , "657e1b6b758226ad1c43d544f1d628daadf4eb4b6b411fbbc547ba7c"
  , "7b6fcf5a5528d3ee37493391fd4536a46f9aa63d41bdae6506ecf58d"
  , "8169906694a3558bd393fdd404a60b0ddb51a5e6c018698054f92f0a"
  , "85f24903d9a4f7a9b7284c0440882b1b9e0946ec51ee8cce40ad423b"
  , "9845463ec1285b4f6923133bf58517e4d90d1b1a71263f882ea6911d"
  , "a2bd7dd7f41c2781d1d11c7f4994fac750525705f9c259f97cb27d0e"
  , "c5b4c543a0d0d181ec387ad8250b18617bb18bcf2eccc0f27fe7aa23"
  , "d877b83ece77d785fee4a52bd7226949fa64e111aa0e20cd4a1c471b"
  , "e14025a93f867851b9bb3c48601d1845bcbe9e2e1856c16cfc052242"
  , "e3351d289f3eaa66e500f17b91a74e492193f4485c32e5ad606da835"
  ]

instance Arbitrary PubKeyHash where
  arbitrary = arbitraryFibonacci randomPubKeyHashes
  shrink x = filter (< x) randomPubKeyHashes


randomCurrencySymbols :: [CurrencySymbol]
randomCurrencySymbols =
  [
    "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8"
  , "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741"
  , "23d79373f7d9edbd016c99e21a473f498a2e425491244ecbc663e9d0"
  , "2839d40108e194eced45205c89613df56bd482e07e6c81a1df2b0e9b"
  , "2c60fb96c894b099f1a21ca9cf51c8c46a4672eb9a30b85252e9adb7"
  , "35c100db45fdf04b9317a2c520c2638ead47fd792984f32c9652cbc7"
  , "443d7002ac74be8c3c53f901d95c89c5932ee8946b188ca9f59db24e"
  , "63f3875b161780b82c7706fbc36fe906e54742e9f5b4c68d260e5da9"
  , "64da8cbb98eccc616bb0061efed2717393e4b48d8f78147396f4521f"
  , "66879477b60f46e5c5ad1d1bb124ab5c3d46a3acc9e54b7da4259655"
  , "9019bb7fb44ec03537b61a6f4aa3fd7b1effaf0776c3d449e9c6274e"
  , "9f92753881b398a247e53b6cad08eab0e158cf1ef5df84c7f5766041"
  , "c1f46ec0147542f9bc155805993497ed44150687a41d0a63af3be466"
  , "cc2189d7adde0ed26355fd03e134feb508e5924959b07a53557f285e"
  , "df97bf95b2d21327731329d94173344ff4db5ac16f92250d9cab00a0"
  , "ead659651c55f5481dbc7038a7c096fd7616d2f86471bd9d46de742e"
 ]

instance Arbitrary CurrencySymbol where
  arbitrary = arbitraryFibonacci randomCurrencySymbols
  shrink x = filter (< x) randomCurrencySymbols


randomTokenNames :: [TokenName]
randomTokenNames =
  [
    "I"
  , "AD"
  , "PIN"
  , "TALE"
  , "RIVER"
  , "METHOD"
  , "REVENUE"
  , ""
  , "POSSIBILITY"
  , "SATISFACTION"
  , "RELATIONSHIPS"
  , "PAYMENT CONCEPT"
  , "OFFICE DEFINITION"
  , "ARTISAN CONVERSATION"
  , "SOFTWARE FEEDBACK METHOD"
  , "INDEPENDENCE EXPLANATION REVENUE"
  ]

instance Arbitrary TokenName where
  arbitrary = arbitraryFibonacci randomTokenNames
  shrink = shrinkByteString (\(TokenName x) -> x) randomTokenNames


instance Arbitrary Token where
  arbitrary =
     do
       isAda <- arbitrary
       if isAda
         then pure $ Token adaSymbol adaToken
         else Token <$> arbitrary <*> arbitrary
  shrink (Token c n)
    | c == adaSymbol && n == adaToken = []
    | otherwise                       = Token adaSymbol adaToken : [Token c' n' | c' <- shrink c, n' <- shrink n]


randomRoleNames :: [TokenName]
randomRoleNames =
  [
    "Cy"
  , "Noe"
  , "Sten"
  , "Cara"
  , "Alene"
  , "Hande"
  , ""
  , "I"
  , "Zakkai"
  , "Laurent"
  , "Alcippe"
  , "Prosenjit"
  , "Dafne Helge Mose"
  , "Nonso Ernie Blanka"
  , "Umukoro Alexander Columb"
  , "Urbanus Roland Alison Ty Ryoichi"
  ]

instance Arbitrary Party where
  arbitrary =
    do
       isPubKeyHash <- frequency [(2, pure True), (8, pure False)]
       if isPubKeyHash
         then PK <$> arbitrary
         else Role <$> arbitraryFibonacci randomRoleNames
  shrink (PK x)   = (Role <$> randomRoleNames) <> (PK <$> filter (< x) randomPubKeyHashes)
  shrink (Role x) = Role <$> shrinkByteString (\(TokenName y) -> y) randomRoleNames x


randomChoiceNames :: [ChoiceName]
randomChoiceNames =
  [
    "be"
  , "dry"
  , "grab"
  , "weigh"
  , "enable"
  , "enhance"
  , "allocate"
  , ""
  , "originate"
  , "understand"
  , "characterize"
  , "derive witness"
  , "envisage software"
  , "attend unknown animals"
  , "position increated radiation"
  , "proclaim endless sordid figments"
  ]

arbitraryChoiceName :: Gen ChoiceName
arbitraryChoiceName = arbitraryFibonacci randomChoiceNames

shrinkChoiceName :: ChoiceName -> [ChoiceName]
shrinkChoiceName = shrinkByteString id randomChoiceNames


instance Arbitrary ChoiceId where
  arbitrary = ChoiceId <$> arbitraryChoiceName <*> arbitrary
  shrink (ChoiceId n p) = [ChoiceId n' p' | n' <- shrinkChoiceName n, p' <- shrink p]


randomValueIds :: [ValueId]
randomValueIds =
  [
    "x"
  , "id"
  , "lab"
  , "idea"
  , "story"
  , "memory"
  , "fishing"
  , ""
  , "drawing"
  , "reaction"
  , "candidate"
  , "difference"
  , "replacement"
  , "paper apartment"
  , "leadership information"
  , "entertainment region assumptions"
  ]

instance Arbitrary ValueId where
  arbitrary = arbitraryFibonacci randomValueIds
  shrink = shrinkByteString (\(ValueId x) -> x) randomValueIds


instance Arbitrary POSIXTime where
  arbitrary = POSIXTime <$> arbitrary
  shrink x = filter (< x) fibonaccis


arbitraryTimeInterval :: Gen TimeInterval
arbitraryTimeInterval =
  do
    start <- arbitrary
    end <- arbitrary `suchThat` (> start)
    pure (start, end)

shrinkTimeInterval :: TimeInterval -> [TimeInterval]
shrinkTimeInterval (start, end) =
  let
    mid = (start + end + 1) `div` 2
  in
    [
      (start  , start + 1)
    , (start  , mid      )
    , (mid - 1, mid      )
    , (mid - 1, end      )
    , (end - 1, end      )
    ]


instance Arbitrary Bound where
  arbitrary =
    do
      lower <- arbitrary
      upper <- arbitrary `suchThat` (>= lower)
      pure $ Bound lower upper
  shrink (Bound lower upper) =
    let
      mid = (lower + upper) `div` 2
    in
      [
        Bound lower lower
      , Bound lower mid
      , Bound mid   mid
      , Bound mid   upper
      , Bound upper upper
      ]


genAccounts :: Gen Accounts
genAccounts =
  do  -- FIXME: Add correlations.
    accounts' <- replicateM 10 arbitrary
    tokens <- replicateM 10 $ suchThat arbitrary (> 0)
    entries <- chooseInt (0, 10)
    fmap (AM.fromList . nubBy ((==) `on` fst))
      . replicateM entries
      $ (,) <$> elements accounts' <*> elements tokens

genFromAccounts :: Accounts -> Gen ((AccountId, Token), Integer)
genFromAccounts accounts'
  | AM.null accounts' = (,) <$> ((,) <$> arbitrary <*> arbitrary) <*> arbitrary
  | otherwise =
    do
      let entries = AM.toList accounts'
      exact <- arbitrary
      exactKey <- arbitrary
      exactAccountId <- arbitrary
      exactToken <- arbitrary
      exactAmount <- arbitrary
      let chooseKey = elements $ fst <$> entries
          chooseAccountId = if exactAccountId then elements $ fst . fst <$> entries else arbitrary
          chooseToken = if exactToken then elements $ snd . fst <$> entries else arbitrary
          chooseAmount = if exactAmount then elements $ snd <$> entries else arbitrary
      case (exact, exactKey) of
        (True , _    ) -> elements entries
        (False, True ) -> (,) <$> chooseKey <*> chooseAmount
        (False, False) -> (,) <$> ((,) <$> chooseAccountId <*> chooseToken) <*> chooseAmount

genAssocMap :: Eq k
            => Arbitrary k
            => Arbitrary v
            => Gen (AM.Map k v)
genAssocMap =
  do
    entries <- chooseInt (0, 10)
    fmap (AM.fromList . nubBy ((==) `on` fst))
      . replicateM entries
      $ (,) <$> arbitrary <*> arbitrary


instance Arbitrary State where
  arbitrary = State <$> genAccounts <*> genAssocMap <*> genAssocMap <*> arbitrary


instance Arbitrary Environment where
  arbitrary = Environment <$> arbitraryTimeInterval
