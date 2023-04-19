-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Marlowe tests.
--
-----------------------------------------------------------------------------


{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}


module Spec.Marlowe.Marlowe
  ( -- * Testing
    prop_noFalsePositives
  , prop_showWorksForContracts
  , tests
  ) where


import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.Aeson (decode, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (first)
import Data.Either (isRight)
import Data.Maybe (isJust, isNothing)
import Data.SBV ()
import Data.String (IsString(fromString))
import Data.Text.Lazy (toStrict)
import GHC.IO (unsafePerformIO)
import Language.Haskell.Interpreter
  ( Extension(OverloadedStrings)
  , MonadInterpreter
  , OptionVal((:=))
  , as
  , interpret
  , languageExtensions
  , runInterpreter
  , set
  , setImports
  )
import Language.Marlowe.Analysis.FSSemantics (warningsTrace)
import Language.Marlowe.Core.V1.Semantics
  ( TransactionInput(TransactionInput, txInputs, txInterval)
  , TransactionOutput(TransactionOutput, txOutState)
  , computeTransaction
  , evalValue
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Choice, Deposit)
  , Bound(Bound)
  , Case(Case)
  , ChoiceId(ChoiceId)
  , Contract(Close, If, Pay, When)
  , Environment(Environment)
  , Observation(ValueGE)
  , Party(Role)
  , Payee(Account, Party)
  , State(State, accounts, boundValues, choices, minTime)
  , Token(Token)
  , Value(AddValue, Constant, DivValue, MulValue, NegValue, SubValue, UseValue)
  , ValueId(ValueId)
  , emptyState
  )
import Language.Marlowe.Core.V1.Semantics.Types.Address
  (deserialiseAddress, deserialiseAddressBech32, mainnet, serialiseAddress, serialiseAddressBech32)
import Language.Marlowe.Scripts (alternateMarloweValidator, marloweValidator)
import Language.Marlowe.Util (ada, extractNonMerkleizedContractRoles)
import Plutus.V2.Ledger.Api (POSIXTime(POSIXTime))
import Spec.Marlowe.Common (alicePk, amount, contractGen, pangramContract, shrinkContract, valueGen)
import Spec.Marlowe.Semantics.Arbitrary ()
import System.Timeout (timeout)
import Test.QuickCheck
  (arbitrary, counterexample, forAll, forAllShrink, property, suchThat, tabulate, (.&&.), (=/=), (===))
import Test.QuickCheck.Instances.ByteString ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@=?))
import Test.Tasty.QuickCheck (Property, testProperty)

import qualified Cardano.Api as C
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Marlowe as M
import qualified Ledger.Tx.CardanoAPI as C
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Prelude as P
import qualified PlutusTx.Ratio as P


-- | Set to `True` to print the JSON for the pangram contract.
_PRINT_PANGRAM_JSON_ :: Bool
_PRINT_PANGRAM_JSON_ = False


-- | Run the tests.
tests :: TestTree
tests = testGroup "Contracts"
  [ testCase "Token Show instance respects HEX and Unicode" tokenShowTest
  , testCase "Pangram Contract serializes into valid JSON" pangramContractSerialization
  , testCase "State serializes into valid JSON" stateSerialization
  , testCase "Input serializes into valid JSON" inputSerialization
  , testGroup "Validator size is reasonable"
      [ testCase "Typed validator size" alternateMarloweValidatorSize
      , testCase "Untyped validator size" marloweValidatorSize
      ]
  , testCase "Mul analysis" mulAnalysisTest
  , testCase "Div analysis" divAnalysisTest
  , testCase "Div tests" divTest
  , testCase "Transfers between accounts work" transferBetweenAccountsTest
  , testCase "extractContractRoles" extractContractRolesTest
  , testProperty "Value equality is reflexive, symmetric, and transitive" checkEqValue
  , testProperty "Value double negation" doubleNegation
  , testProperty "Values form abelian group" valuesFormAbelianGroup
  , testProperty "Values can be serialized to JSON" valueSerialization
  , testProperty "Multiply by zero" mulTest
  , testProperty "Divide zero and by zero" divZeroTest
  , testProperty "DivValue rounding" divisionRoundingTest
  , testGroup "Address serialisation"
    [
      testProperty "Compare to Cardano API serialisation to Bech32" addressSerialiseCardanoApi
    , testProperty "Serialise to bytes then deserialise" addressSerialiseDeserialiseBytes
    , testProperty "Serialise to Bech32 then deserialise" addressSerialiseDeserialiseBech32
    ]
  , testGroup "Party serialization"
    [
      testProperty "Serialise toJSON then deserialise" partySerialiseDeserialiseJSON
    ]
  ]


maxAlternateMarloweValidatorSize :: Int
maxMarloweValidatorSize :: Int
#ifdef TRACE_PLUTUS
maxAlternateMarloweValidatorSize = 15361
maxMarloweValidatorSize = 12840
#else
maxAlternateMarloweValidatorSize = 14821
maxMarloweValidatorSize = 12296
#endif


-- | Test that the typed validator is not too large.
alternateMarloweValidatorSize :: IO ()
alternateMarloweValidatorSize = do
    let validator = Scripts.validatorScript alternateMarloweValidator
    let vsize = SBS.length . SBS.toShort . LB.toStrict $ Serialise.serialise validator
    assertBool ("alternateMarloweValidator is too large " <> show vsize) (vsize <= maxAlternateMarloweValidatorSize)

-- | Test that the untyped validator is not too large.
marloweValidatorSize :: IO ()
marloweValidatorSize = do
    let validator = Scripts.validatorScript marloweValidator
    let vsize = SBS.length . SBS.toShort . LB.toStrict $ Serialise.serialise validator
    assertBool ("marloweValidator is too large " <> show vsize) (vsize <= maxMarloweValidatorSize)


-- | Test `extractNonMerkleizedContractRoles`.
extractContractRolesTest :: IO ()
extractContractRolesTest = do
    extractNonMerkleizedContractRoles Close @=? mempty
    extractNonMerkleizedContractRoles
        (Pay (Role "Alice") (Party (Role "Bob")) ada (Constant 1) Close)
            @=? Set.fromList ["Alice", "Bob"]
    extractNonMerkleizedContractRoles
        (When [Case (Deposit (Role "Bob") (Role "Alice") ada (Constant 10)) Close] 10 Close)
            @=? Set.fromList ["Alice", "Bob"]
    extractNonMerkleizedContractRoles
        (When [Case (Choice (ChoiceId "test" (Role "Alice")) [Bound 0 1]) Close] 10 Close)
            @=? Set.fromList ["Alice"]


-- | Test that equality on `Value` is symmetric and transitive.
checkEqValue :: Property
checkEqValue = property $ do
    let gen = do
            a <- valueGen
            b <- valueGen
            c <- valueGen
            return (a, b, c)
    forAll gen $ \(a, b, c) ->
        (a P.== a) -- reflective
            .&&. ((a P.== b) == (b P.== a)) -- symmetric
            --   (a == b && b == c) => (a == c)
            .&&. (not (a P.== b && b P.== c) || a P.== c) -- transitive


-- | Test that `NegValue` is its own inverse.
doubleNegation :: Property
doubleNegation = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll valueGen $ \a -> eval (NegValue (NegValue a)) === eval a


-- | Test that `Value` forms an Abelian group.
valuesFormAbelianGroup :: Property
valuesFormAbelianGroup = property $ do
    let gen = do
            a <- valueGen
            b <- valueGen
            c <- valueGen
            return (a, b, c)
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll gen $ \(a, b, c) ->
        -- associativity of addition
        eval (AddValue (AddValue a b) c) === eval (AddValue a (AddValue b c)) .&&.
        -- commutativity of addition
        eval (AddValue a b) === eval (AddValue b a) .&&.
        -- additive identity
        eval (AddValue a (Constant 0)) === eval a .&&.
        -- additive inverse
        eval (AddValue a (NegValue a)) === 0 .&&.
        -- substraction works
        eval (SubValue (AddValue a b) b) === eval a


-- | Test rounding of `DivValue`.
divisionRoundingTest :: Property
divisionRoundingTest = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    -- test half-even rounding
    let gen = do
            n <- amount
            d <- suchThat amount (/= 0)
            return (n, d)
    forAll gen $ \(n, d) -> eval (DivValue (Constant n) (Constant d)) === roundToZero (n M.% d)
    where
      roundToZero = P.truncate


-- | Test `DivValue` with zero in numerator or denominator.
divZeroTest :: Property
divZeroTest = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll valueGen $ \a ->
        eval (DivValue (Constant 0) a) === 0 .&&.
        eval (DivValue a (Constant 0)) === 0


-- | Test `MulValue` with a zero numerator.
mulTest :: Property
mulTest = property $ do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    forAll valueGen $ \a ->
        eval (MulValue (Constant 0) a) === 0


-- | Test the serialization of `Value`.
valueSerialization :: Property
valueSerialization = property $
    forAll valueGen $ \a ->
        let decoded :: Maybe (Value Observation)
            decoded = decode $ encode a
        in Just a === decoded


-- | Test a complicated sequence of multiplications.
mulAnalysisTest :: IO ()
mulAnalysisTest = do
    let muliply = foldl (\a _ -> MulValue (UseValue $ ValueId "a") a) (Constant 1) ([1..100] :: [Int])
        contract = If (muliply `M.ValueGE` Constant 10000) Close (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
    result <- warningsTrace contract
--  print result
    assertBool "Analysis ok" $ isRight result


-- | Test a complicated division.
divAnalysisTest :: IO ()
divAnalysisTest = do
    let
        contract n d = If (DivValue (Constant n) (Constant d) `ValueGE` Constant 5)
            Close
            (Pay alicePk (Party alicePk) ada (Constant (-100)) Close)
    result <- warningsTrace (contract 11 2)
    assertBool "Analysis ok" $ isRight result && either (const False) isNothing result
    result' <- warningsTrace (contract 9 2)
    assertBool "Analysis ok" $ isRight result' && either (const False) isJust result'

    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    eval (DivValue (Constant 0) (Constant 2)) @=? 0
    eval (DivValue (Constant 1) (Constant 0)) @=? 0
    eval (DivValue (Constant 5) (Constant 2)) @=? 2
    eval (DivValue (Constant (-5)) (Constant 2)) @=? -2
    eval (DivValue (Constant 7) (Constant 2)) @=? 3
    eval (DivValue (Constant (-7)) (Constant 2)) @=? -3


-- | Golden tests for `DivValue`.
divTest :: IO ()
divTest = do
    let eval = evalValue (Environment (POSIXTime 10, POSIXTime 1000)) (emptyState (POSIXTime 10))
    eval (DivValue (Constant 0) (Constant 2)) @=? 0
    eval (DivValue (Constant 1) (Constant 0)) @=? 0
    eval (DivValue (Constant 5) (Constant 2)) @=? 2
    eval (DivValue (Constant (-5)) (Constant 2)) @=? -2
    eval (DivValue (Constant 7) (Constant 2)) @=? 3
    eval (DivValue (Constant (-7)) (Constant 2)) @=? -3


-- | Test transfer of funds between internal accounts.
transferBetweenAccountsTest :: IO ()
transferBetweenAccountsTest = do
    let state = State
            { accounts = AssocMap.fromList [((Role "alice", Token "" ""), 100)]
            , choices  = AssocMap.empty
            , boundValues = AssocMap.empty
            , minTime = 10 }
    let contract = Pay "alice" (Account "bob") (Token "" "") (Constant 100) (When [] 100 Close)
    let txInput = TransactionInput {
                    txInterval = (20, 30),
                    txInputs = [] }
    case computeTransaction txInput state contract of
        TransactionOutput {txOutState = State{accounts}} -> do
            assertBool "Accounts check" $ accounts == AssocMap.fromList [(("bob",Token "" ""), 100)]
        e -> fail $ show e


-- | Serialize the Pangram contract.
pangramContractSerialization :: IO ()
pangramContractSerialization = do
    when _PRINT_PANGRAM_JSON_ $ do
      let json = toStrict (encodeToLazyText pangramContract)
      T.putStrLn json
      Just pangramContract @=? (decode $ encode pangramContract)
      T.putStrLn . T.pack . show $ pangramContract
    contract <- readFile "test/contract.json"
    let decoded :: Maybe M.Contract
        decoded = decode (fromString contract)
    case decoded of
        Just cont -> cont @=? pangramContract
        _         -> assertFailure "Nope"


-- | Test printing of tokens.
tokenShowTest :: IO ()
tokenShowTest = do
    -- SCP-834, CurrencySymbol is HEX encoded ByteString,
    -- and TokenSymbol as UTF8 encoded Unicode string
    let actual :: M.Value M.Observation
        actual = M.AvailableMoney (M.Role "alice") (M.Token "00010afF" "ÚSD©")

    show actual @=? "AvailableMoney \"alice\" (Token \"00010aff\" \"ÚSD©\")"


-- | Test JSON serialization of input.
inputSerialization :: IO ()
inputSerialization = do
    state <- readFile "test/input.json"
    let decoded :: Either String [M.Input]
        decoded = eitherDecode (fromString state)
    case decoded of
        Right input ->
            case eitherDecode $ encode input of
                Right input' -> assertBool "Should be equal" (input == input')
                Left e       -> assertFailure $ "Could not decode encoded input: " <> e
        Left e -> assertFailure $ "Could not decode test/input.json: " <> e


-- | Test JSON serialization of state.
stateSerialization :: IO ()
stateSerialization = do
    state <- readFile "test/state.json"
    let decoded :: Maybe M.State
        decoded = decode (fromString state)
    case decoded of
        Just st ->
            case decode $ encode st of
                Just st' -> assertBool "Should be equal" (st P.== st')
                Nothing  -> assertFailure "Nope"
        Nothing -> assertFailure "Nope"


-- | Test that showing contracts works, with shrinkage of test cases.
prop_showWorksForContracts :: Property
prop_showWorksForContracts = forAllShrink contractGen shrinkContract showWorksForContract


-- | Test that showing a contract works.
showWorksForContract :: Contract -> Property
showWorksForContract contract = unsafePerformIO $ do
  res <- runInterpreter $ setImports ["Language.Marlowe"]
                        >> set [ languageExtensions := [ OverloadedStrings ] ]
                        >> interpretContractString (show contract)
  return (case res of
            Right x  -> x === contract
            Left err -> counterexample (show err) False)


-- | Test reading of contracts.
interpretContractString :: MonadInterpreter m => String -> m Contract
interpretContractString contractStr = interpret contractStr (as :: Contract)


-- | Test that a contract execution does not exhibit false positives for warnings.
noFalsePositivesForContract :: Maybe Int -> Contract -> Property
noFalsePositivesForContract timeLimit cont =
  unsafePerformIO (do res <- catch (limitTime $ first Right <$> warningsTrace cont)
                                   (\exc -> return . Just . Left $ Left (exc :: SomeException))
                      return (case res of
                                Nothing -> tabulate ("Timed out after "
                                             <> show timeLimit
                                             <> " seconds") ["True"] True
                                Just (Left err) -> counterexample (show err) False
                                Just (Right answer) ->
                                   tabulate "Has counterexample" [show (isJust answer)]
                                   (case answer of
                                      Nothing ->
                                         tabulate "Is empty contract" [show (cont == Close)]
                                                  True
                                      Just (is, li, warns) ->
                                         counterexample ("Trace: " ++ show (is, li)) $
                                         tabulate "Number of warnings" [show (length warns)]
                                                  (warns =/= []))))
    where limitTime = maybe (Just <$>) timeout $ (1_000_000 *) <$> timeLimit


-- | Test that contract execution does not exhibit false positives for warnings.
prop_noFalsePositives :: Maybe Int -> Property
prop_noFalsePositives = forAllShrink contractGen shrinkContract . noFalsePositivesForContract


-- | Compare address serialisation to Cardano API.
addressSerialiseCardanoApi :: Property
addressSerialiseCardanoApi =
  property
  . forAll arbitrary
  $ \(network, address) ->
    let
      encoded = serialiseAddressBech32 network address
      encoded' = C.serialiseAddress <$> C.toCardanoAddressInEra (if network == mainnet then C.Mainnet else C.Testnet (C.NetworkMagic 2)) address
    in
      Right encoded === encoded'


-- | Serialise an address to bytes and then deserialize.
addressSerialiseDeserialiseBytes :: Property
addressSerialiseDeserialiseBytes =
  property
  . forAll arbitrary
  $ \(network, address) ->
    let
      encoded = serialiseAddress network address
      decoded = deserialiseAddress encoded
    in
      Just (network, address) === decoded


-- | Serialise an address to Bech32 and then deserialize.
addressSerialiseDeserialiseBech32 :: Property
addressSerialiseDeserialiseBech32 =
  property
  . forAll arbitrary
  $ \(network, address) ->
    let
      encoded = serialiseAddressBech32 network address
      decoded = deserialiseAddressBech32 encoded
    in
      Just (network, address) === decoded


-- | Serialise a party to JSON and then deserialise.
partySerialiseDeserialiseJSON :: Property
partySerialiseDeserialiseJSON =
  property
  . forAll arbitrary
  $ \party ->
    let
      encoded = encode (party :: Party)
      decoded = decode encoded
    in
      Just party === decoded
